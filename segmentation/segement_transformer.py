#!/usr/bin/env python
# coding: utf-8
"""
Soybean Semantic Segmentation — SegFormer-B0
============================================
Sections:
  1. Configuration
  2. Dataset & DataLoader
  3. Training
  4. Inference (sliding-window prediction)
  5. Batch prediction across directories
"""

# ─────────────────────────────────────────────
# 1. CONFIGURATION
# ─────────────────────────────────────────────
import os
from pathlib import Path

# Paths — edit these before running
TRAIN_IMAGE_DIR = "/Soybean/Train/RGB/"
TRAIN_MASK_DIR  = "/Soybean/Train/Mask/"
MODEL_SAVE_PATH = "model_soybean_segmentation.pth"

# Prediction directories (relative to BASE_PRED_PATH)
BASE_PRED_PATH = Path.home() / "Soybean/"
PRED_DIRS = [
    ("2022/SB016/RGB1/Segmentation/", slice(2, 10)),
    ("2021/SB015/RGB1/Segmentation/", slice(1,  9)),
    ("2020/SB014/RGB1/Segmentation/", slice(3, 10)),
]

# Model / training hyperparameters
NUM_CLASSES     = 3       # 0 = background, 1 = class1, 2 = class2
NUM_EPOCHS      = 24
BATCH_SIZE      = 16
LEARNING_RATE   = 5e-5
PATIENCE        = 5       # early-stopping patience

# Sliding-window inference settings
PATCH_SIZE      = 2048
OVERLAP         = 128
PRETRAINED_ID   = "nvidia/segformer-b0-finetuned-ade-512-512"

# ─────────────────────────────────────────────
# 2. DATASET & DATALOADER
# ─────────────────────────────────────────────
import glob
import numpy as np
import torch
from torch.utils.data import Dataset, DataLoader
from transformers import SegformerFeatureExtractor
from PIL import Image
from sklearn.model_selection import train_test_split


def load_image_mask_paths(image_dir, mask_dir, exclude_keyword="collage4"):
    """Return sorted, filtered lists of image/mask paths."""
    image_paths = sorted([
        p for p in glob.glob(os.path.join(image_dir, "*.png"))
        if exclude_keyword not in os.path.basename(p)
    ])
    mask_paths = sorted([
        p for p in glob.glob(os.path.join(mask_dir, "*.png"))
        if exclude_keyword not in os.path.basename(p)
    ])
    print(f"Found {len(image_paths)} images and {len(mask_paths)} masks.")
    return image_paths, mask_paths


class SegmentationDataset(Dataset):
    def __init__(self, image_paths, mask_paths, feature_extractor):
        self.image_paths = image_paths
        self.mask_paths  = mask_paths
        self.feature_extractor = feature_extractor

    def __len__(self):
        return len(self.image_paths)

    def __getitem__(self, idx):
        image = Image.open(self.image_paths[idx]).convert("RGB")
        mask  = Image.open(self.mask_paths[idx]).convert("L")

        pixel_values = self.feature_extractor(
            images=image, return_tensors="pt"
        )["pixel_values"].squeeze(0)                         # [3, H, W]
        mask = torch.tensor(np.array(mask), dtype=torch.long)  # [H, W]
        return pixel_values, mask


# ─────────────────────────────────────────────
# 3. TRAINING
# ─────────────────────────────────────────────
import matplotlib.pyplot as plt
import torch.nn as nn
from transformers import SegformerForSemanticSegmentation


def build_model(num_classes, pretrained_id=PRETRAINED_ID):
    model = SegformerForSemanticSegmentation.from_pretrained(
        pretrained_id,
        ignore_mismatched_sizes=True,
        num_labels=num_classes,
    )
    return model


def train(model, train_loader, val_loader, num_epochs, lr, patience, save_path):
    """
    Train with validation-based early stopping.
    Saves the best checkpoint to `save_path`.
    Returns (train_losses, val_losses).
    """
    device = next(model.parameters()).device
    optimizer = torch.optim.AdamW(model.parameters(), lr=lr)

    train_losses, val_losses = [], []
    best_val_loss = float("inf")
    patience_counter = 0

    for epoch in range(num_epochs):
        # ── Train ──
        model.train()
        train_loss = 0.0
        for pixel_values, masks in train_loader:
            pixel_values, masks = pixel_values.to(device), masks.to(device)
            loss = model(pixel_values=pixel_values, labels=masks).loss
            loss.backward()
            optimizer.step()
            optimizer.zero_grad()
            train_loss += loss.item()

        avg_train = train_loss / len(train_loader)
        train_losses.append(avg_train)

        # ── Validate ──
        model.eval()
        val_loss = 0.0
        with torch.no_grad():
            for pixel_values, masks in val_loader:
                pixel_values, masks = pixel_values.to(device), masks.to(device)
                val_loss += model(pixel_values=pixel_values, labels=masks).loss.item()

        avg_val = val_loss / len(val_loader)
        val_losses.append(avg_val)

        print(f"Epoch {epoch:>3}: Train={avg_train:.4f}  Val={avg_val:.4f}")

        # ── Checkpoint & early stopping ──
        if avg_val < best_val_loss:
            best_val_loss = avg_val
            torch.save(model.state_dict(), save_path)
            patience_counter = 0
            print(f"  ✅ Saved best model (val={avg_val:.4f})")
        else:
            patience_counter += 1
            if patience_counter >= patience:
                print("  🛑 Early stopping triggered.")
                break

    return train_losses, val_losses


def plot_losses(train_losses, val_losses):
    plt.figure(figsize=(8, 5))
    plt.plot(train_losses, label="Train Loss", marker="o")
    plt.plot(val_losses,   label="Val Loss",   marker="x")
    plt.title("Training vs Validation Loss")
    plt.xlabel("Epoch")
    plt.ylabel("Loss")
    plt.legend()
    plt.grid(True)
    plt.tight_layout()
    plt.show()


# ─────────────────────────────────────────────
# 4. INFERENCE  (sliding-window)
# ─────────────────────────────────────────────
import torch.nn.functional as F
from tqdm import tqdm


def load_model_for_inference(weights_path, num_classes, pretrained_id=PRETRAINED_ID, device=None):
    """Load a trained SegFormer model from a .pth file."""
    if device is None:
        device = torch.device("cuda" if torch.cuda.is_available() else "cpu")

    from transformers import SegformerConfig
    config = SegformerConfig.from_pretrained(pretrained_id)
    config.num_labels = num_classes

    model = SegformerForSemanticSegmentation(config)
    model.load_state_dict(torch.load(weights_path, map_location=device))
    model.to(device)
    model.eval()
    print(f"Loaded weights from '{weights_path}' onto {device}.")
    return model, device


def build_inference_extractor(pretrained_id=PRETRAINED_ID):
    """Feature extractor with resizing disabled (needed for large images)."""
    fe = SegformerFeatureExtractor.from_pretrained(pretrained_id)
    fe.do_resize = False
    fe.size = None
    return fe


def predict_image(image, model, feature_extractor, patch_size, overlap, device):
    """Sliding-window segmentation of a single PIL image. Returns uint8 mask."""
    width, height = image.size
    stride = patch_size - overlap
    final_mask = np.zeros((height, width), dtype=np.uint8)

    for top in range(0, height, stride):
        for left in range(0, width, stride):
            bottom = min(top + patch_size, height)
            right  = min(left + patch_size, width)

            crop = image.crop((left, top, right, bottom))
            padded = Image.new("RGB", (patch_size, patch_size))
            padded.paste(crop, (0, 0))

            inputs = feature_extractor(images=padded, return_tensors="pt")
            inputs = {k: v.to(device) for k, v in inputs.items()}

            with torch.no_grad():
                logits = model(**inputs).logits
                upsampled = F.interpolate(
                    logits, size=(patch_size, patch_size),
                    mode="bilinear", align_corners=False,
                )
                pred = torch.argmax(upsampled, dim=1).squeeze().cpu().numpy()

            h_cut, w_cut = bottom - top, right - left
            final_mask[top:bottom, left:right] = pred[:h_cut, :w_cut]

    return final_mask


# ─────────────────────────────────────────────
# 5. BATCH PREDICTION
# ─────────────────────────────────────────────

def predict_directory(input_dir, model, feature_extractor,
                      patch_size, overlap, device, glob_pattern="*_q90.jpg"):
    """
    Run sliding-window prediction on all matching images in `input_dir`.
    Saves each mask as  <original_name>_mask.png  alongside the source image.
    """
    image_files = glob.glob(os.path.join(input_dir, "**", glob_pattern), recursive=True)
    image_files = [p for p in image_files if "_mask" not in os.path.basename(p)]
    print(f"  Found {len(image_files)} images in {input_dir}")

    model.eval()
    for img_path in tqdm(image_files, desc=f"  Predicting {os.path.basename(input_dir)}"):
        image = Image.open(img_path).convert("RGB")
        mask  = predict_image(image, model, feature_extractor, patch_size, overlap, device)

        # Scale class indices for visibility (0→0, 1→100, 2→200)
        debug_mask  = (mask * 100).clip(0, 255).astype(np.uint8)
        base, _     = os.path.splitext(img_path)
        base        = base.replace("_q90", "")
        mask_path   = f"{base}_mask.png"
        Image.fromarray(debug_mask).save(mask_path)
        print(f"    ✅ Saved: {mask_path}")


def predict_dirs(dir_spec_list, base_path, model, feature_extractor,
                 patch_size, overlap, device):
    """
    Run batch prediction over a list of (subdir, slice) tuples.

    Parameters
    ----------
    dir_spec_list : list of (str, slice)
        Each entry is a (relative_subdir, slice_of_sorted_subdirs) pair.
    base_path : str
        Root path that dir_spec_list entries are relative to.
    """
    for rel_subdir, idx_slice in dir_spec_list:
        full_path = os.path.join(base_path, rel_subdir)
        all_subdirs = sorted([
            d for d in os.listdir(full_path)
            if os.path.isdir(os.path.join(full_path, d))
        ])
        selected = all_subdirs[idx_slice]
        print(f"\n📂 {full_path}  →  {selected}")
        for subdir in selected:
            predict_directory(
                input_dir=os.path.join(full_path, subdir),
                model=model,
                feature_extractor=feature_extractor,
                patch_size=patch_size,
                overlap=overlap,
                device=device,
            )


# ─────────────────────────────────────────────
# MAIN
# ─────────────────────────────────────────────
if __name__ == "__main__":

    # ── TRAINING ──────────────────────────────
    feature_extractor = SegformerFeatureExtractor.from_pretrained(PRETRAINED_ID)

    image_paths, mask_paths = load_image_mask_paths(TRAIN_IMAGE_DIR, TRAIN_MASK_DIR)

    train_imgs, val_imgs, train_masks, val_masks = train_test_split(
        image_paths, mask_paths, test_size=0.2, random_state=42
    )

    train_loader = DataLoader(
        SegmentationDataset(train_imgs, train_masks, feature_extractor),
        batch_size=BATCH_SIZE, shuffle=True,
    )
    val_loader = DataLoader(
        SegmentationDataset(val_imgs, val_masks, feature_extractor),
        batch_size=BATCH_SIZE, shuffle=False,
    )

    device = torch.device("cuda" if torch.cuda.is_available() else "cpu")
    model  = build_model(NUM_CLASSES).to(device)

    train_losses, val_losses = train(
        model, train_loader, val_loader,
        num_epochs=NUM_EPOCHS,
        lr=LEARNING_RATE,
        patience=PATIENCE,
        save_path=MODEL_SAVE_PATH,
    )
    plot_losses(train_losses, val_losses)

    # ── INFERENCE ─────────────────────────────
    model, device = load_model_for_inference(MODEL_SAVE_PATH, NUM_CLASSES)
    inf_extractor  = build_inference_extractor()

    predict_dirs(
        dir_spec_list=PRED_DIRS,
        base_path=BASE_PRED_PATH,
        model=model,
        feature_extractor=inf_extractor,
        patch_size=PATCH_SIZE,
        overlap=OVERLAP,
        device=device,
    )
