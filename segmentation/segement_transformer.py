#!/usr/bin/env python
# coding: utf-8

# In[1]:


import os
path="/cluster/scratch/kellebea"

os.chdir(path)
print(os.getcwd())


num_classes = 3  # Example: 0 = background, 1 = class1, 2 = class2
num_epochs = 24


# In[ ]:


image_dir="/cluster/scratch/kellebea/SoySeg/Train/RGB/"
mask_dir="/cluster/scratch/kellebea/SoySeg/Train/Mask/"



import glob
import os

image_paths_all = sorted(glob.glob(os.path.join(image_dir, "*.png")))
mask_paths_all = sorted(glob.glob(os.path.join(mask_dir, "*.png")))

# Filter out files containing "collage4" in either directory
image_paths = sorted([path for path in image_paths_all 
                    if "collage4" not in os.path.basename(path)])

mask_paths = sorted([path for path in mask_paths_all 
                   if "collage4" not in os.path.basename(path)])

print("Found", len(image_paths), "images")
print("Found", len(mask_paths), "masks")


# In[ ]:





# In[3]:


#pip install torch torchvision transformers datasets


# In[4]:


import torch
import torch.nn as nn
from torch.utils.data import Dataset, DataLoader
from transformers import SegformerForSemanticSegmentation, SegformerFeatureExtractor
from PIL import Image
import os
import numpy as np

# -------------- Custom Dataset ----------------
class SegmentationDataset(Dataset):
    def __init__(self, image_paths, mask_paths, feature_extractor):
        self.image_paths = image_paths
        self.mask_paths = mask_paths
        self.feature_extractor = feature_extractor

    def __getitem__(self, idx):
        image = Image.open(self.image_paths[idx]).convert("RGB")
        mask = Image.open(self.mask_paths[idx]).convert("L")  # 🔥 FIXED
    
        inputs = self.feature_extractor(images=image, return_tensors="pt")
        pixel_values = inputs["pixel_values"].squeeze(0)  # [3, H, W]
        
        mask = torch.tensor(np.array(mask), dtype=torch.long)  # [H, W]
        
        return pixel_values, mask


    def __len__(self):
        return len(self.image_paths)


# -------------- Data Loader ----------------
feature_extractor = SegformerFeatureExtractor.from_pretrained("nvidia/segformer-b0-finetuned-ade-512-512")
train_dataset = SegmentationDataset(image_paths, mask_paths, feature_extractor)
train_loader = DataLoader(train_dataset, batch_size=18, shuffle=True)

# -------------- Model Setup ----------------
model = SegformerForSemanticSegmentation.from_pretrained(
    "nvidia/segformer-b0-finetuned-ade-512-512",
    ignore_mismatched_sizes=True,
    num_labels=num_classes
)

device = torch.device("cuda" if torch.cuda.is_available() else "cpu")
model.to(device)



# In[ ]:


# -------------- Training Loop ----------------
optimizer = torch.optim.AdamW(model.parameters(), lr=5e-5)


train_losses = []  # Store average loss per epoch
    
for epoch in range(num_epochs):
        model.train()
        epoch_loss = 0.0
        batch_count = 0
    
        for pixel_values, masks in train_loader:
            pixel_values = pixel_values.to(device)
            masks = masks.to(device)
    
            outputs = model(pixel_values=pixel_values, labels=masks)
            loss = outputs.loss
            loss.backward()
            optimizer.step()
            optimizer.zero_grad()
    
            epoch_loss += loss.item()
            batch_count += 1
    
        avg_loss = epoch_loss / batch_count
        train_losses.append(avg_loss)
    
        print(f"Epoch {epoch} Average Loss: {avg_loss:.4f}")


# In[13]:


import matplotlib.pyplot as plt
import matplotlib.pyplot as plt

plt.figure(figsize=(8, 5))
plt.plot(train_losses, marker='o')
plt.title("Training Loss Curve")
plt.xlabel("Epoch")
plt.ylabel("Average Loss")
plt.grid(True)
plt.show()


# In[7]:


torch.save(model.state_dict(), "segformer_weights_excl4.pth")


# In[4]:


from transformers import SegformerConfig, SegformerForSemanticSegmentation
import torch

# Load the original config and modify num_labels
config = SegformerConfig.from_pretrained("nvidia/segformer-b0-finetuned-ade-512-512")
config.num_labels = 3  # or your number of classes

# Initialize the model with the config (no weights yet)
model = SegformerForSemanticSegmentation(config)

device = torch.device("cuda" if torch.cuda.is_available() else "cpu")
# Load your trained weights
model.load_state_dict(torch.load("segformer_weights.pth"))
model.to(device)
model.eval()


# In[3]:


from transformers import SegformerFeatureExtractor

feature_extractor = SegformerFeatureExtractor.from_pretrained(
    "nvidia/segformer-b0-finetuned-ade-512-512"
)


# In[34]:


path_to_new_image = "./SoySeg/Tests/FPSB0160111_RGB1_20220603_094248_q90.jpg"


# In[ ]:


from PIL import Image
import numpy as np
import matplotlib.pyplot as plt

# Load your image
image = Image.open(path_to_new_image).convert("RGB")

# Preprocess with the feature extractor
inputs = feature_extractor(images=image, return_tensors="pt").to(device)

# Forward pass
with torch.no_grad():
    outputs = model(**inputs)
    logits = outputs.logits  # shape: [1, num_classes, H, W]

# Get predicted mask (class with highest score)
predicted_mask = torch.argmax(logits, dim=1).squeeze().cpu().numpy()

# Visualize result
plt.figure(figsize=(10, 5))
plt.subplot(1, 2, 1)
plt.imshow(image)
plt.title("Input Image")
plt.axis("off")

plt.subplot(1, 2, 2)
plt.imshow(predicted_mask, cmap="jet")  # Or define a custom color map
plt.title("Predicted Mask")
plt.axis("off")
plt.show()


# In[26]:


best_loss = float('inf')

for epoch in range(num_epochs):
    model.train()
    epoch_loss = 0.0
    batch_count = 0

    for pixel_values, masks in train_loader:
        pixel_values = pixel_values.to(device)
        masks = masks.to(device)

        outputs = model(pixel_values=pixel_values, labels=masks)
        loss = outputs.loss
        loss.backward()
        optimizer.step()
        optimizer.zero_grad()

        epoch_loss += loss.item()
        batch_count += 1

    avg_loss = epoch_loss / batch_count
    print(f"Epoch {epoch} Average Loss: {avg_loss:.4f}")

    # ✅ Save the model with the lowest loss
    if avg_loss < best_loss:
        best_loss = avg_loss
        torch.save(model.state_dict(), "best_model_epochs.pth")
        print(f"✅ Saved best model at epoch {epoch} with loss {avg_loss:.4f}")


# In[ ]:





# In[8]:


from sklearn.model_selection import train_test_split

# Assuming image_paths and mask_paths are lists
train_imgs, val_imgs, train_masks, val_masks = train_test_split(
    image_paths, mask_paths, test_size=0.2, random_state=42
)


# In[9]:


train_dataset = SegmentationDataset(train_imgs, train_masks, feature_extractor)
val_dataset = SegmentationDataset(val_imgs, val_masks, feature_extractor)

train_loader = DataLoader(train_dataset, batch_size=16, shuffle=True)
val_loader = DataLoader(val_dataset, batch_size=16, shuffle=False)


# In[12]:


import torch
import numpy as np
optimizer = torch.optim.AdamW(model.parameters(), lr=5e-5)

train_losses = []
val_losses = []

best_val_loss = float('inf')
patience = 5
patience_counter = 0

for epoch in range(num_epochs):
    model.train()
    train_loss = 0.0

    for pixel_values, masks in train_loader:
        pixel_values = pixel_values.to(device)
        masks = masks.to(device)

        outputs = model(pixel_values=pixel_values, labels=masks)
        loss = outputs.loss
        loss.backward()
        optimizer.step()
        optimizer.zero_grad()

        train_loss += loss.item()

    avg_train_loss = train_loss / len(train_loader)
    train_losses.append(avg_train_loss)

    # Validation
    model.eval()
    val_loss = 0.0
    with torch.no_grad():
        for pixel_values, masks in val_loader:
            pixel_values = pixel_values.to(device)
            masks = masks.to(device)

            outputs = model(pixel_values=pixel_values, labels=masks)
            loss = outputs.loss
            val_loss += loss.item()

    avg_val_loss = val_loss / len(val_loader)
    val_losses.append(avg_val_loss)

    print(f"Epoch {epoch}: Train Loss = {avg_train_loss:.4f}, Val Loss = {avg_val_loss:.4f}")

    # Early stopping
    if avg_val_loss < best_val_loss:
        best_val_loss = avg_val_loss
        torch.save(model.state_dict(), "best_model_epoch_val.pth")
        patience_counter = 0
        print("✅ New best model saved.")
    else:
        patience_counter += 1
        if patience_counter >= patience:
            print("🛑 Early stopping triggered.")
            break


# In[13]:


import matplotlib.pyplot as plt

plt.figure(figsize=(8, 5))
plt.plot(train_losses, label="Train Loss", marker='o')
plt.plot(val_losses, label="Val Loss", marker='x')
plt.title("Training vs Validation Loss")
plt.xlabel("Epoch")
plt.ylabel("Loss")
plt.legend()
plt.grid(True)
plt.tight_layout()
plt.show()


# In[ ]:





# In[19]:


import torch
from transformers import SegformerConfig, SegformerForSemanticSegmentation

# Load config and set correct number of classes
config = SegformerConfig.from_pretrained("nvidia/segformer-b0-finetuned-ade-512-512")
config.num_labels = 3  # or however many classes you have

# Load model and weights from epoch 14
model = SegformerForSemanticSegmentation(config)
model.load_state_dict(torch.load("segformer_weights_excl4.pth"))
model.to(device)
model.eval()


# In[ ]:


import torch
import numpy as np
from PIL import Image

def sliding_window_segmentation(image, model, feature_extractor, patch_size=128, overlap=0, device='cpu'):
    model.eval()
    width, height = image.size
    stride = patch_size - overlap

    # Create an empty array for the final prediction mask
    final_mask = np.zeros((height, width), dtype=np.uint8)

    for top in range(0, height, stride):
        for left in range(0, width, stride):
            bottom = min(top + patch_size, height)
            right = min(left + patch_size, width)

            crop = image.crop((left, top, right, bottom))

            # Handle edge patches by padding
            padded_crop = Image.new("RGB", (patch_size, patch_size))
            padded_crop.paste(crop, (0, 0))

            # Preprocess input
            inputs = feature_extractor(images=padded_crop, return_tensors="pt").to(device)

            with torch.no_grad():
                outputs = model(**inputs)
                logits = outputs.logits  # [1, num_classes, 128, 128]
                pred_mask = torch.argmax(logits, dim=1).squeeze().cpu().numpy()

            # Remove padding if we're at the edge
            h_cut = bottom - top
            w_cut = right - left
            pred_mask = pred_mask[:h_cut, :w_cut]

            final_mask[top:bottom, left:right] = pred_mask

            import matplotlib.pyplot as plt
            
        


    return final_mask


# In[ ]:


image = Image.open(path_to_new_image).convert("RGB")
mask = sliding_window_segmentation(image, model, feature_extractor, device=device)

# Save or display
Image.fromarray(mask.astype(np.uint8)).save("predicted_full_mask.png")


# In[ ]:


Image.fromarray((mask * 100).clip(0, 255).astype(np.uint8)).save("debug_mask.png")


# In[ ]:


# Define colors for each class (R, G, B)
color_map = {
    0: (0, 0, 0),       # black for background
    1: (0, 255, 0),     # green for class 1
    2: (255, 0, 0),     # red for class 2
}

# Create color image
h, w = mask.shape
color_mask = np.zeros((h, w, 3), dtype=np.uint8)

for class_index, color in color_map.items():
    color_mask[mask == class_index] = color

# Convert and save as color PNG
color_image = Image.fromarray(color_mask)
color_image.save("predicted_colored_mask.png")


# In[ ]:


import matplotlib.pyplot as plt

plt.imshow(mask, cmap="gray", vmin=0, vmax=num_classes-1)
plt.title("Grayscale Class Mask")
plt.colorbar()
plt.show()


# In[ ]:



    plt.figure(figsize=(12, 6))
    plt.subplot(1, 2, 1)
    plt.imshow(image)
    plt.title("Original Image")
    
    plt.subplot(1, 2, 2)
    plt.imshow(mask, cmap="jet", alpha=0.6)
    plt.title("Predicted Mask")
    plt.show()


# In[14]:


import os
import glob
import torch
import numpy as np
from PIL import Image
from tqdm import tqdm
import torch.nn.functional as F
from transformers import SegformerFeatureExtractor

# Load feature extractor without resizing
feature_extractor = SegformerFeatureExtractor.from_pretrained("nvidia/segformer-b0-finetuned-ade-512-512")
feature_extractor.do_resize = False
feature_extractor.size = None

def predict_all_images_inplace(input_dir, model, feature_extractor, patch_size=256, overlap=32, device='cpu'):
    """
    Predict masks for all images in a directory using sliding window segmentation.
    Saves grayscale masks next to each original image.
    """
    # Supported formats
    exts = ["*.jpg", "*.jpeg", "*.png", "*.bmp", "*.tif"]
    image_files = []
    for ext in exts:
        image_files.extend(glob.glob(os.path.join(input_dir, "**", ext), recursive=True))

    print(f"Found {len(image_files)} images.")
    model.eval()

    for img_path in tqdm(image_files, desc="Predicting masks"):
        if "_mask" in os.path.basename(img_path):
            continue

        image = Image.open(img_path).convert("RGB")
        width, height = image.size
        stride = patch_size - overlap
        final_mask = np.zeros((height, width), dtype=np.uint8)

        for top in range(0, height, stride):
            for left in range(0, width, stride):
                bottom = min(top + patch_size, height)
                right = min(left + patch_size, width)

                crop = image.crop((left, top, right, bottom))
                padded_crop = Image.new("RGB", (patch_size, patch_size))
                padded_crop.paste(crop, (0, 0))

                # Preprocess input without resizing
                inputs = feature_extractor(images=padded_crop, return_tensors="pt").to(device)

                with torch.no_grad():
                    outputs = model(**inputs)
                    logits = outputs.logits  # [1, num_classes, H_out, W_out]

                    # 🔥 Upsample logits to match patch_size
                    upsampled = F.interpolate(logits, size=(patch_size, patch_size), mode="bilinear", align_corners=False)
                    pred_mask = torch.argmax(upsampled, dim=1).squeeze().cpu().numpy()

                # Crop prediction to match image slice (in case it's on edge)
                h_cut = bottom - top
                w_cut = right - left
                pred_mask = pred_mask[:h_cut, :w_cut]

                final_mask[top:bottom, left:right] = pred_mask

        # Save the predicted mask (amplified for visibility)
        debug_mask = (final_mask * 100).clip(0, 255).astype(np.uint8)
        mask_image = Image.fromarray(debug_mask)

        base, ext = os.path.splitext(img_path)
        mask_path = f"{base}_mask.png"
        mask_image.save(mask_path)
        print(f"✅ Saved: {mask_path}")
        del inputs, outputs, logits, upsampled, pred_mask
        torch.cuda.empty_cache()


# In[ ]:





# In[15]:


predict_all_images_inplace(
    input_dir="./SoySeg/Tests",
    model=model,
    feature_extractor=feature_extractor,
    patch_size=1536,  # Or 128, 512, etc.
    overlap=64,
    device=device
)


# In[ ]:





# In[ ]:





# In[ ]:





# In[2]:


import os
path="/home/kellebea/public/Evaluation/Projects/KP0023_legumes/Soybean/"

os.chdir(path)
print(os.getcwd())


# In[3]:


import os
import glob
import torch
import numpy as np
from PIL import Image
from tqdm import tqdm
import torch.nn.functional as F
from transformers import SegformerConfig, SegformerFeatureExtractor, SegformerForSemanticSegmentation

# Force PyTorch to use CPU
device = torch.device("cpu")
torch.set_num_threads(os.cpu_count())  # Optional: utilize all CPU cores

# Load feature extractor with no resizing
feature_extractor = SegformerFeatureExtractor.from_pretrained("nvidia/segformer-b0-finetuned-ade-512-512")
feature_extractor.do_resize = False
feature_extractor.size = None

# Load model config and weights
config = SegformerConfig.from_pretrained("nvidia/segformer-b0-finetuned-ade-512-512")
config.num_labels = 3  # Change as needed

model = SegformerForSemanticSegmentation(config)
model.load_state_dict(torch.load("Train/best_model_epoch_val.pth", map_location=device))  # 👈 force CPU loading
model.to(device)
model.eval()

def predict_all_images_inplace(input_dir, model, feature_extractor, patch_size=256, overlap=32, device='cpu'):
    exts = ["*_q90.jpg"]
    image_files = []
    for ext in exts:
        image_files.extend(glob.glob(os.path.join(input_dir, "**", ext), recursive=True))

    print(f"Found {len(image_files)} images.")
    model.eval()

    for img_path in tqdm(image_files, desc="Predicting masks"):
        if "_mask" in os.path.basename(img_path):
            continue

        image = Image.open(img_path).convert("RGB")
        width, height = image.size
        stride = patch_size - overlap
        final_mask = np.zeros((height, width), dtype=np.uint8)

        for top in range(0, height, stride):
            for left in range(0, width, stride):
                bottom = min(top + patch_size, height)
                right = min(left + patch_size, width)

                crop = image.crop((left, top, right, bottom))
                padded_crop = Image.new("RGB", (patch_size, patch_size))
                padded_crop.paste(crop, (0, 0))

                # Process patch on CPU
                inputs = feature_extractor(images=padded_crop, return_tensors="pt")
                inputs = {k: v.to(device) for k, v in inputs.items()}

                with torch.no_grad():
                    outputs = model(**inputs)
                    logits = outputs.logits
                    upsampled = F.interpolate(logits, size=(patch_size, patch_size), mode="bilinear", align_corners=False)
                    pred_mask = torch.argmax(upsampled, dim=1).squeeze().cpu().numpy()

                h_cut = bottom - top
                w_cut = right - left
                pred_mask = pred_mask[:h_cut, :w_cut]
                final_mask[top:bottom, left:right] = pred_mask

        # Save debug mask
        debug_mask = (final_mask * 100).clip(0, 255).astype(np.uint8)
        mask_image = Image.fromarray(debug_mask)
        base, ext = os.path.splitext(img_path)
        base = base.replace("_q90", "")
        mask_path = f"{base}_mask.png"
        mask_image.save(mask_path)

        print(f"✅ Saved: {mask_path}")


# In[32]:


predict_all_images_inplace(
    input_dir="/home/kellebea/public/Evaluation/Projects/KP0023_legumes/Cache/aatest/",
    model=model,
    feature_extractor=feature_extractor,
    patch_size=2048,
    overlap=128,
    device=device
)


# In[33]:


import torch
print("PyTorch threads:", torch.get_num_threads())


# In[29]:


import pkg_resources
import yaml

packages = sorted(["{}=={}".format(i.key, i.version) for i in pkg_resources.working_set])
env = {"dependencies": packages}

with open("env:root_export.yml", "w") as f:
    yaml.dump(env, f)

print("✅ Environment exported to env_export.yml")


# In[ ]:




def predict_all_dirs_inplace(dirlist, base_path, model, feature_extractor, patch_size=256, overlap=32, device='cpu'):
    for subdir in dirlist:
        full_path = os.path.join(base_path, subdir)
        print(f"\n🔁 Running prediction for directory: {full_path}")
        predict_all_images_inplace(
            input_dir=full_path,
            model=model,
            feature_extractor=feature_extractor,
            patch_size=patch_size,
            overlap=overlap,
            device=device
        )


# In[ ]:



import os
path="/home/kellebea/public/Evaluation/Projects/KP0023_legumes/Soybean/2022/SB016/RGB1/Segmentation/"

os.chdir(path)
print(os.getcwd())

dirlist_all = [ item for item in os.listdir(path) if os.path.isdir(os.path.join(path, item)) ]
dirlist_all=sorted(dirlist_all)
#print(dirlist_all)
####
dirlist = dirlist_all[2:10]
dirlist




predict_all_dirs_inplace(
    dirlist=dirlist,
    base_path=path,
    model=model,
    feature_extractor=feature_extractor,
    patch_size=2048,
    overlap=128,
    device=device
)




import os
path="/home/kellebea/public/Evaluation/Projects/KP0023_legumes/Soybean/2021/SB015/RGB1/Segmentation/"

os.chdir(path)
print(os.getcwd())

dirlist_all = [ item for item in os.listdir(path) if os.path.isdir(os.path.join(path, item)) ]
dirlist_all=sorted(dirlist_all)
#print(dirlist_all)
####
dirlist = dirlist_all[1:9]
dirlist



predict_all_dirs_inplace(
    dirlist=dirlist,
    base_path=path,
    model=model,
    feature_extractor=feature_extractor,
    patch_size=2048,
    overlap=128,
    device=device
)



import os
path="/home/kellebea/public/Evaluation/Projects/KP0023_legumes/Soybean/2020/SB014/RGB1/Segmentation/"

os.chdir(path)
print(os.getcwd())

dirlist_all = [ item for item in os.listdir(path) if os.path.isdir(os.path.join(path, item)) ]
dirlist_all=sorted(dirlist_all)
#print(dirlist_all)
####
dirlist = dirlist_all[3:10]
dirlist





predict_all_dirs_inplace(
    dirlist=dirlist,
    base_path=path,
    model=model,
    feature_extractor=feature_extractor,
    patch_size=2048,
    overlap=128,
    device=device
)







