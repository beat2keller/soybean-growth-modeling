#!pip install -q "transformers==4.49.0"

# ====== CELL A — setup + config =============================================
import importlib.metadata as md
NEED = "4.49.0"
try:
    have = md.version("transformers")
except md.PackageNotFoundError:
    have = None
if have != NEED:
    print(f"transformers {have} -> installing {NEED}")
    !pip install -q "transformers=={NEED}"
    raise SystemExit("✅ installed — now RESTART the runtime (Runtime → Restart session), then re-run from Cell A")
print("transformers", have, "OK")

# (Drive already mounted from your earlier cells)
!pip -q install scikit-image >/dev/null 2>&1

from pathlib import Path
import numpy as np, io, base64
from PIL import Image
from skimage.segmentation import slic, find_boundaries

IMG_DIR    = Path("/canopy-cover-stats-lab/segmentation/annotation-validation/")
LABELS_DIR = IMG_DIR / "labels"                 # masks saved here
EXTS       = {".png", ".jpg", ".jpeg", ".tif", ".tiff"}

# green filter — unchanged from your pipeline
GMIN, RATG, RATGB = 25, 1.06, 1.08

# superpixels: N_SP is an upper bound (SLIC undershoots). Higher = finer edges = more clicks.
N_SP, COMPACT = 400, 12
TREAT_BLACK_AS_BORDER = True     # ortho-cut plots often have black padding; set False if yours are tight crops
DISPLAY_MAX = 1400               # cap transmitted overlay width (px) for click latency

def green_mask(rgb):
    r = rgb[...,0].astype(np.float32); g = rgb[...,1].astype(np.float32); b = rgb[...,2].astype(np.float32)
    return (g > r*RATG) & (g > b*RATGB) & (g > GMIN)

# ====== CELL A additions (append to your config) ============================
SPLIT_AREA = 30000     # green components larger than this get SLIC-subdivided;
                       # smaller ones stay ONE clickable unit (one click per plant/weed).
                       # raise if isolated plants split; lower for finer weed removal in dense canopy.
N_SP, COMPACT = 300, 10   # SLIC granularity used only inside oversized components
CLICK_BOX = 128        # a plain click only affects green within this window (px) around the cursor

# ====== CELL B — load images + green clusters + per-pixel labels =============
from skimage.measure import label as cc_label
from skimage.segmentation import find_boundaries

def load_image(path):
    if path.suffix.lower() in {".tif", ".tiff"}:
        import rasterio
        with rasterio.open(path) as src:
            arr = src.read()
            rgb = np.transpose(arr[:3], (1,2,0)).astype(np.uint8)
            if src.count >= 4:            valid = arr[3] > 0
            elif src.nodata is not None:  valid = ~np.all(arr[:3]==src.nodata, axis=0)
            else:                         valid = ~np.all(arr[:3]==0, axis=0)
    else:
        im = Image.open(path)
        if im.mode == "RGBA":
            a = np.array(im); rgb = a[...,:3]; valid = a[...,3] > 0
        else:
            rgb = np.array(im.convert("RGB"))
            valid = np.ones(rgb.shape[:2], bool)
    if TREAT_BLACK_AS_BORDER:
        valid &= ~np.all(rgb == 0, axis=2)
    return rgb, valid

def build_units(rgb, valid, name):
    green = green_mask(rgb) & valid
    units = cc_label(green, connectivity=2).astype(np.int32)
    seed  = np.where(green, 1, 0).astype(np.uint8)             # all-soybean baseline
    plab, loaded = seed.copy(), False
    lp = LABELS_DIR / f"{name}_label.png"
    if lp.exists():                                            # resume prior labeling
        saved = np.array(Image.open(lp))
        if saved.shape == green.shape:
            plab = np.zeros_like(seed)
            plab[saved == 1] = 1; plab[saved == 2] = 2         # nodata(3)/soil(0) -> 0
            plab[~green] = 0
            loaded = True
    bnd = find_boundaries(units, mode="thick") & green
    return dict(units=units, green=green, plab=plab, seed_plab=seed.copy(), bnd=bnd, loaded=loaded)

SESSION = []
for p in sorted(q for q in IMG_DIR.iterdir() if q.suffix.lower() in EXTS):
    rgb, valid = load_image(p)
    SESSION.append(dict(name=p.stem, rgb=rgb, valid=valid, **build_units(rgb, valid, p.stem)))
print(f"loaded {len(SESSION)} plot(s)")
for S in SESSION:
    tag = "  ↩ resumed" if S["loaded"] else ""
    print(f"  {S['name']:28s} clusters={int(S['units'].max()):4d}  green={S['green'].mean()*100:5.1f}%{tag}")

SESSION=SESSION[0:6] # only relevant years which have noteworthy weed pressure and hence, were used for soybean-weed segmentation
for S in SESSION:
    print(f"  {S['name']:28s} clusters={int(S['units'].max()):4d}  green={S['green'].mean()*100:5.1f}%{tag}")

# ====== CELL C — labeler: click cluster · Ctrl-drag rect · erase brush ========
from IPython.display import HTML, JSON, display
from google.colab import output as colab_output
import io, base64

STATE = {"i": 0}

def _overlay_full(i):
    S = SESSION[i]; plab = S["plab"]; disp = S["rgb"].astype(np.float32).copy()
    soy = plab == 1; weed = plab == 2
    disp[soy]  = 0.50*disp[soy]  + 0.50*np.array([40,220,40])
    disp[weed] = 0.40*disp[weed] + 0.60*np.array([255,120,0])
    disp[S["bnd"]] = [255,255,0]; disp[~S["valid"]] = [0,0,0]
    return disp.clip(0,255).astype(np.uint8)

def _uri(i):
    im = Image.fromarray(_overlay_full(i)); w,h = im.size
    if w > DISPLAY_MAX: im = im.resize((DISPLAY_MAX, round(h*DISPLAY_MAX/w)), Image.NEAREST)
    b = io.BytesIO(); im.save(b,"PNG"); return "data:image/png;base64,"+base64.b64encode(b.getvalue()).decode()

def _status(i):
    S = SESSION[i]; return f"[{i+1}/{len(SESSION)}]  {S['name']}  —  weed px: {int((S['plab']==2).sum())}"

def _save_one(i):
    S = SESSION[i]; LABELS_DIR.mkdir(parents=True, exist_ok=True)
    out = S["plab"].copy(); out[~S["valid"]] = 3               # 0 soil,1 soybean,2 weed,3 nodata
    Image.fromarray(out).save(LABELS_DIR / f"{S['name']}_label.png")
    Image.fromarray(_overlay_full(i)).save(LABELS_DIR / f"{S['name']}_overlay.png")
    vp = (out != 3).sum(); return 100.0*(out==1).sum()/vp if vp else float("nan")

def _pl(i, msg=None):
    S = SESSION[i]
    return JSON({"img": _uri(i), "w": int(S["rgb"].shape[1]), "h": int(S["rgb"].shape[0]),
                 "status": (msg+" — " if msg else "")+_status(i)})

def _unit(i, x, y):
    S = SESSION[i]; U = S["units"]; H, W = U.shape
    if not (0 <= y < H and 0 <= x < W): return
    u = int(U[y, x])
    if u < 1: return                                          # soil ignored
    h = CLICK_BOX // 2
    y0, y1 = max(0, y-h), min(H, y+h); x0, x1 = max(0, x-h), min(W, x+h)
    reg = np.zeros(U.shape, bool); reg[y0:y1, x0:x1] = True
    m = (U == u) & reg                                        # this cluster, capped to the box
    cur = S["plab"][m]
    S["plab"][m] = 2 if (cur==1).sum() >= (cur==2).sum() else 1

def _rect(i, x0, y0, x1, y1):
    S = SESSION[i]; H, W = S["plab"].shape
    xa, xb = sorted((max(0,x0), min(W-1,x1))); ya, yb = sorted((max(0,y0), min(H-1,y1)))
    box = np.zeros(S["plab"].shape, bool); box[ya:yb+1, xa:xb+1] = True
    S["plab"][box & S["green"]] = 2                             # only green pixels -> weed

def _erase(i, pts, r):
    S = SESSION[i]; plab = S["plab"]; H, W = plab.shape
    for x, y in pts:
        x, y = int(x), int(y)
        y0, y1 = max(0,y-r), min(H,y+r+1); x0, x1 = max(0,x-r), min(W,x+r+1)
        if y1 <= y0 or x1 <= x0: continue
        gy, gx = np.ogrid[y0:y1, x0:x1]; disk = (gy-y)**2 + (gx-x)**2 <= r*r
        sub = plab[y0:y1, x0:x1]; sub[disk & (sub==2)] = 1      # only weed -> soybean

def labeler_cmd(cmd, payload):
    i = STATE["i"]; msg = None
    if   cmd == "unit":  _unit(i, payload["x"], payload["y"])
    elif cmd == "rect":  _rect(i, payload["x0"], payload["y0"], payload["x1"], payload["y1"])
    elif cmd == "erase": _erase(i, payload["pts"], int(payload["r"]))
    elif cmd == "nav":   STATE["i"] = i = max(0, min(len(SESSION)-1, i+int(payload["d"])))
    elif cmd == "reset": SESSION[i]["plab"] = SESSION[i]["seed_plab"].copy(); msg = "reset to all-soybean"
    elif cmd == "save":  cc = _save_one(i); msg = f"saved ✓ soybean CC={cc:.1f}%"
    elif cmd == "saveall":
        for j in range(len(SESSION)): _save_one(j)
        msg = f"saved ALL {len(SESSION)} ✓"
    return _pl(STATE["i"], msg)

colab_output.register_callback("labeler_cmd", labeler_cmd)

display(HTML("""
<div style="font-family:monospace;font-size:13px">
  <div id="st" style="margin:4px 0">loading…</div>
  <div style="margin:4px 0">
    <button onclick="nav(-1)">◀ prev</button><button onclick="nav(1)">next ▶</button>
    &nbsp;<button id="tUnit" onclick="setTool('unit')">click-cluster</button>
    <button id="tErase" onclick="setTool('erase')">erase brush</button>
    &nbsp;brush <input id="rad" type="range" min="4" max="60" value="14"
      oninput="radv.textContent=this.value"><span id="radv">14</span>px
    &nbsp;<button onclick="send('reset',{})">reset</button>
    <button onclick="send('save',{})">save this</button>
    <button onclick="send('saveall',{})">💾 save ALL</button>
  </div>
  <div style="color:#666;margin:2px 0">click green cluster → weed &nbsp;·&nbsp; <b>Ctrl+drag</b> rectangle → green becomes weed &nbsp;·&nbsp; erase-brush drag → weed back to soybean</div>
  <canvas id="cv" style="cursor:crosshair;border:1px solid #999;max-width:100%"></canvas>
</div>
<script>
const cv=document.getElementById('cv'),ctx=cv.getContext('2d'),st=document.getElementById('st'),radEl=document.getElementById('rad');
let OW=1,OH=1,tool='unit',drag=null,pts=[],start=null;
const im=new Image(); im.onload=()=>ctx.drawImage(im,0,0,cv.width,cv.height);
function render(d){OW=d.w;OH=d.h;cv.width=OW;cv.height=OH;im.src=d.img;st.textContent=d.status;}
async function send(c,p){const r=await google.colab.kernel.invokeFunction('labeler_cmd',[c,p],{});render(r.data['application/json']);}
function nav(d){send('nav',{d:d});}
function setTool(t){tool=t;tUnit.style.fontWeight=t=='unit'?'bold':'normal';tErase.style.fontWeight=t=='erase'?'bold':'normal';}
setTool('unit');
function P(ev){const r=cv.getBoundingClientRect();return [Math.round((ev.clientX-r.left)*(OW/r.width)),Math.round((ev.clientY-r.top)*(OH/r.height))];}
function repaint(){ctx.drawImage(im,0,0,cv.width,cv.height);}
cv.addEventListener('mousedown',ev=>{ev.preventDefault();const p=P(ev);
  if(ev.ctrlKey||ev.metaKey){drag='rect';start=p;}
  else if(tool=='erase'){drag='erase';pts=[p];}
  else{drag='unit';start=p;}});
cv.addEventListener('mousemove',ev=>{if(!drag)return;const p=P(ev),r=+radEl.value;
  if(drag=='erase'){const l=pts[pts.length-1];
    if((p[0]-l[0])**2+(p[1]-l[1])**2>(r*0.5)**2)pts.push(p);
    repaint();ctx.fillStyle='rgba(40,220,40,0.45)';for(const q of pts){ctx.beginPath();ctx.arc(q[0],q[1],r,0,7);ctx.fill();}}
  else if(drag=='rect'){repaint();ctx.strokeStyle='yellow';ctx.lineWidth=2;
    ctx.strokeRect(Math.min(start[0],p[0]),Math.min(start[1],p[1]),Math.abs(p[0]-start[0]),Math.abs(p[1]-start[1]));}});
window.addEventListener('mouseup',ev=>{if(!drag)return;const p=P(ev);
  if(drag=='erase')send('erase',{pts:pts,r:+radEl.value});
  else if(drag=='rect')send('rect',{x0:start[0],y0:start[1],x1:p[0],y1:p[1]});
  else if(drag=='unit')send('unit',{x:start[0],y:start[1]});
  drag=null;pts=[];});
send('nav',{d:0});
</script>
"""))

# ====== CELL E — run trained SegFormer on the labeled plots =================
import torch, torch.nn.functional as F, warnings
warnings.filterwarnings("ignore")
from transformers import SegformerForSemanticSegmentation, SegformerConfig
try:
    from transformers import SegformerImageProcessor as SegProc
except ImportError:
    from transformers import SegformerFeatureExtractor as SegProc

WEIGHTS       = IMG_DIR.parent / "model_soybean_segmentation.pth"
PREDS_DIR     = IMG_DIR / "preds"
PRETRAINED_ID = "nvidia/segformer-b0-finetuned-ade-512-512"
NUM_CLASSES   = 3
PATCH_SIZE, OVERLAP = 2048, 128
assert WEIGHTS.exists(), f"weights not found at {WEIGHTS}"

def load_model():
    dev = torch.device("cuda" if torch.cuda.is_available() else "cpu")
    cfg = SegformerConfig.from_pretrained(PRETRAINED_ID); cfg.num_labels = NUM_CLASSES
    m = SegformerForSemanticSegmentation(cfg)
    m.load_state_dict(torch.load(WEIGHTS, map_location=dev)); m.to(dev).eval()
    fe = SegProc.from_pretrained(PRETRAINED_ID); fe.do_resize = False
    print("model on", dev); return m, fe, dev

def predict_image(image, model, fe, dev, patch=PATCH_SIZE, overlap=OVERLAP):
    W, H = image.size; stride = patch - overlap
    out = np.zeros((H, W), np.uint8)
    for top in range(0, H, stride):
        for left in range(0, W, stride):
            bot = min(top+patch, H); right = min(left+patch, W)
            pad = Image.new("RGB", (patch, patch)); pad.paste(image.crop((left, top, right, bot)), (0, 0))
            inp = fe(images=pad, return_tensors="pt"); inp = {k: v.to(dev) for k, v in inp.items()}
            with torch.no_grad():
                up = F.interpolate(model(**inp).logits, size=(patch, patch), mode="bilinear", align_corners=False)
                pr = torch.argmax(up, 1).squeeze().cpu().numpy().astype(np.uint8)
            out[top:bot, left:right] = pr[:bot-top, :right-left]
    return out


# ====== CELL E — memory-lean prediction loop (replaces the run loop) =========
import gc

PATCH_SIZE, OVERLAP = 1024, 128        # smaller window = less peak RAM per tile

PREDS_DIR.mkdir(parents=True, exist_ok=True)
# work from file paths, NOT from SESSION (avoids holding every plot in RAM)
# only predict labeled plots that don't already have a saved prediction

todo = []
for p in sorted(q for q in IMG_DIR.iterdir() if q.suffix.lower() in EXTS):
    if (LABELS_DIR / f"{p.stem}_label.png").exists() and not (PREDS_DIR / f"{p.stem}.png").exists():
        todo.append(p)
print(f"{len(todo)} to predict")

if todo:
    model, fe, dev = load_model()
    for p in todo:
        rgb, _ = load_image(p)                       # load one plot
        img = Image.fromarray(rgb)
        del rgb; gc.collect()
        pred = predict_image(img, model, fe, dev, patch=PATCH_SIZE, overlap=OVERLAP)
        Image.fromarray(pred).save(PREDS_DIR / f"{p.stem}.png")
        print(f"  ✓ {p.stem}  {img.size}  classes={np.unique(pred).tolist()}")
        del img, pred; gc.collect()
        if dev.type == "cuda": torch.cuda.empty_cache()
    del model; gc.collect()
    if dev.type == "cuda": torch.cuda.empty_cache()
else:
    print("nothing to predict — all cached")

# ---- sanity check with class legend: confirm class-1=soybean, class-2=weed ----
import matplotlib.pyplot as plt
from matplotlib.colors import ListedColormap, BoundaryNorm
import matplotlib.patches as mpatches

CLASS_COLORS = ["#8c6d4a", "#2ecc40", "#ff851b", "#000000"]   # 0 soil,1 soybean,2 weed,3 nodata
CLASS_NAMES  = ["0 soil", "1 soybean", "2 weed", "3 nodata"]
cmap = ListedColormap(CLASS_COLORS); norm = BoundaryNorm([-.5,.5,1.5,2.5,3.5], cmap.N)

preds_ready = [s for s in SESSION if (PREDS_DIR / f"{s['name']}.png").exists()]
if preds_ready:
    S  = preds_ready[0]
    gt = np.array(Image.open(LABELS_DIR / f"{S['name']}_label.png"))
    pr = np.array(Image.open(PREDS_DIR  / f"{S['name']}.png"))
    fig, ax = plt.subplots(1, 3, figsize=(16, 6))
    ax[0].imshow(S["rgb"]);                 ax[0].set_title("RGB")
    ax[1].imshow(gt, cmap=cmap, norm=norm); ax[1].set_title("GT (hand labels)")
    ax[2].imshow(pr, cmap=cmap, norm=norm); ax[2].set_title("SegFormer prediction")
    for a in ax: a.axis("off")
    handles = [mpatches.Patch(color=CLASS_COLORS[i], label=CLASS_NAMES[i]) for i in range(4)]
    fig.legend(handles=handles, loc="lower center", ncol=4, frameon=False, bbox_to_anchor=(0.5, 0.02))
    fig.suptitle(S["name"], fontsize=10)
    plt.tight_layout(); plt.show()
    print("Class-1 should be green on soybean in BOTH panels. If pred's green/orange are swapped vs GT, set PRED_SOYBEAN/PRED_WEED in Cell F.")
else:
    print("no predictions available to visualize")

# ====== CELL F — metrics: SegFormer vs hand labels =========================
PRED_SOYBEAN, PRED_WEED = 1, 2       # set from Cell E's sanity check (swap if the map is inverted)

def _sc(gt, pred):
    valid = gt != 3; g, p = gt[valid], pred[valid]
    iou  = lambda a, b: (np.nan if (a|b).sum()==0 else (a&b).sum()/(a|b).sum())
    dice = lambda a, b: (np.nan if a.sum()+b.sum()==0 else 2*(a&b).sum()/(a.sum()+b.sum()))
    soy_i = iou(g==1, p==PRED_SOYBEAN); soy_d = dice(g==1, p==PRED_SOYBEAN)
    weed_i = iou(g==2, p==PRED_WEED);   soil_i = iou(g==0, p==0)
    return (soy_i, soy_d, weed_i, soil_i, np.nanmean([soy_i, weed_i, soil_i]),
            (g==1).mean(), (p==PRED_SOYBEAN).mean(),          # soybean CC: GT, pred
            (g==2).mean(), (p==PRED_WEED).mean())             # weed CC:    GT, pred

def _boot(v, n=5000, s=0):
    v = np.array([x for x in v if not np.isnan(x)]); r = np.random.default_rng(s)
    if not len(v): return (np.nan,)*3
    b = [r.choice(v, len(v), True).mean() for _ in range(n)]
    return v.mean(), np.percentile(b, 2.5), np.percentile(b, 97.5)

rows, SOY, SOYD, MI, ccg, ccp, wcg, wcp = [], [], [], [], [], [], [], []
for S in SESSION:
    lp = LABELS_DIR / f"{S['name']}_label.png"; pp = PREDS_DIR / f"{S['name']}.png"
    if not (lp.exists() and pp.exists()): continue
    gt = np.array(Image.open(lp)); pr = np.array(Image.open(pp))
    si, sd, wi, soi, mi, cg, cp, wg, wp = _sc(gt, pr)
    rows.append((S["name"], si, sd, wi, soi, mi, wg, wp))
    SOY.append(si); SOYD.append(sd); MI.append(mi)
    ccg.append(cg); ccp.append(cp); wcg.append(wg); wcp.append(wp)

print(f"{'plot':26s}{'soyIoU':>8}{'soyDice':>9}{'weedIoU':>9}{'soilIoU':>9}{'mIoU':>7}{'weedCCgt':>10}{'weedCCpr':>10}")
for r in rows:
    print(f"{r[0]:26s}{r[1]:8.3f}{r[2]:9.3f}{r[3]:9.3f}{r[4]:9.3f}{r[5]:7.3f}{r[6]:10.4f}{r[7]:10.4f}")
print("-"*88)
for nm, a in [("soybean IoU", SOY), ("soybean Dice/F1", SOYD), ("mIoU (3-class)", MI)]:
    m, lo, hi = _boot(a); print(f"{nm:16s} mean={m:.3f}  95% CI [{lo:.3f}, {hi:.3f}]  (n={len(a)} plots)")

ccg, ccp, wcg, wcp = map(np.array, (ccg, ccp, wcg, wcp))
print(f"soybean CC (0-1 fraction):  RMSE={np.sqrt(((ccp-ccg)**2).mean()):.4f}  "
      f"bias={(ccp-ccg).mean():+.4f}  r={np.corrcoef(ccg, ccp)[0,1]:.3f}")
print(f"weed CC    (0-1 fraction):  RMSE={np.sqrt(((wcp-wcg)**2).mean()):.4f}  "
      f"bias={(wcp-wcg).mean():+.4f}  r={np.corrcoef(wcg, wcp)[0,1]:.3f}")
print(f"weed CC mean:  GT={wcg.mean():.4f} ({100*wcg.mean():.2f}%)   "
      f"pred={wcp.mean():.4f} ({100*wcp.mean():.2f}%)   range GT=[{wcg.min():.4f}, {wcg.max():.4f}]")

# weed as share of total green (weed / (soybean + weed)) — the "% of canopy" figure
sh_g = wcg / (ccg + wcg); sh_p = wcp / (ccp + wcp)
print(f"weed share of green:  GT mean={100*sh_g.mean():.2f}%  max={100*sh_g.max():.2f}%   "
      f"pred mean={100*sh_p.mean():.2f}%")

# error contributed if weeds were left in the soybean class
print(f"if weeds unsegmented, soybean CC would be inflated by "
      f"{100*wcg.mean():.2f} pp on average (max {100*wcg.max():.2f} pp)")

# ====== CELL G — review panel: RGB | manual | SegFormer, one row per plot ====
import matplotlib.pyplot as plt
from matplotlib.colors import ListedColormap, BoundaryNorm
import matplotlib.patches as mpatches

CLASS_COLORS = ["#8c6d4a", "#2ecc40", "#ff851b", "#000000"]   # 0 soil,1 soybean,2 weed,3 nodata
CLASS_NAMES  = ["Soil (class 0)", "Soybean (class 1)", "Weed (class 2)", "No data (class 3)"]
cmap = ListedColormap(CLASS_COLORS); norm = BoundaryNorm([-.5,.5,1.5,2.5,3.5], cmap.N)

panel = sorted([S for S in SESSION
                if (LABELS_DIR / f"{S['name']}_label.png").exists()
                and (PREDS_DIR / f"{S['name']}.png").exists()],
               key=lambda s: s["name"])
assert panel, "no plots have both a label and a prediction — run Cell E first"
n = len(panel)

fig, axes = plt.subplots(n, 3, figsize=(12, 4*n), squeeze=False)
col_titles = ["RGB", "Manual annotation", "SegFormer prediction"]
for r, S in enumerate(panel):
    gt = np.array(Image.open(LABELS_DIR / f"{S['name']}_label.png"))
    pr = np.array(Image.open(PREDS_DIR  / f"{S['name']}.png"))
    axes[r,0].imshow(S["rgb"])
    axes[r,1].imshow(gt, cmap=cmap, norm=norm, interpolation="nearest")
    axes[r,2].imshow(pr, cmap=cmap, norm=norm, interpolation="nearest")
    for c in range(3):
        axes[r,c].set_xticks([]); axes[r,c].set_yticks([])
        if r == 0: axes[r,c].set_title(col_titles[c], fontsize=12)
    axes[r,0].set_ylabel(S["name"], fontsize=7, rotation=90, va="center")

handles = [mpatches.Patch(color=CLASS_COLORS[i], label=CLASS_NAMES[i]) for i in range(4)]
fig.legend(handles=handles, loc="lower center", ncol=4, frameon=False, bbox_to_anchor=(0.5, 0.0))
plt.tight_layout(rect=[0, 0.02, 1, 1])

out_path = LABELS_DIR / "review_panel.png"
fig.savefig(out_path, dpi=150, bbox_inches="tight")
plt.show()
print("saved:", out_path)

# ====== CELL H — RGB | soybean error map (TP/FN/FP), one row per plot ========
import matplotlib.pyplot as plt
import matplotlib.patches as mpatches

PRED_SOYBEAN = 1     # keep consistent with Cell F (flip if your mapping is inverted)
C_TP = (0.18, 0.80, 0.25)   # both soybean
C_FN = (0.00, 0.45, 0.85)   # manual soybean, model missed  -> under-segmentation
C_FP = (0.90, 0.15, 0.15)   # model soybean, manual didn't  -> over-segmentation
C_BG = (0.92, 0.92, 0.92)   # agreement on non-soybean / not CC-relevant

def soy_error_rgb(gt, pr):
    H, W = gt.shape
    img = np.ones((H, W, 3)) * np.array(C_BG)
    valid = gt != 3
    gs, ps = (gt == 1), (pr == PRED_SOYBEAN)
    img[gs & ps & valid] = C_TP
    img[gs & ~ps & valid] = C_FN
    img[~gs & ps & valid] = C_FP
    img[~valid] = [1, 1, 1]        # nodata -> white
    return img

def soy_iou(gt, pr):
    v = gt != 3; g = gt[v] == 1; p = pr[v] == PRED_SOYBEAN
    u = (g | p).sum(); return np.nan if u == 0 else (g & p).sum() / u

panel = sorted([S for S in SESSION
                if (LABELS_DIR / f"{S['name']}_label.png").exists()
                and (PREDS_DIR / f"{S['name']}.png").exists()],
               key=lambda s: s["name"])
assert panel, "no plots have both a label and a prediction — run Cell E first"
n = len(panel)

fig, axes = plt.subplots(n, 2, figsize=(9, 4.2*n), squeeze=False)
for r, S in enumerate(panel):
    gt = np.array(Image.open(LABELS_DIR / f"{S['name']}_label.png"))
    pr = np.array(Image.open(PREDS_DIR  / f"{S['name']}.png"))
    axes[r,0].imshow(S["rgb"])
    axes[r,1].imshow(soy_error_rgb(gt, pr), interpolation="nearest")
    for c in range(2): axes[r,c].set_xticks([]); axes[r,c].set_yticks([])
    if r == 0:
        axes[r,0].set_title("RGB", fontsize=12)
        axes[r,1].set_title("soybean error map", fontsize=12)
    axes[r,0].set_ylabel(S["name"], fontsize=7)
    axes[r,1].text(0.5, -0.05, f"soybean IoU = {soy_iou(gt, pr):.3f}",
                   transform=axes[r,1].transAxes, ha="center", fontsize=8)

leg = [mpatches.Patch(color=C_TP, label="TP (both soybean)"),
       mpatches.Patch(color=C_FN, label="FN (missed · under-seg)"),
       mpatches.Patch(color=C_FP, label="FP (extra · over-seg)"),
       mpatches.Patch(color=C_BG, label="agreement / non-soybean")]
fig.legend(handles=leg, loc="lower center", ncol=4, frameon=False, bbox_to_anchor=(0.5, 0.0))
plt.tight_layout(rect=[0, 0.03, 1, 1])

out_path = LABELS_DIR / "error_panel.png"
fig.savefig(out_path, dpi=150, bbox_inches="tight")
plt.show()
print("saved:", out_path)

# ====== CELL G+H — fused review figure (A4, LaTeX-ready, memory-safe) ======
import gc
import matplotlib.pyplot as plt
import matplotlib.patches as mpatches
from matplotlib.colors import ListedColormap, BoundaryNorm

# ---- config ---------------------------------------------------------------
PRED_SOYBEAN   = 1        # keep consistent with Cell F
FIG_W          = 6.2      # inches ≈ \textwidth of a standard A4 article
ROW_H          = 1.225     # inches per row
TOP_PAD_IN     = 0.85     # inches between legends and first panel row (+1 cm)
BOT_PAD_IN     = 0.1     # inches below the last panel row
ROWS_PER_PAGE  = 6        # split into several PDFs if more plots than this
MAX_PX         = 600      # display resolution per panel (300 dpi × ~1.7 in)
DPI_PDF, DPI_PNG = 300, 200

CLASS_COLORS = ["#8c6d4a", "#2ecc40", "#ff851b", "#000000"]
CLASS_NAMES  = ["Soil (class 0)", "Soybean (class 1)", "Weed (class 2)", "No data (class 3)"]
cmap = ListedColormap(CLASS_COLORS); norm = BoundaryNorm([-.5,.5,1.5,2.5,3.5], cmap.N)
C_TP, C_FN, C_FP, C_BG = (46,204,64), (0,115,217), (230,38,38), (235,235,235)

# ---- helpers --------------------------------------------------------------
def shrink(a, max_px=MAX_PX):
    """Nearest-neighbour decimation; safe for label masks (no class blending)."""
    k = max(1, int(np.ceil(max(a.shape[0], a.shape[1]) / max_px)))
    return a[::k, ::k] if k > 1 else a

def soy_error_rgb(gt, pr):
    """uint8 error map, 3 bytes/px instead of 24."""
    img = np.empty((*gt.shape, 3), dtype=np.uint8)
    img[:] = C_BG
    v = gt != 3; gs = gt == 1; ps = pr == PRED_SOYBEAN
    img[gs & ps & v] = C_TP; img[gs & ~ps & v] = C_FN; img[~gs & ps & v] = C_FP
    img[~v] = 255
    return img

def soy_scores(gt, pr):
    """Soybean-class IoU and Dice/F1 on valid pixels."""
    v = gt != 3; g = gt[v] == 1; p = pr[v] == PRED_SOYBEAN
    tp = (g & p).sum(); u = (g | p).sum(); s = g.sum() + p.sum()
    iou = np.nan if u == 0 else tp / u
    f1  = np.nan if s == 0 else 2 * tp / s
    return iou, f1

# ---- select plots ---------------------------------------------------------
panel = sorted([S for S in SESSION
                if (LABELS_DIR / f"{S['name']}_label.png").exists()
                and (PREDS_DIR / f"{S['name']}.png").exists()], key=lambda s: s["name"])
assert panel, "no plots have both a label and a prediction — run Cell E first"

pages = [panel[i:i+ROWS_PER_PAGE] for i in range(0, len(panel), ROWS_PER_PAGE)]
cols  = ["RGB", "Manual annotation", "SegFormer prediction", "Soybean error map"]
saved = []

# ---- draw -----------------------------------------------------------------
for pi, page in enumerate(pages, 1):
    n = len(page)
    fig_h = ROW_H * n + TOP_PAD_IN + BOT_PAD_IN
    top    = 1.0 - TOP_PAD_IN / fig_h          # margins fixed in inches, not fractions
    bottom = BOT_PAD_IN / fig_h
    fig, axes = plt.subplots(n, 4, figsize=(FIG_W, fig_h), squeeze=False,
                             gridspec_kw=dict(wspace=0.02, hspace=0.07,
                                              left=0.005, right=0.995,
                                              top=top, bottom=bottom))
    for r, S in enumerate(page):
        gt = np.array(Image.open(LABELS_DIR / f"{S['name']}_label.png"))
        pr = np.array(Image.open(PREDS_DIR  / f"{S['name']}.png"))
        iou, f1 = soy_scores(gt, pr)                # metrics on full resolution
        gt_s, pr_s = shrink(gt), shrink(pr)         # display copies only
        err = soy_error_rgb(gt_s, pr_s)
        del gt, pr

        axes[r,0].imshow(shrink(np.asarray(S["rgb"])))
        axes[r,1].imshow(gt_s, cmap=cmap, norm=norm, interpolation="nearest")
        axes[r,2].imshow(pr_s, cmap=cmap, norm=norm, interpolation="nearest")
        axes[r,3].imshow(err, interpolation="nearest")
        for c in range(4):
            axes[r,c].set_xticks([]); axes[r,c].set_yticks([])
            for sp in axes[r,c].spines.values(): sp.set_linewidth(0.3)
            if r == 0: axes[r,c].set_title(cols[c], fontsize=8, pad=12)

        # row label on top of the row, left-aligned with the first panel
        axes[r,0].text(0.0, 1.02, S["name"], transform=axes[r,0].transAxes,
                       ha="left", va="bottom", fontsize=6)

        axes[r,3].text(0.99, 0.02, f"IoU {iou:.3f}\nF1 {f1:.3f}",
                       transform=axes[r,3].transAxes, ha="right", va="bottom",
                       fontsize=5.5, linespacing=1.15,
                       bbox=dict(fc="white", ec="none", alpha=0.75, pad=0.8))
        del gt_s, pr_s, err; gc.collect()

    h1 = [mpatches.Patch(color=CLASS_COLORS[i], label=CLASS_NAMES[i]) for i in range(3)]
    l1 = fig.legend(handles=h1, loc="upper center", ncol=3, frameon=False, fontsize=7,
                    bbox_to_anchor=(0.5, 1.0), handlelength=1.2, columnspacing=1.2)
    h2 = [mpatches.Patch(color=np.array(C_TP)/255, label="TP (both soybean)"),
          mpatches.Patch(color=np.array(C_FN)/255, label="FN (missed)"),
          mpatches.Patch(color=np.array(C_FP)/255, label="FP (extra)")]

    fig.add_artist(l1)
    fig.legend(handles=h2, loc="upper center", ncol=3, frameon=False,
               fontsize=7, bbox_to_anchor=(0.5, 1.0 - 0.22 / fig_h),
               handlelength=1.2, columnspacing=1.2)

    tag = "" if len(pages) == 1 else f"_p{pi}"
    pdf_path = LABELS_DIR / f"validation_panel{tag}.pdf"
    png_path = LABELS_DIR / f"validation_panel{tag}.png"
    #pdf_path = LABELS_DIR / f"validation_generalized_panel{tag}.pdf"
    #png_path = LABELS_DIR / f"validation_generalized_panel{tag}.png"
    fig.savefig(pdf_path, dpi=DPI_PDF); fig.savefig(png_path, dpi=DPI_PNG)
    plt.close(fig); gc.collect()
    saved += [pdf_path, png_path]

print("saved:", *saved, sep="\n  ")