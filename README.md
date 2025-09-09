# Soybean Growth Modeling

Non-linear modeling of soybean canopy cover across breeding lines and years, integrating weather covariates to analyze G×E and make predictions for untested environments.

## Repository structure

```
.
├── data/                     # Raw & processed data (see Data)
├── functions/                # Reusable R helpers
├── model/                    # Saved non-linear mixed model
├── segmentation/             # (Optional) Python canopy/weed segmentation
├── adjusted_means_ref_traits.R
├── data_FIP_UAV.R
├── data_pipeline_join_df.R
├── data_pipeline_predictions.R
├── data_pipeline_soybeans.R
├── data_pipeline_weather.R
├── diagnostics.R
├── get_ideal_candidates.R
├── modelling.R
└── visualization.R
```

## What the pipeline does

0) **Transformer-based segmention** of soybean and weeds for improved canopy cover extraction
1) **Ingest & clean** weather and phenotyping data  
2) **Join & feature-build** across site-year-plot  
3) **Model** non-linear growth with G×E weather effects  
4) **Predict** canopy trajectories (observed & new environments)  
5) **Evaluate & visualize** fits and summarize candidates

## Requirements

### R (≥ 4.2 recommended)

Install the packages your scripts call:

```r
install.packages(c(
  "data.table","nlme","ggplot2"
))
```

> If you prefer **base R**, the pipeline remains compatible—use base functions where tidyverse verbs appear.

### Python (only if using `segmentation/`)

Create the environment **from the provided file**:

```bash
# using conda or mamba
conda env create -f segmentation/requirements.yml
# then activate the environment name defined inside that file:
conda activate <env-name-from-requirements.yml>
```

## Data

### Raw image data and training set creation (optional)
Public example data for 2021, site SB015, camera RGB1 is available here:

- `data/2021/SB015/RGB1` on GitLab  
  (https://gitlab.ethz.ch/crop_phenotyping/fip-soybean-canopycover)

Download those images into your preferred folder and point the segmentation scripts to that path.

```bash
# 1) Identify rows and in-row space to generate labeled cut-out images of soybean and weeds, respectively for training
Rscript identify_row_in-row_space_for_training.R

# 2) Combine UAV and FIP canopy cover data: create soybean_pixels_data.csv from scratch
Rscript data_FIP_UAV.R

```
## Segmentation (optional)

If you want to derive canopy/weed masks from created cut-out images:

```bash
# 1) Create & activate env from the repo spec
conda env create -f segmentation/requirements.yml
conda activate <env-name-from-requirements.yml>

# 2) Train or run inference (examples; see scripts in ./segmentation)
python segmentation/segement_transformer.py   --train and predict soybean and weed pixels
```

Exported cover metrics can then be used by `data_pipeline_soybeans.R`.


## Quick start for non-linear mixed modeling

You can run these with `Rscript` from the repo root or source them in an R session.

```bash

# 1) Weather: (optional) impute gaps, then standardize features
Rscript Weather_imputation.R
Rscript data_pipeline_weather.R

# 2) Phenotypes: clean canopy cover / plot keys / dates
Rscript data_pipeline_soybeans.R

# 3) Join into a modeling table
Rscript data_pipeline_join_df.R

# 4) Fit non-linear + G×E models (saves model objects in ./model)
Rscript modelling.R

# 6) Predict in-sample and across environments
Rscript data_pipeline_predictions.R

# 7) Evaluation & figures
Rscript diagnostics.R
Rscript visualization.R

# 8) Candidate selection
Rscript get_ideal_candidates.R
```

### Running from an R session

```r
source("data_pipeline_weather.R")
source("data_pipeline_soybeans.R")
source("data_pipeline_join_df.R")
source("modelling.R")
source("data_pipeline_predictions.R")
source("diagnostics.R")
source("visualization.R")
```


