# Soybean Growth Modeling

Non-linear mixed-effects modeling of soybean canopy cover across breeding lines and years, integrating weather covariates to analyze G×E interactions and predict canopy trajectories in untested environments.

**Preprint:** [https://doi.org/10.22541/au.177088529.95951774/v1](https://doi.org/10.22541/au.177088529.95951774/v1)

## Repository structure

```
.
├── data/                     # Raw & processed data (see Data)
├── figures/                  # Example output figures
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
├── identify_row_in-row_space_for_training.R
├── modelling.R
├── visualization.R
└── Weather_imputation.R
```

## What the pipeline does

1. **Transformer-based segmentation** of soybean and weeds for improved canopy cover extraction
2. **Ingest & clean** weather and phenotyping data
3. **Join & feature-build** across site-year-plot
4. **Model** non-linear growth with G×E weather effects
5. **Predict** canopy trajectories (observed & new environments)
6. **Evaluate & visualize** fits and summarize candidates

## Requirements

### R (≥ 4.2 recommended)

Install the packages your scripts call:

```r
install.packages(c(
  "data.table", "nlme", "ggplot2"
))
```

### Python (only if using `segmentation/`)

Create the environment from the provided file:

```bash
conda env create -f segmentation/requirements.yml
conda activate <env-name-from-requirements.yml>
```

## Data

### Raw image data and training set creation (optional)

Data for all years is available from the associated data publication and on GitLab:

- **Data publication:** [https://www.nature.com/articles/s41597-026-06663-z](https://www.nature.com/articles/s41597-026-06663-z)
- **GitLab:** `data/<year>/<site>/<camera>` at [https://gitlab.ethz.ch/crop_phenotyping/fip-soybean-canopycover](https://gitlab.ethz.ch/crop_phenotyping/fip-soybean-canopycover)

Download images into your preferred folder and point the segmentation scripts to that path.

```bash
# 1) Identify rows and in-row space to generate labeled cut-out images of soybean and weeds for training
Rscript identify_row_in-row_space_for_training.R

# 2) Combine UAV and FIP canopy cover data: create soybean_pixels_data.csv from scratch
Rscript data_FIP_UAV.R
```

## Segmentation (optional)

If you want to derive canopy/weed masks from the cut-out images created above:

```bash
# activate the environment created under Requirements > Python, then:

# train or run inference — see script args for options
python segmentation/segement_transformer.py
```

Exported cover metrics can then be used by `data_pipeline_soybeans.R`.

![Validation panel](https://github.com/beat2keller/soybean-growth-modeling/blob/main/segmentation/annotation-validation/labels/validation_panel.png?raw=true)

## Quick start for non-linear mixed modeling

Example output — canopy cover dynamics across all trials:

![Green canopy cover dynamics across all trials](Green_conopy_cover_dynamics_all_trials.png)

Run these with `Rscript` from the repo root, or source them in an R session.

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

# 5) Predict in-sample and across environments
Rscript data_pipeline_predictions.R

# 6) Evaluation & figures
Rscript diagnostics.R
Rscript visualization.R

# 7) Candidate selection
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
