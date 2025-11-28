# Repository Guidelines

## Project Structure & Module Organization

Scripts are organized by workflow phase:

- `01_data_preparation/` - Baseline table construction, data cleaning, and TableOne generation
- `02_feature_engineering/` - Feature extraction pipelines:
  - `A1_*` - Cognitive feature extraction (FAMD composite score)
  - `B0-B7` - Activity regularity metrics:
    - `B0_main.R` - Main entry point
    - `B1-B4` - Variability metrics (diurnal, intensity, weekday) → PCA regularity score
    - `B5-B7` - Classification & clustering (weekly pattern, time pattern, exposure cluster)
  - `C0-C6` - Survival/clinical outcomes preparation:
    - `C0_main.R` - Main entry point
    - `C1-C5` - Dementia diagnosis → endpoint status → survival time
    - `C6` - Competing risk data preparation
- `03_analysis/` - Statistical modeling:
  - `cox_models/` - Cox regression for dementia outcomes
  - `linear_models/` - Linear models for cognitive outcomes
  - `nonlinear_models/` - GAM/RCS for non-linear relationships
- `04_eda/` - Exploratory data analysis and visualization
- `05_tables_figures/` - Result beautification and normality tests
- `06_validation/` - Quick validation and sanity checks

Data and results:
- `data/` - Input data files
- `mid_result/` - Intermediate results
- `result/` - Final analysis outputs
- `renv/` + `renv.lock` - Package version management

## Build, Test, and Development Commands

- `R -q -e "renv::restore()"` – Install exact package versions from `renv.lock`
- `Rscript 02_feature_engineering/B0_main.R` – Run activity regularity pipeline
- `Rscript 02_feature_engineering/C0_main.R` – Prepare survival outcomes
- `Rscript 03_analysis/cox_models/mice_cox_regular_dementia.R` – Example Cox model
- `Rscript 03_analysis/linear_models/mice_lm_mvpa_cognitive.R` – Example linear model

## Coding Style & Naming Conventions

- tidyverse-friendly R style: 2 spaces for indentation, `<-` for assignment
- snake_case for objects
- Script naming: `{Module}{Number}_{description}.R` (e.g., `B1_diurnal_variability.R`)
- Keep scripts executable via `Rscript`
- Source subordinate helpers near the top
- Use `dplyr` pipelines instead of nested base-R
- Document non-obvious constants with inline comments

## Testing Guidelines

- Validation scripts in `06_validation/`
- When modeling code changes, reproduce at least one analysis path
- Capture key metrics (c-index, HRs, etc.) in console output
- Use deterministic seeds in `mice` or resampling loops
- Record dataset filters at script top for reproducibility

## Commit & Pull Request Guidelines

- Conventional commit messages (e.g., `feat: add diurnal regularity scorer`)
- Reference UK Biobank application IDs when relevant
- Describe model/dataset changes in PR body
- Link to generated tables/figures
- Confirm large CSVs/raw data remain untracked before review
