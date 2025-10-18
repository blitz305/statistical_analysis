# Statistical Analysis of Physical Activity Regularity and Cognitive Outcomes

## Overview
This repository contains an R-based analysis workflow for studying how accelerometer-derived physical activity regularity relates to cognitive performance and dementia outcomes in the UK Biobank cohort. The codebase focuses on preparing harmonized datasets from raw biobank extracts, deriving exposure metrics from 24-hour activity traces, constructing cognitive composite scores, and fitting survival models that incorporate multiple imputation for missing data.

## Repository structure
| Path | Purpose |
| --- | --- |
| `paths.R` | Defines helper functions that create `mid_result/` and `result/` output folders used across the project. |
| `A/` | Builds baseline and follow-up cognitive outcomes, performs FAMD-based dimension reduction, and saves scored datasets. |
| `B/` | Computes accelerometer regularity features (intensity consistency, diurnal regularity, temporal patterns) and orchestrates their execution. |
| `C/` | Classifies dementia diagnoses and deaths, merges registry sources, and derives time-to-event endpoints for survival analyses. |
| `analyze/` | Runs multiply-imputed Cox and linear models to evaluate associations between regularity metrics and dementia or cognitive outcomes. |
| `EDA/` | Provides exploratory data analysis scripts for summarizing hourly MVPA distributions and time-of-day profiles. |
| `table1/` | Generates descriptive tables and figures for reporting. |
| `renv/`, `renv.lock`, `UKb.Rproj` | Define the reproducible R environment pinned to the versions captured in the lockfile. |

## Required source data
The scripts expect that cleaned UK Biobank extracts are loaded into the R session before execution. Key data frames referenced throughout the workflow include:
- `Derived_accelerometry` containing overall and hourly MVPA summaries.
- `regularity_score` and other accelerometer-derived metrics.
- Cognitive assessment tables such as `Fluid_intelligence`, `prospective_memory`, `Reaction_time`, and `Pairs_matching`.
- Hospital episode statistics (e.g., `Summary_diagnoses`) and mortality linkage tables (`Death_register`, `data_lost_to_follow-up`).

Because these datasets originate from restricted UK Biobank resources, they are not bundled with the repository. Modify the scripts or add import helpers as needed to load the data objects into memory.

## Environment setup
1. Install R (version 4.5.1 is recommended, matching `renv.lock`).
2. Open the project in R (e.g., via `UKb.Rproj`).
3. Install the `renv` package if it is not available: `install.packages("renv")`.
4. Restore the project-specific library: `renv::restore()`.

These steps recreate the package versions used when the analyses were last run.

## Typical workflow
1. Source `paths.R` to ensure the intermediate and final output directories exist.
2. Run the cognitive scoring pipeline in `A/A.cognitive_result.R` to create baseline and follow-up composite scores saved to `mid_result/`.
3. Execute the activity regularity orchestration script `B/B.main.R`, which sources the individual feature builders under `B/`.
4. Build the analytic cohort and time-to-event outcomes by running `C/C.main.R`, which sequentially sources the dementia classification and survival time modules.
5. Use the scripts under `analyze/` (e.g., `mice cox+regular_dementia.R`) to perform multiply imputed Cox regression or linear models linking the exposures and outcomes of interest.
6. Optionally, explore additional summaries with the notebooks in `EDA/` and construct publication tables with the helpers in `table1/`.

## Outputs
- Intermediate artifacts are written to `mid_result/` (for example, `cognitive_scores.rds`).
- Final model-ready tables and exports should be saved into `result/` by the analysis scripts.

Both directories are created automatically when `paths.R` is sourced.

## Reproducibility notes
- Many scripts rely on `source()`-ing sibling files; run them from the project root or set the working directory accordingly.
- The multiple imputation workflow uses the `mice` package and can be computationally intensive. Adjust the number of imputations (`m`) or iterations (`maxit`) in the analysis scripts if runtime is a concern.
- Sensitive data paths and identifiers have been omitted. Ensure compliance with UK Biobank data handling policies when adapting the code to new environments.
