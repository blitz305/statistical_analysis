# UKB Statistical Analysis Toolkit ✨

Welcome aboard! This toolkit packages the end-to-end statistical workflows built on UK Biobank accelerometry and registry data. It walks you through cognitive feature processing (Phase A), activity regularity & exposure clustering (Phase B), plus survival/clinical outcome modeling (Phase C). Alongside the main pipelines it ships a reproducible environment, quick validation scripts, and code for manuscript-ready tables.

## IMPORTANCE
It is not advised to copy and use it directly , as the reproducibility was not considered at the beginning!!!

## Repository Layout 🗂️
- `cognitive_features/` (formerly `A/`): cleans and summarizes raw cognitive tests and screening fields. Use `A.cognitive_result.R` as the entry point to kick off Phase A.
- `activity_regularity/` (formerly `B/`): segments accelerometer features, performs clustering, and computes regularity scores. Run `B.main.R` to orchestrate the module scripts in this directory.
- `survival_outcomes/` (formerly `C/`): prepares death and dementia outcomes, resolves conflicts, and derives survival time. Use `C.main.R` to execute Phase C.
- `analyze/`: houses MICE + cox/gam/lm specifications. Scripts follow the "model+exposure/outcome" naming scheme and run via `Rscript analyze/<script>.R`.
- `quick_checks/`: lightweight validation or exploratory scripts (for example `regular_score+demential_cox.R`). Use commands like `Rscript quick_checks/not_covariates.R` for rapid inspection.
- `EDA/`: exploratory figures and ad-hoc analyses.
- `table1/`: scripts that generate publication-ready tables.
- `paths.R`: centralized definitions of data I/O paths, seeds, and cache locations; every new script should reuse these constants.
- `renv/` and `renv.lock`: `renv`-managed dependency snapshot.
- `UKb.Rproj`: opens the repo as an RStudio project.

## Environment Setup ⚙️
1. **Install dependencies**: run `R -q -e "renv::restore()"` in the repo root to sync the package versions stored in `renv.lock`.
2. **Configure data paths**: edit `paths.R` to match your filesystem so every phase can find its inputs and outputs.
3. **Match the R version**: use the version recorded by `renv` (typically 4.x) to avoid untested runtime differences.

## Workflow Overview 🚀
### 1. Phase A – Cognitive Features 🧠
```bash
Rscript cognitive_features/A.cognitive_result.R
```
- Aggregates and cleans cognitive assessment results.
- Produces intermediate tables consumed by the activity-regularity and survival pipelines.

### 2. Phase B – Activity Regularity 🔄
```bash
Rscript activity_regularity/B.main.R
```
`B.main.R` sequentially sources:
- `B.classify_time_of_day.R` / `B.classify.R` for time-of-day segmentation and activity context.
- `B.clusterize_exposures.R` for exposure pattern aggregation.
- `B.diurnal_regularity.R`, `B.intensity_consistency.R`, `B.regularity_socre.R`, `B.Temporal Regularity.R` for regularity scores and intensity consistency metrics.
- The final tables are written to the destinations defined in `paths.R`.

### 3. Phase C – Survival & Outcomes ❤️‍🔥
```bash
Rscript survival_outcomes/C.main.R
```
- Scripts such as `C.outcome_variables.R`, `C.death_register.R`, and `C.dementia_classify.R` build outcome variables and follow-up times.
- `C.conflict.R` and `C.competing_risk_preparation.R` handle conflict checks and competing-risk structures.

### 4. Analysis & Modeling 📊
- Choose a script under `analyze/`, for example:
  - `Rscript analyze/mice\ cox+regular_dementia.R` to evaluate how regularity scores relate to dementia.
  - `Rscript analyze/mice_lm\ +mvpa_cognitive.R` to probe the linear relation between MVPA and cognition.
- Set seeds and cohort filters at the top of each script to keep runs reproducible.

### 5. Quick Validation & QA 🕵️‍♀️
- Keep one-off validation logic in `quick_checks/` so the main workflow stays clean.
- Example: `Rscript quick_checks/regular_score+demential_cox.R`.
- Before submitting changes, rerun at least one `analyze/` path (or the matching quick check) and capture key metrics (c-index, HR, etc.) from the console output.

## Contribution Guidelines 🤝
- Follow tidyverse style (2-space indent, `<-` assignment, snake_case names).
- Ensure any new data outputs do **not** add sensitive CSVs/raw data to Git.
- Commit example: `feat: add diurnal regularity scorer`. Describe model/data changes in the PR body and attach screenshots whenever UI/figure outputs change.

## FAQ 💡
- **Missing packages or version mismatches**: confirm `renv::restore()` has completed; if needed, remove `renv/library` and rerun the restore.
- **Scripts cannot find data**: check the directories defined in `paths.R` and verify read/write permissions.
- **Long-running modules**: wrap expensive logic in functions and call them from `B.main.R` or `C.main.R` so downstream analyses can reuse them.
Keep README updates in this structure (but feel free to keep the ✨ energy!) when documenting new directories or phases so collaborators can onboard quickly.
