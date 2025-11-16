# Repository Guidelines

## Project Structure & Module Organization
Scripts are grouped by study phase: `A/` covers cognitive feature extraction, `B/` handles activity regularity pipelines, and `C/` contains survival/clinical outcomes utilities. Exploratory notebooks and figures live in `EDA/`, while published tables reside in `table1/` and interim exports in `result/` plus `result_optmise/`. Analysis-ready R scripts used for sensitivity work live in `analyze/`, and quick validation snippets are stored under `just_test。。/`. The `renv/` folder and `renv.lock` pin package versions for reproducible runs.

## Build, Test, and Development Commands
- `R -q -e "renv::restore()"` – install the exact package set declared in `renv.lock` before working on a new machine.
- `Rscript B/B.main.R` – orchestrates the full regularity metric workflow by sourcing the module scripts referenced inside `B/`.
- `Rscript C/C.main.R` – prepares outcomes, merges registries, and triggers survival analysis utilities.
- `Rscript analyze/mice\ cox+regular_dementia.R` – template for running a mice-based model; swap in other scripts within `analyze/` as needed.

## Coding Style & Naming Conventions
Stick to tidyverse-friendly R style—2 spaces for indentation, `<-` for assignment, and snake_case for objects even if filenames carry the `Phase.Module_task.R` prefix. Keep each script executable via `Rscript`, source subordinate helpers near the top, and guard long-running code with functions so downstream analyses can reuse them. Use `dplyr` pipelines instead of nested base-R when possible, and document non-obvious constants with inline comments.

## Testing Guidelines
Small regression checks live in `just_test。。/`; keep adding task-focused scripts there and run them via `Rscript just_test。。/not_covariates.R`. When modeling code changes, reproduce at least one path in `analyze/` and capture key metrics (c-index, HRs, etc.) in the console log. Favor deterministic seeds inside any `mice` or resampling loop, and record dataset filters at the top of each script so reviewers can re-run with the same cohorts.

## Commit & Pull Request Guidelines
History currently shows only an initial import, so adopt conventional concise messages (e.g., `feat: add diurnal regularity scorer`). Reference UK Biobank application IDs or ticket numbers when relevant, describe model or dataset changes in the PR body, link to generated tables or figures, and include screenshots if UI-facing outputs (plots, dashboards) change. Confirm that large CSVs or raw participant data remain untracked by Git before requesting review.
