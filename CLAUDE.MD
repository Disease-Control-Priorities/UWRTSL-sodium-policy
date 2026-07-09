# CLAUDE.md

Guidance for Claude when working on the **Sodium Policy Impact &
Economic Evaluation Platform**.

This repository estimates the national-scale health and economic impact
of dietary sodium-reduction policies via a Comparative Risk Assessment
(CRA), a state-transition (well/sick/dead) mortality model, policy
costing, and an interactive Shiny simulator.

The causal pathway modeled is **Sodium → systolic blood pressure (SBP) →
cardiovascular disease**, evaluated for four causes: ischemic heart
disease (`ihd`), ischemic stroke (`istroke`), intracerebral/hemorrhagic
stroke (`hstroke`), and hypertensive heart disease (`hhd`).

------------------------------------------------------------------------

# Repository Structure

The actual layout differs from a conventional split into
`models/`/`costing/`/`economic/` folders — **those do not exist**. All
analysis code is in `code/`; the Shiny app is in `www/`.

    code/         Numbered R pipeline (see below) — the core model
    data/raw/     Source inputs (GBD, IHME RR xlsx). git-ignored except *.md
    data/processed/  Committed model inputs (.rds/.csv) the pipeline consumes
    data/interim/    Scratch space (currently empty)
    www/          Shiny application (app.R) + its own CSV inputs
    output/out_model/  Per-country model_output_sodium_<country>.rds + logs (git-ignored)
    docs/         R Markdown reports, Beamer slides, math write-up, references.bib
    reports/      report.RMD → report.html
    library/      Literature PDFs (a *reference* library, NOT an R package library)
    tests/        Placeholder only — no automated tests exist

Note: every subdirectory `README.md` is an identical boilerplate copy of
the project overview and carries no directory-specific information —
ignore them and rely on this file.

------------------------------------------------------------------------

# The Pipeline (`code/`)

Run the whole model by sourcing `00_run_model.R`. It sets paths, loads
libraries, defines control flags, and sources `01`–`05` followed by
`06_run_scenarios_targets.R`.

- **`00_run_model.R`** — orchestrator. Sets hard-coded absolute Windows
  paths (`wd`, `wd_raw`, `wd_data`, `wd_outp`), boolean control flags
  (e.g. `run_adjustment_model`, `run_bgmx_trend`, `run_CF_trend`,
  `run_CF_trend_80`), and the `cause_map` abbreviations. **Edit `wd` when
  running on another machine.**
- **`01_utils.R`** — helpers: `get.bp.prob()` (BP-control/normal-based
  distribution shift), TFA mortality-reduction helper, `create_age_groups()`.
- **`02_load_inputs.R`** — heavy data-prep that rebuilds the sodium, TFA,
  statin, and BP-control scenario files. Reads local
  `data/processed/{Sodium,TFAPolicy,Statins}/` and NCD-RisC/GBD raw
  inputs that are **not committed**; it regenerates the committed
  `sodium_policy_scenarios.rds` etc. Downstream runs rely on the already
  committed processed files, so this script is not needed for a standard
  model run.
- **`03_clean_inputs.R`** — empty placeholder.
- **`04_define_intervention.R`** — a self-contained gamma-distribution
  sodium→BP→CVD state-transition implementation (`get_bp_prob_salt`,
  `build_intervention_rates_sodium`, `run_state_transitions_sodium`,
  `run_sodium_model`). Loaded into the environment but the active run
  uses the ETIHAD-based path in `06`, not `run_sodium_model`.
- **`05_build_baseline.R`** — assembles the baseline objects the model
  runs on: `b_rates` (age × sex × year × cause transition rates: `IR`,
  `CF`, `BG.mx`, `Nx`/`pop`, etc.) and `data.in` (BP distribution by
  `bp_cat`). Applies COVID excess mortality, UNWPP-2024 populations, and
  forecasted downward trends in background mortality and case-fatality
  (gated by the `run_*` flags).
- **`06_run_scenarios.R`** — earlier scenario runner, **kept for
  reference only** (not sourced).
- **`06_run_scenarios_targets.R`** — the **current active model**.
  Sodium-only. Loads GBD and ETIHAD relative risks (per 10 mmHg SBP),
  defines the sodium policy/cost tables, and runs
  `calculate_sodium_impact_etihad` → `project.all` → state-transition
  loop → `validate_intervention_results` via `run_multiple_scenarios`.
  Parallelised over countries (`parallel`/`doParallel`/`foreach`, 6
  cores), writing `output/out_model/model_output_sodium_<country>.rds`
  and a per-country log.
- **`07_run_interventions.R`** — a newer **source-split** refactor of
  `06_targets`: splits baseline sodium into five sources (discretionary,
  packaged, restaurant, public, inherent; shares sum to 1) and applies
  each intervention's documented reduction only to the sources it
  affects, composing multiple policies on one source without
  double-counting. Produces the same `out_model` outputs and shares the
  downstream pathway with `06`; the only change is how the scalar
  `salteff` is derived. Run this **instead of** `06_targets` for the
  source-split analysis — it is not sourced by `00_run_model.R`.

Scenario framing: **Progress** (15% sodium reduction by 2030) and
**Aspirational** (30% by 2030), with a scale-up window of ~2026–2030 and
a projection horizon to 2040.

------------------------------------------------------------------------

# Dependencies

No `renv`/`packrat` lockfile — packages are expected to be installed in
the ambient R library. Loaded via `library()`:

- **Model pipeline:** `data.table`, `dplyr`, `tidyr`, `ggplot2`,
  `RColorBrewer`, `readxl`, `countrycode`, `stringr`, `parallel`,
  `doParallel`, `foreach`, `gmodels`, `forecast`.
- **Shiny app (`www/app.R`):** `shiny`, `shinydashboard`, `shinyWidgets`,
  `shinyjs`, `shinycssloaders`, `DT`, `plotly`, `networkD3`,
  `rhandsontable`, `htmlwidgets`, plus `data.table`/`dplyr`/`tidyr`/`ggplot2`.

------------------------------------------------------------------------

# Shiny App (`www/app.R`)

A `shinydashboard` app with tabs: Baseline data, Sodium policies,
Hypertension treatment, Cost inputs, Results. It reads its **own** CSV
inputs from the `www/` working directory (e.g. `base_rates_app2.csv`,
`bp_data.csv`, `baseline_out_app2.csv`, `currencies.csv`) — these are not
committed. Keep model logic reproducible outside the app; the app is a
presentation layer, not the source of truth for results.

------------------------------------------------------------------------

# Conventions

1. **`data.table` is the default.** Use in-place `:=`, `by=` grouping,
   keyed joins; avoid gratuitous `data.frame`/tidyverse conversion.
   Validate row counts after joins — dimensional integrity across
   age × sex × year × cause is critical.
2. **Cause abbreviations** are used throughout: `ihd`, `istroke`,
   `hstroke`, `hhd` (plus `aod`, `all` where relevant). Map to GBD full
   names only at output/labeling time.
3. **Age handling:** GBD 5-year groups are expanded to single-year ages
   20–95; `95` is the terminal group.
4. **Paths are hard-coded absolute Windows paths** in `00_run_model.R`
   (and some are embedded in `02`/`05`). This is a Windows-based project.
5. Keep exposure/epidemiologic modeling separate from costing/economic
   logic. Parameterize scenarios via the config/flag mechanisms already
   present rather than hard-coding new constants inline.
6. Do not rewrite the modeling architecture or introduce abstraction
   layers without an explicit request — prioritize epidemiologic
   correctness, transparent CRA logic, and computational efficiency.

------------------------------------------------------------------------

# Running & Reproducing

- **Full model:** open the project (`uwrtsl-sodium-policy.Rproj`) and
  source `code/00_run_model.R` after fixing `wd`. Outputs land in
  `output/out_model/`.
- **Source-split variant:** source `01`–`05`, then
  `code/07_run_interventions.R`.
- **App:** run `shiny::runApp("www")` (requires the app's CSV inputs).
- **Reports/slides:** knit `docs/*.Rmd` (Beamer slides use
  `beamer_preamble.tex` and cite `references.bib`); `reports/report.RMD`
  renders `report.html`.

There is no build system or test suite; the deliverable is the R script
pipeline plus the app and rendered documents.
