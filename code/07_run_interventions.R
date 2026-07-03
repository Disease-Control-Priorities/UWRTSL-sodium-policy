
# 07_run_interventions.R
#
# Sodium Policy Intervention Model - SOURCE-SPLIT implementation.
#
# This script refactors 06_run_scenarios_targets.R. It produces the same
# per-country out_model .rds outputs and uses the SAME downstream pathway
# (calculate_sodium_impact_etihad -> project.all -> state-transition loop ->
# validate_intervention_results). The ONLY change is UPSTREAM, in how the
# scalar sodium reduction fraction ('salteff') is derived.
#
# Key design change vs. 06:
#   - 06 treated sodium as a single pool and applied one blended 'salteff'
#     fraction to the whole 'salt' column via a policy-efficacy table.
#   - 07 splits baseline sodium into FIVE SOURCES (discretionary, packaged,
#     restaurant, public, inherent; shares sum to 1.0 per country) and applies
#     each intervention's documented relative reduction ONLY to the source(s)
#     it can affect. The per-source reductions are then collapsed back to a
#     single total-intake fraction:
#
#         total_reduction_fraction =
#             sum_over_sources ( source_share_s x composed_effect_s )
#
#     where, for a PACKAGE of interventions hitting the same source,
#         composed_effect_s = 1 - prod_i ( 1 - eff_i,s )
#     so multiple policies on one source (e.g. FOPL + salt targets on
#     'packaged') never double-count or exceed 100%.
#
#   - That fraction is EXACTLY the 'salteff' that calculate_sodium_impact_etihad
#     (saltmet = "percent") already expects, so everything downstream is
#     unchanged. Because efficacy is now country-specific, scenario_configs
#     carry intervention NAMES (not a fixed scalar); run_multiple_scenarios
#     resolves 'salteff' per country via compute_total_efficacy().
#
# Unchanged from 06 (verbatim): Sections 1-2 (GBD/ETIHAD RR), Section 4 (epi
# helpers), Section 6 (calculate_sodium_impact_etihad), Section 7 (baseline
# cleaning), Section 8 (project.all), Section 11 (comparison/validation).
#
# Intended to be sourced after 05_build_baseline.R (like 06), so it relies on
# the globals data.in, b_rates, inc, repYear and the path vars wd_raw, wd_data,
# wd_outp being present in the environment.

library(data.table)
library(readxl)
library(parallel)
library(doParallel)
library(foreach)
library(dplyr)

###############################################################################
# SECTION 0: USER CONTROLS  (which interventions, and how to score them)
###############################################################################

# Which interventions to model. Options:
#   - a single name:        "fopl"
#   - a character vector:    c("fopl", "salt_targets", "fiscal")
#   - the keyword "all":     every defined intervention
#       (public_procurement, media_campaign, fopl, salt_targets, fiscal, lss)
selected_interventions <- "all"

# How to turn the selection into scenarios:
#   "individual" = one scenario per selected intervention   (+ baseline)
#   "combined"   = all selected interventions as ONE package (+ baseline)
#   "both"       = individual scenarios AND a combined package (+ baseline)
scenario_mode <- "both"

# Should placeholder interventions (e.g. LSS, whose effect size is not yet
# finalised) be folded into the combined 'full_package' scenario? Default FALSE
# reproduces the documented package (public + media + fopl + salt_targets +
# fiscal); LSS still runs as its own individual scenario. Set TRUE to include
# placeholders in the package once their effect sizes are confirmed.
include_placeholders_in_package <- FALSE

# Scale-up window (first year of scale-up -> year of full implementation).
SCALEUP_YEAR1 <- 2026
SCALEUP_YEAR2 <- 2030

###############################################################################
# SECTION 1: GBD Relative Risks Setup                       (unchanged from 06)
###############################################################################

dt_gbd_rr <- as.data.table(
  read_excel(
    paste0(wd_raw, "IHME_GBD_2019_RELATIVE_RISKS_Y2020M10D15_HTN.xlsx"),
    sheet = "Sheet1", range = "A3:AB20"
  )
)

dt_gbd_rr[, c("Category / Units", "Morbidity / Mortality", "Sex", "All-age") := NULL]
dt_gbd_rr[, `20-24 years` := `25-29 years`]
dt_gbd_rr[, (2:8) := NULL]

dt_gbd_rr <- melt(
  dt_gbd_rr,
  id.vars       = "Risk-Outcome",
  variable.name = "age",
  value.name    = "rr_per_10mmhg"
)

dt_gbd_rr[, age := gsub(" years", "", age)]
dt_gbd_rr[, rr_per_10mmhg := as.numeric(sub("^\\s*([0-9.]+).*", "\\1", rr_per_10mmhg))]

dt_gbd_rr[, cause := fcase(
  `Risk-Outcome` == "Ischaemic heart disease",     "ihd",
  `Risk-Outcome` == "Ischaemic stroke",            "istroke",
  `Risk-Outcome` == "Intracerebral hemorrhage",    "hstroke",
  `Risk-Outcome` == "Hypertensive heart disease",  "hhd",
  default = NA_character_
)]
dt_gbd_rr[, `Risk-Outcome` := NULL]
dt_gbd_rr <- dt_gbd_rr[cause %in% c("ihd", "istroke", "hstroke", "hhd")]

# Expand GBD age groups to single-year ages
expand_gbd_age <- function(age_group) {
  if (grepl("\\+", age_group)) {
    start <- as.numeric(sub("\\+", "", age_group))
    return(start:95)
  }
  bounds <- as.numeric(unlist(strsplit(age_group, "-")))
  bounds[1]:bounds[2]
}

dt_expanded <- dt_gbd_rr[
  , .(age_single = expand_gbd_age(age)),
  by = .(age, rr_per_10mmhg, cause)
]
dt_expanded[, age := as.integer(age_single)][, age_single := NULL]
dt_gbd_rr <- copy(dt_expanded)
rm(dt_expanded)

###############################################################################
# SECTION 2: ETIHAD Relative Risks Setup                    (unchanged from 06)
###############################################################################

ETIHAD_RR <- fread(paste0(wd_data, "ettehad_rr_bp_reduction_10mmHg.csv"))

ETIHAD_RR[, cause := fcase(
  Cause == "Coronary heart disease", "ihd",
  Cause == "Heart failure",          "hhd",
  Cause == "Stroke",                 "istroke",
  default = NA_character_
)]

ETIHAD_RR <- ETIHAD_RR[
  cause %in% c("ihd", "hhd", "istroke", "hstroke"),
  c("cause", "SBP_Category", "RR"),
  with = FALSE
]

# Carry hstroke RR from istroke
etihad_hstroke_rr       <- ETIHAD_RR[cause == "istroke"]
etihad_hstroke_rr[, cause := "hstroke"]
ETIHAD_RR               <- rbind(ETIHAD_RR, etihad_hstroke_rr)
ETIHAD_RR               <- ETIHAD_RR[SBP_Category != "Total"]
setnames(ETIHAD_RR, c("SBP_Category", "RR"), c("bp_cat", "rr_per_10mmhg"))
rm(etihad_hstroke_rr)

# Map 8-bin BP categories to ETIHAD's coarser bins, then expand back
bp_full <- c("<120", "120-129", "130-139", "140-149",
             "150-159", "160-169", "170-179", "180+")

map_bp <- function(x) {
  fcase(
    x %in% c("<120", "120-129", "<130"),          "<130",
    x == "130-139",                               "130-139",
    x == "140-149",                               "140-149",
    x == "150-159",                               "150-159",
    x %in% c("160-169", "170-179", "180+", ">=160"), ">=160"
  )
}

bp_map <- data.table(bp_cat_full = bp_full, bp_cat = map_bp(bp_full))

expanded <- bp_map[
  ETIHAD_RR, on = .(bp_cat), allow.cartesian = TRUE
][, .(cause, bp_cat_full, rr = rr_per_10mmhg)][order(cause, bp_cat_full)]

ETIHAD_RR <- copy(expanded)
setnames(ETIHAD_RR, c("bp_cat_full", "rr"), c("bp_cat", "rr_per_10mmhg"))
rm(expanded, bp_map)

# Cumulative ETIHAD effect-size table (per BP bin x cause)
ETIHAD_RR_BIN <- as.data.table(
  read_excel(paste0(wd_data, "ettehad_rr_bp_reduction_effects.xlsx"), sheet = "Sheet1")
)

###############################################################################
# SECTION 3: Source-Split Intervention Logic  (REPLACES 06's policy tables)
#
# Replaces default_sodium_policy_table() and summarize_sodium_policy_package()
# with:
#   - documented per-source effect-size constants,
#   - intervention_effects  : one row per (intervention, source) relative
#                             reduction,
#   - build_sodium_source_shares() : per-country source-share table + default,
#   - compute_total_efficacy()     : collapse to a single salteff fraction,
#   - build_scenario_configs()     : build scenario_configs from user controls.
###############################################################################

# >>> BEGIN SOURCE-LOGIC (unit-testable definitions) >>>

# The five sodium sources. Shares sum to 1.0 per country. 'inherent' (sodium
# naturally present in unprocessed foods) is NEVER targeted by any intervention.
SODIUM_SOURCES     <- c("discretionary", "packaged", "restaurant", "public", "inherent")
TARGETABLE_SOURCES <- setdiff(SODIUM_SOURCES, "inherent")

# --- Effect-size constants (relative reduction applied to the target source) --
# Each value is the fractional reduction in that SOURCE's sodium contribution
# when the intervention is fully implemented. Source: RTSL sodium policy
# document intervention effect-size tables.
EFF_PROCUREMENT_PUBLIC   <- 0.20   # Public food procurement & service -> 'public' (20%)
EFF_MEDIA                <- 0.010  # Behaviour change / media campaigns -> 'discretionary' + 'packaged' (1.0%)
EFF_FOPL_PACKAGED        <- 0.138  # Front-of-pack labelling (FOPL) -> 'packaged' (13.8%)
EFF_SALTTARGETS_PACKAGED <- 0.20   # Salt targets (reformulation) -> 'packaged' (20%; conservative floor 15% for sensitivity)
EFF_FISCAL_PACKAGED      <- 0.10   # Fiscal policy / salt tax -> 'packaged' (10%)
EFF_LSS_DISCRETIONARY    <- 0.15   # Low-sodium salt substitutes (LSS) -> 'discretionary'
                                   # (PLACEHOLDER 15% - config hook; finalise when evidence is confirmed)

# --- intervention_effects: one row per (intervention, source) ----------------
# Sources NOT listed for an intervention are treated as 0 reduction. 'inherent'
# must never appear here (asserted below).
intervention_effects <- data.table(
  intervention = c(
    "public_procurement",
    "media_campaign", "media_campaign",
    "fopl",
    "salt_targets",
    "fiscal",
    "lss"
  ),
  source = c(
    "public",
    "discretionary", "packaged",
    "packaged",
    "packaged",
    "packaged",
    "discretionary"
  ),
  effect = c(
    EFF_PROCUREMENT_PUBLIC,
    EFF_MEDIA, EFF_MEDIA,
    EFF_FOPL_PACKAGED,
    EFF_SALTTARGETS_PACKAGED,
    EFF_FISCAL_PACKAGED,
    EFF_LSS_DISCRETIONARY
  )
)

# --- intervention_registry: canonical order, labels, placeholder flags -------
intervention_registry <- data.table(
  intervention = c("public_procurement", "media_campaign", "fopl",
                   "salt_targets", "fiscal", "lss"),
  label        = c("Public food procurement & service",
                   "Behaviour change / media campaigns",
                   "Front-of-pack labelling (FOPL)",
                   "Salt targets (reformulation)",
                   "Fiscal policy (salt tax)",
                   "Low-sodium salt substitutes (LSS)"),
  placeholder  = c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE)
)
DEFINED_INTERVENTIONS <- intervention_registry$intervention

#' Build the per-country sodium source-share table (+ 'default' row) and,
#' optionally, write it to disk as sodium_source_shares.rds in wd_data.
#'
#' Values are the document's country tables (already ~normalised to 100%). Each
#' row is renormalised to sum to exactly 1.0 (handles rounding, e.g. Thailand's
#' documented shares sum to 0.999). Any country not listed here resolves to the
#' 'default' row = simple mean of each source share across the listed countries,
#' then renormalised (CLARIFICATION 3).
#'
#' Country names use the model's canonical location names (matching the name_map
#' in 05_build_baseline.R, e.g. "Viet Nam").
#'
#' @param wd_data Processed-data directory (for writing the .rds). May be NULL
#'                when write = FALSE.
#' @param write   If TRUE, save sodium_source_shares.rds to wd_data.
#' @return data.table: location + the five source-share columns (rows sum to 1).
build_sodium_source_shares <- function(wd_data = NULL, write = TRUE) {
  # Documented per-country source shares (fractions).
  shares_raw <- data.table(
    location      = c("Viet Nam", "Philippines", "Bangladesh", "China",
                      "Ethiopia", "India", "Malaysia", "Thailand"),
    discretionary = c(0.109, 0.320, 0.738, 0.594, 0.783, 0.738, 0.320, 0.235),
    packaged      = c(0.641, 0.430, 0.062, 0.173, 0.017, 0.062, 0.430, 0.141),
    restaurant    = c(0.100, 0.100, 0.050, 0.083, 0.050, 0.050, 0.100, 0.473),
    public        = c(0.050, 0.050, 0.050, 0.050, 0.050, 0.050, 0.050, 0.050),
    inherent      = c(0.100, 0.100, 0.100, 0.100, 0.100, 0.100, 0.100, 0.100)
  )
  # Malaysia uses the Philippines source profile as a proxy (per document).

  # Default row = simple mean of each source across the documented countries.
  default_row <- shares_raw[, lapply(.SD, mean), .SDcols = SODIUM_SOURCES]
  default_row[, location := "default"]
  setcolorder(default_row, c("location", SODIUM_SOURCES))

  shares <- rbind(shares_raw, default_row, use.names = TRUE)

  # Renormalise every row to sum to exactly 1.0.
  rs <- rowSums(shares[, ..SODIUM_SOURCES])
  shares[, (SODIUM_SOURCES) := lapply(.SD, function(x) x / rs), .SDcols = SODIUM_SOURCES]

  if (isTRUE(write)) {
    if (is.null(wd_data)) stop("wd_data must be supplied when write = TRUE")
    saveRDS(shares, file = paste0(wd_data, "sodium_source_shares.rds"))
  }
  shares[]
}

#' Collapse a set of interventions into a single total-intake reduction fraction
#' for one country (the 'salteff' consumed by calculate_sodium_impact_etihad).
#'
#' For a package (multiple interventions), effects on the same source are
#' composed as 1 - prod_i(1 - eff_i,s) so overlapping policies never
#' double-count or exceed 100%. The composed per-source effects are then
#' share-weighted and summed.
#'
#' @param intervention_names  Character vector (may be length 0 = baseline).
#' @param country             Location string (falls back to 'default' row).
#' @param source_shares       Table from build_sodium_source_shares().
#' @param intervention_effects (intervention, source, effect) table.
#' @return Numeric scalar total-intake reduction fraction. A per-source
#'         decomposition data.table is attached as attr(x, "decomposition")
#'         for logging / traceability.
compute_total_efficacy <- function(intervention_names,
                                   country,
                                   source_shares,
                                   intervention_effects) {
  sources <- c("discretionary", "packaged", "restaurant", "public", "inherent")

  # Resolve this country's source-share row (fall back to 'default').
  srow <- source_shares[location == country]
  if (nrow(srow) == 0L) srow <- source_shares[location == "default"]
  if (nrow(srow) == 0L) {
    stop("No source shares for country '", country, "' and no 'default' row.")
  }
  shares_vec <- unlist(srow[1L, ..sources])

  # Baseline (no interventions) -> zero reduction.
  if (is.null(intervention_names) || length(intervention_names) == 0L) {
    dec <- data.table(source = sources,
                      share = shares_vec[sources],
                      composed_effect = 0,
                      contribution = 0)
    total <- 0
    attr(total, "decomposition") <- dec[]
    return(total)
  }

  # Guard against unknown intervention names.
  unknown <- setdiff(intervention_names, unique(intervention_effects$intervention))
  if (length(unknown) > 0L) {
    stop("Unknown intervention(s) in compute_total_efficacy(): ",
         paste(unknown, collapse = ", "))
  }

  eff <- intervention_effects[intervention %in% intervention_names]

  # Per-source composed effect: 1 - prod(1 - eff_i,s).
  composed <- vapply(sources, function(s) {
    e <- eff[source == s, effect]
    if (length(e) == 0L) 0 else 1 - prod(1 - e)
  }, numeric(1))

  dec <- data.table(
    source          = sources,
    share           = shares_vec[sources],
    composed_effect = composed[sources],
    contribution    = shares_vec[sources] * composed[sources]
  )

  total <- sum(dec$contribution)
  attr(total, "decomposition") <- dec[]
  total
}

#' Build the scenario_configs list from the user controls.
#'
#' Each entry carries an 'interventions' character vector (empty = baseline);
#' run_multiple_scenarios() resolves the per-country salteff from it. Always
#' includes 'baseline'. In "individual"/"both" mode, one scenario per selected
#' intervention; in "combined"/"both" mode, a 'full_package' scenario. By
#' default placeholder interventions are excluded from the combined package
#' (see include_placeholders_in_package).
#'
#' @param selected_interventions Character vector or the keyword "all".
#' @param scenario_mode          "individual" | "combined" | "both".
#' @param registry               intervention_registry (defaults to global).
#' @param include_placeholders_in_package Include placeholder interventions in
#'        the combined package (default FALSE).
#' @param saltyear1, saltyear2   Scale-up window stored on each scenario.
#' @return Named list of scenario configs.
build_scenario_configs <- function(selected_interventions,
                                   scenario_mode = c("both", "individual", "combined"),
                                   registry = intervention_registry,
                                   include_placeholders_in_package = FALSE,
                                   saltyear1 = 2026,
                                   saltyear2 = 2030) {
  scenario_mode <- match.arg(scenario_mode)
  defined <- registry$intervention

  # Resolve the "all" keyword.
  if (length(selected_interventions) == 1L && identical(selected_interventions, "all")) {
    selected <- defined
  } else {
    selected <- selected_interventions
  }

  # Validate every requested name.
  unknown <- setdiff(selected, defined)
  if (length(unknown) > 0L) {
    stop("Unknown intervention(s): ", paste(unknown, collapse = ", "),
         ". Defined interventions are: ", paste(defined, collapse = ", "),
         " (or the keyword \"all\").")
  }
  # Keep canonical order and drop duplicates.
  selected <- defined[defined %in% selected]

  configs <- list()

  # Baseline is always present.
  configs$baseline <- list(
    interventions = character(0),
    saltyear1     = saltyear1,
    saltyear2     = saltyear2,
    label         = "Baseline (no intervention)"
  )

  # Individual scenarios.
  if (scenario_mode %in% c("individual", "both")) {
    for (nm in selected) {
      configs[[nm]] <- list(
        interventions = nm,
        saltyear1     = saltyear1,
        saltyear2     = saltyear2,
        label         = registry[intervention == nm, label]
      )
    }
  }

  # Combined package scenario.
  if (scenario_mode %in% c("combined", "both")) {
    pkg <- selected
    if (!isTRUE(include_placeholders_in_package)) {
      pkg <- setdiff(pkg, registry[placeholder == TRUE, intervention])
      pkg <- defined[defined %in% pkg]  # re-impose canonical order
    }
    if (length(pkg) > 0L) {
      configs$full_package <- list(
        interventions = pkg,
        saltyear1     = saltyear1,
        saltyear2     = saltyear2,
        label         = paste0("Full package (", paste(pkg, collapse = " + "), ")")
      )
    }
  }

  configs
}

# <<< END SOURCE-LOGIC (unit-testable definitions) <<<

# --- Build / write / load source shares (mirrors prepare_sodium_data) --------
build_sodium_source_shares(wd_data, write = TRUE)
source_shares <- readRDS(paste0(wd_data, "sodium_source_shares.rds"))

# --- Build scenario_configs from the user controls ---------------------------
scenario_configs <- build_scenario_configs(
  selected_interventions          = selected_interventions,
  scenario_mode                   = scenario_mode,
  registry                        = intervention_registry,
  include_placeholders_in_package = include_placeholders_in_package,
  saltyear1                       = SCALEUP_YEAR1,
  saltyear2                       = SCALEUP_YEAR2
)

# --- Structural validation of the source logic (fail fast) -------------------
# 1. Every country's source shares sum to 1.0 (tolerance 1e-6).
.chk_sums <- source_shares[, rowSums(.SD), .SDcols = SODIUM_SOURCES]
if (any(abs(.chk_sums - 1) > 1e-6)) {
  stop("Source shares do not sum to 1.0 (tol 1e-6) for: ",
       paste(source_shares$location[abs(.chk_sums - 1) > 1e-6], collapse = ", "))
}

# 2. No intervention may reduce the 'inherent' source, and every targeted
#    source must be one of the five defined sources.
if (nrow(intervention_effects[source == "inherent"]) > 0L) {
  stop("Invalid intervention_effects: the 'inherent' source must never be targeted.")
}
.bad_src <- setdiff(unique(intervention_effects$source), SODIUM_SOURCES)
if (length(.bad_src) > 0L) {
  stop("Unknown source(s) in intervention_effects: ", paste(.bad_src, collapse = ", "))
}

# 3. Worked-example sanity check: for a 5%-public-share country (Viet Nam,
#    whose shares sum to exactly 1.0), public_procurement salteff should equal
#    0.20 x 0.05 = 0.01 (matches the document's worked example).
.chk_pub <- as.numeric(compute_total_efficacy("public_procurement", "Viet Nam",
                                              source_shares, intervention_effects))
if (abs(.chk_pub - 0.20 * 0.05) > 1e-6) {
  stop(sprintf("Sanity check failed: public_procurement salteff for Viet Nam = %.6f (expected %.6f)",
               .chk_pub, 0.20 * 0.05))
}
cat(sprintf("\nStructural validation OK. Sanity check: public_procurement salteff (Viet Nam) = %.4f (= 0.20 x 0.05)\n",
            .chk_pub))
rm(.chk_sums, .bad_src, .chk_pub)

# --- Scenario summary + illustrative per-country efficacies -------------------
cat("\nDefined scenarios and their interventions:\n")
for (nm in names(scenario_configs)) {
  ints <- scenario_configs[[nm]]$interventions
  ints_str <- if (length(ints) == 0L) "(none - baseline)" else paste(ints, collapse = " + ")
  cat(sprintf("  %-16s : %s\n", nm, ints_str))
}

cat("\nIllustrative salteff (fraction of TOTAL sodium reduced) - resolved per country at run time:\n")
.ref_countries <- intersect(c("Viet Nam", "Bangladesh", "Thailand", "default"),
                            source_shares$location)
for (nm in names(scenario_configs)) {
  effs <- vapply(.ref_countries, function(cc) {
    as.numeric(compute_total_efficacy(scenario_configs[[nm]]$interventions, cc,
                                      source_shares, intervention_effects))
  }, numeric(1))
  cat(sprintf("  %-16s %s\n", nm,
              paste(sprintf("%s=%.4f", .ref_countries, effs), collapse = "  ")))
}
rm(.ref_countries)

###############################################################################
# SECTION 3b: Retained cost helpers  (used downstream for costing)
###############################################################################

#' Compute baseline, reduction, and target sodium intake from an efficacy fraction.
#'
#' @param baseline_sodium_g Baseline mean sodium intake (grams/day).
#' @param total_efficacy    Fraction of sodium reduced (from compute_total_efficacy).
#' @return data.table with Baseline, Reduced, Target columns.
build_sodium_intake_table <- function(baseline_sodium_g, total_efficacy) {
  reduced <- baseline_sodium_g * total_efficacy
  target  <- baseline_sodium_g - reduced
  data.table(Baseline = baseline_sodium_g, Reduced = reduced, Target = target)
}

#' Compute year-by-year programme costs for a sodium policy.
#'
#' Costs scale linearly from 0 at start_year to full per-capita cost at end_year,
#' then remain flat thereafter.  Population is divided by 4 (quarterly scaling).
#'
#' @param pop_dt        data.table with columns year and Pop.
#' @param per_capita_cost Annual per-capita cost (USD).
#' @param start_year, end_year Scale-up period.
#' @param exchange      Optional exchange rate multiplier (default 1).
#' @return pop_dt with added column saltcosts.
calc_sodium_policy_costs <- function(
    pop_dt,
    per_capita_cost,
    start_year,
    end_year,
    exchange = 1
) {
  pop_dt <- copy(as.data.table(pop_dt))
  pop_dt[, saltcosts := 0]

  pop_dt[
    year >= start_year & year < end_year,
    saltcosts := (Pop / 4) * per_capita_cost * exchange *
      (year - start_year + 1) / (end_year - start_year + 1)
  ]

  pop_dt[
    year >= end_year,
    saltcosts := (Pop / 4) * per_capita_cost * exchange
  ]

  pop_dt[]
}

###############################################################################
# SECTION 4: Epidemiological Helper Functions               (unchanged from 06)
###############################################################################

#' Expand 5-year age groups (e.g., "20-24") to single-year ages.
expand_to_single_year_ages <- function(dt) {
  dt[, age := as.numeric(substr(age, 1, 2))]
  dt <- dt[rep(seq_len(nrow(dt)), each = 5)]
  dt[, age2 := rep(1:5, nrow(dt) / 5)][, age := age + age2 - 1]

  over90 <- dt[age == 89]
  over90 <- over90[rep(seq_len(nrow(over90)), each = 6)]
  over90[, age2 := rep(1:6, nrow(over90) / 6)][, age := age + age2]

  rbindlist(list(dt, over90))[, age2 := NULL]
}

#' Look up GBD RR for a given BP category x age x cause combination.
#'
#' RR is computed relative to the <120 mmHg reference using the GBD
#' log-linear RR per 10 mmHg SBP increase.
get_gbd_relative_risks <- function(bp_cat, age, cause, dt_gbd_rr = NULL) {
  if (is.null(dt_gbd_rr)) {
    dt_gbd_rr <- get("dt_gbd_rr", envir = .GlobalEnv)
  }

  sbp_midpoint <- case_when(
    bp_cat == "<120"    ~ 110,
    bp_cat == "120-129" ~ 125,
    bp_cat == "130-139" ~ 135,
    bp_cat == "140-149" ~ 145,
    bp_cat == "150-159" ~ 155,
    bp_cat == "160-169" ~ 165,
    bp_cat == "170-179" ~ 175,
    bp_cat == "180+"    ~ 185,
    TRUE ~ NA_real_
  )

  inc_10 <- (sbp_midpoint - 120) / 10
  tmp    <- data.table(age = age, cause = cause)
  tmp    <- dt_gbd_rr[tmp, on = c("age", "cause")]
  rr10   <- tmp$rr_per_10mmhg
  ifelse(inc_10 > 0, rr10^inc_10, 1)
}

#' Compute BP-category probabilities from a gamma-like BP distribution.
#'
#' Called with rx = 0 for the sodium model (no antihypertensive treatment
#' shift is applied; covinc is set to 0 unconditionally).
get.bp.prob <- function(DT, rx, drugaroc = "baseline") {
  cov_var <- switch(
    drugaroc,
    "baseline" = "aroc2",
    "p75"      = "p_change2",
    "p975"     = "a_change2",
    "ideal"    = "ideal",
    stop("Invalid 'drugaroc'. Must be one of: baseline, p75, p975, ideal")
  )

  DT[, covinc := if (rx == 1) get(cov_var) else aroc2]
  DT[, covinc := 0]  # sodium model: no treatment coverage increment

  bp_breaks <- c(-Inf, 120, 130, 140, 150, 160, 170, 180, Inf)
  bp_labels <- c("<120", "120-129", "130-139", "140-149",
                 "150-159", "160-169", "170-179", "180+")

  for (i in seq_along(bp_labels)) {
    lower <- bp_breaks[i]
    upper <- bp_breaks[i + 1]
    DT[bp_cat == bp_labels[i],
       prob := pnorm(upper, Mean, stdev) - pnorm(lower, Mean, stdev)]
  }

  DT[, .(age, sex, Year, bp_cat, prob, location)]
}

#' Compute bin-specific baseline incidence rates using GBD RRs.
calculate_baseline_incidence_gbd <- function(bp_prob, intervention_rates,
                                             Country, dt_gbd_rr) {
  cat("  - Calculating baseline incidence with GBD RRs\n")

  bp_prob <- expand_to_single_year_ages(bp_prob)

  causes <- c("ihd", "hhd", "istroke", "hstroke", "aod")
  for (cause in causes) {
    col_name <- paste0("RRi_", toupper(cause))
    bp_prob[, (col_name) := get_gbd_relative_risks(bp_cat, age, cause, dt_gbd_rr)]
  }

  alphas <- bp_prob[, .(
    ihd    = sum(prob * RRi_IHD),
    istroke = sum(prob * RRi_ISTROKE),
    hstroke = sum(prob * RRi_HSTROKE),
    hhd    = sum(prob * RRi_HHD),
    aod    = sum(prob * RRi_AOD)
  ), by = .(age, sex, location, Year)]

  alphas <- melt(alphas, id.vars = c("age", "sex", "location", "Year"),
                 variable.name = "cause", value.name = "alpha")

  rris <- bp_prob[, .(age, sex, Year, location, bp_cat, prob,
                      RRi_IHD, RRi_HHD, RRi_ISTROKE, RRi_AOD)]
  rris[, RRi_HSTROKE := RRi_ISTROKE]
  setnames(rris,
           c("RRi_IHD", "RRi_HHD", "RRi_ISTROKE", "RRi_HSTROKE", "RRi_AOD"),
           c("ihd", "hhd", "istroke", "hstroke", "aod"))

  rris <- melt(rris, id.vars = c("age", "sex", "location", "bp_cat", "prob", "Year"),
               variable.name = "cause", value.name = "RRi")

  bp_prob_full <- merge(rris, alphas,
                        by = c("age", "sex", "location", "cause", "Year"))
  setnames(bp_prob_full, "Year", "year")

  dt <- merge(intervention_rates[location == Country], bp_prob_full,
              by = c("age", "sex", "location", "cause", "year"))

  dt[, IR_bin := (RRi * IR) / alpha]
  return(dt)
}

#' Compute diabetes-weighted cumulative ETIHAD effect size for a BP bin x cause.
calculate_etihad_cumulative_rr <- function(bp_cat, cause_name,
                                           diabetes_weight = 0.1,
                                           etihad_rr_table = ETIHAD_RR_BIN) {
  if (length(bp_cat) != length(cause_name)) {
    stop("bp_cat and cause_name must have the same length")
  }

  lookup_key <- paste(cause_name, bp_cat, sep = "_")
  table_key  <- paste(etihad_rr_table$cause, etihad_rr_table$bp_cat, sep = "_")
  idx        <- match(lookup_key, table_key)

  if (any(is.na(idx))) {
    stop("Some bp_cat-cause combinations were not found in ETIHAD_RR_BIN")
  }

  effect_no_diab <- etihad_rr_table$effect_size_nodiabetes[idx]
  effect_diab    <- etihad_rr_table$effect_size_diabetes[idx]

  (1 - diabetes_weight) * effect_no_diab + diabetes_weight * effect_diab
}

###############################################################################
# SECTION 5: Sodium Data Preparation                        (unchanged from 06)
###############################################################################

#' Merge country-specific sodium intake data into the BP distribution table.
#'
#' Reads the pre-built sodium_policy_scenarios.rds, extracts 2024 baseline
#' intakes, and attaches them to data.in as the 'salt' column.
prepare_sodium_data <- function(data.in, wd_data) {
  dt_sodium_scenarios <- readRDS(file = paste0(wd_data, "sodium_policy_scenarios.rds"))
  dt_sodium_scenarios <- dt_sodium_scenarios[year == 2024, .(location, sodium_current)]

  data.in <- merge(data.in, dt_sodium_scenarios, by = "location", all.x = TRUE)
  data.in[!is.na(sodium_current), salt := sodium_current]
  data.in[, sodium_current := NULL]

  return(data.in)
}

data.in <- prepare_sodium_data(data.in, wd_data)

###############################################################################
# SECTION 6: Core Sodium Intervention Model                 (unchanged from 06)
###############################################################################

#' Compute intervention-modified incidence rates from a sodium reduction.
#'
#' Uses the Filippini dose-response (2.8 mmHg per g for raised-BP individuals,
#' 1.0 mmHg for normal-BP) and ETIHAD per-10-mmHg RRs to derive IR_new.
#' Case fatality is held at baseline (no secondary CF effect from sodium).
#'
#' @param intervention_rates Baseline rates data.table (from b_rates).
#' @param Country            Location string matching intervention_rates$location.
#' @param DT.in              BP distribution data.table (from data.in, expanded
#'                           over years via repYear).
#' @param salteff            Fraction of sodium reduced (from
#'                           compute_total_efficacy()).
#' @param saltmet            Reduction method: "percent" | "target" | "app".
#' @param saltyear1          First year of scale-up.
#' @param saltyear2          Year of full implementation.
#' @param dt_gbd_rr          GBD RR table (defaults to global).
#' @return data.table with modified IR, CF, and effect ratios eff_ir / eff_cf.
calculate_sodium_impact_etihad <- function(
    intervention_rates,
    Country,
    DT.in,
    salteff,
    saltmet,
    saltyear1 = 2026,
    saltyear2 = 2030,
    dt_gbd_rr
) {
  cat(" - Calculating sodium impact using ETIHAD effect sizes\n")

  # Step 1: Baseline BP distribution (no intervention)
  bp_prob_base <- get.bp.prob(DT.in, rx = 0, drugaroc = "baseline")

  # Step 2: Bin-specific baseline incidence
  dt_baseline <- calculate_baseline_incidence_gbd(
    copy(bp_prob_base), intervention_rates, Country, dt_gbd_rr
  )

  # Step 3: Salt reduction amount per method
  salt_info <- unique(DT.in[, .(age, sex, salt, raisedBP, Year, aroc)])
  setnames(salt_info, "Year", "year")

  expand_age_group <- function(x) {
    if (x == "85plus") return(85:95)
    bounds <- as.numeric(unlist(strsplit(x, "-")))
    seq(bounds[1], bounds[2])
  }

  dt_exp <- salt_info[, .(age_single = expand_age_group(age)),
                      by = .(age, sex, salt, raisedBP, aroc, year)]
  dt_exp <- dt_exp[, .(age = age_single, sex, salt, raisedBP, aroc, year)]

  dt_baseline <- merge(dt_baseline, dt_exp, by = c("age", "sex", "year"), all.x = TRUE)

  # Target reduction in grams
  if (saltmet == "percent") {
    dt_baseline[, salt_target := salt * salteff]
  } else if (saltmet == "target") {
    dt_baseline[, salt_target := pmin(salt, salteff)]
  } else if (saltmet == "app") {
    dt_baseline[, salt_target := pmax(0, salt - salteff)]
  }

  # Enforce minimum intake of 2 g/day
  dt_baseline[, salt_target := ifelse(salt - salt_target < 2, salt - 2, salt_target)]

  # Step 4: Progressive linear scale-up
  dt_baseline[year >= saltyear1 & year <= saltyear2,
              salt_reduction := salt_target * (year - saltyear1 + 1) /
                (saltyear2 - saltyear1 + 1)]
  dt_baseline[year > saltyear2, salt_reduction := salt_target]
  dt_baseline[year < saltyear1, salt_reduction := 0]
  dt_baseline[is.na(salt_reduction) | salt_reduction < 0, salt_reduction := 0]

  # Step 5: Filippini dose-response -> SBP reduction
  dt_baseline[, sbp_reduction := ((2.8 * raisedBP) + ((1 - raisedBP) * 1.0)) * salt_reduction]

  # Step 6: ETIHAD RRs per BP bin x cause
  dt_baseline <- merge(dt_baseline, ETIHAD_RR, by = c("bp_cat", "cause"), all.x = TRUE)

  # Diabetes-weighted ETIHAD cumulative effects
  etihad_effects <- dt_baseline[, .(N = mean(pop)),
                                by = .(location, year, age, sex, bp_cat, cause)]
  diabetes_prop  <- expand_to_single_year_ages(DT.in)
  diabetes_prop  <- diabetes_prop[, c("location", "Year", "age", "sex", "bp_cat", "diabetes"),
                                  with = FALSE]
  setnames(diabetes_prop, "Year", "year")

  etihad_effects <- merge(etihad_effects, diabetes_prop, all.x = TRUE)
  etihad_effects[, etihad_effect := calculate_etihad_cumulative_rr(
    bp_cat, cause, diabetes_weight = diabetes)]
  etihad_effects[, c("diabetes", "N") := NULL]

  dt_baseline <- merge(dt_baseline, etihad_effects,
                       by = c("location", "year", "age", "sex", "bp_cat", "cause"),
                       all.x = TRUE)

  # Step 7: Effect on incidence (proportional to SBP reduction)
  dt_baseline[, etihad_effect        := (1 - rr_per_10mmhg)]
  dt_baseline[, etihad_effect_sodium := etihad_effect * 0.1 * sbp_reduction]
  dt_baseline[, IR_bin_new           := IR_bin * (1 - etihad_effect_sodium)]

  # Step 8: Population-weighted average incidence
  dt_baseline[, IR_new := sum(IR_bin_new * prob),
              by = .(age, sex, location, cause, year)]
  dt_baseline[year < saltyear1, IR_new := IR]
  dt_baseline[, eff_ir := IR_new / IR]

  # Step 9: Case fatality - no secondary effect from sodium reduction
  dt_baseline[, CF_new := CF]
  dt_baseline[, eff_cf := 1]

  # Step 10: Collapse BP-bin dimension
  dt_final <- unique(dt_baseline[, .(
    age, sex, location, cause, year,
    IR = IR_new, CF = CF_new,
    BG.mx, BG.mx.all, PREVt0, DIS.mx.t0, Nx, ALL.mx,
    eff_ir, eff_cf
  )])

  setorder(dt_final, year, sex, location, cause, age)

  cat("  - Sodium impact applied (method:", saltmet,
      "| salteff:", salteff,
      "| years:", saltyear1, "-", saltyear2, ")\n")

  return(dt_final)
}

###############################################################################
# SECTION 7: Baseline Rate Cleaning                         (unchanged from 06)
###############################################################################

b_rates[CF >= 1, CF := 0.99]
b_rates[IR >= 1, IR := 0.99]
b_rates[CF < 0,  CF := 0]
b_rates[IR < 0,  IR := 0]

###############################################################################
# SECTION 8: project.all()  - sodium-only projection        (unchanged from 06)
###############################################################################

#' Run the sodium intervention model for a single country.
#'
#' @param Country   Location string.
#' @param saltmet   Sodium reduction method passed to calculate_sodium_impact_etihad.
#' @param salteff   Fraction of baseline sodium reduced.  Use
#'                  compute_total_efficacy() to obtain this per country from a
#'                  set of interventions.
#' @param saltyear1 First year of scale-up (default 2026).
#' @param saltyear2 Year of full implementation (default 2030).
#' @return data.table with state-transition outputs: age, cause, sex, year,
#'         well, sick, newcases, dead, pop, all.mx, intervention, location,
#'         eff_ir, eff_cf.
project.all <- function(
    Country,
    saltmet   = "percent",
    salteff   = 0.0,
    saltyear1 = 2026,
    saltyear2 = 2030
) {
  cat("\n========================================\n")
  cat("STARTING PROJECTION FOR:", Country, "\n")
  cat("salteff  =", salteff, " | method:", saltmet, "\n")
  cat("scale-up:", saltyear1, "-", saltyear2, "\n")
  cat("========================================\n\n")

  #--------------------------------------------------------------------
  # Preliminaries: subset and expand input data
  #--------------------------------------------------------------------
  base_rates <- b_rates[location == Country & year >= 2017]

  DT <- unique(data.in[location == Country][, Year := 2017][, -c("Lower95", "Upper95")])
  DT.in <- as.data.table(
    left_join(
      DT[rep(seq(1, nrow(DT)), 34)][, Year := repYear(.I)],
      inc %>% select(-location),
      by = c("iso3", "Year")
    )
  )

  # Force AROC-related variables to zero (not used in sodium model)
  DT.in[, c("aroc", "aroc2", "p_change", "p_change2",
             "a_change", "a_change2", "ideal", "drugaroc") := 0]

  #--------------------------------------------------------------------
  # Initialise intervention rates from baseline
  #--------------------------------------------------------------------
  intervention_rates <- copy(base_rates)
  intervention_rates[, `:=`(eff_ir = 1, eff_cf = 1)]

  #--------------------------------------------------------------------
  # Apply sodium intervention (if salteff > 0)
  #--------------------------------------------------------------------
  if (salteff > 0) {
    cat("\n=== Applying Sodium Intervention ===\n")

    DT.in.sodium <- copy(DT.in)

    intervention_rates_sodium <- calculate_sodium_impact_etihad(
      intervention_rates,
      Country,
      DT.in.sodium,
      salteff,
      saltmet,
      saltyear1,
      saltyear2,
      dt_gbd_rr
    )

    # Extract effect ratios and modified rates; merge back into baseline
    # structure to preserve all columns (including covid.mx).
    eff_cols <- intervention_rates_sodium[, .(
      age, sex, location, cause, year,
      eff_ir_salt = eff_ir, eff_cf_salt = eff_cf,
      IR_new = IR, CF_new = CF
    )]

    intervention_rates <- merge(
      intervention_rates, eff_cols,
      by = c("age", "sex", "location", "cause", "year"),
      all.x = TRUE
    )

    intervention_rates[!is.na(eff_ir_salt), `:=`(
      eff_ir = eff_ir_salt, eff_cf = eff_cf_salt,
      IR = IR_new, CF = CF_new
    )]
    intervention_rates[is.na(eff_ir_salt), `:=`(eff_ir = 1, eff_cf = 1)]
    intervention_rates[, c("eff_ir_salt", "eff_cf_salt", "IR_new", "CF_new") := NULL]

    # NOTE: 'intervention' is a binary ARM flag only (Baseline vs any sodium
    # reduction). It is intentionally NOT scenario-specific. The authoritative
    # scenario identity is the 'scenario' column added by run_multiple_scenarios()
    # via rbindlist(idcol = "scenario"). Downstream summaries must key off
    # 'scenario', not 'intervention'.
    intervention_rates[, intervention := "Sodium reduction"]
  } else {
    intervention_rates[, intervention := "Baseline"]
  }

  #--------------------------------------------------------------------
  # Initialise population states
  #--------------------------------------------------------------------
  cat("\n=== Setting Initial Population States ===\n")

  intervention_rates[year == 2017 | age == 20, `:=`(
    sick   = Nx * PREVt0,
    dead   = Nx * DIS.mx.t0,
    well   = Nx * (1 - (PREVt0 + BG.mx)),
    pop    = Nx,
    all.mx = Nx * DIS.mx.t0 + Nx * BG.mx
  )]

  intervention_rates[CF > 0.99, CF := 0.99]
  intervention_rates[IR > 0.99, IR := 0.99]

  setorder(intervention_rates, sex, location, cause, age)

  #--------------------------------------------------------------------
  # State transitions  (2017 -> 2058, 41 steps)
  #--------------------------------------------------------------------
  cat("\n=== Running State Transition Model ===\n")
  cat("Projecting from 2017 to 2058...\n")

  for (i in 1:41) {
    if (i %% 10 == 0) cat("  Year", 2017 + i, "\n")

    b2 <- intervention_rates[year <= 2017 + i & year >= 2017 + i - 1]
    b2[, age2 := age + 1]

    b2[, newcases2 := shift(well) * IR,
       by = .(sex, location, cause, age, intervention)]

    b2[, sick2 := shift(sick) * (1 - (CF + BG.mx + covid.mx)) + shift(well) * IR,
       by = .(sex, location, cause, age, intervention)]
    b2[sick2 < 0, sick2 := 0]

    b2[, dead2 := shift(sick) * CF,
       by = .(sex, location, cause, age, intervention)]
    b2[dead2 < 0, dead2 := 0]

    b2[, pop2 := shift(pop) - shift(all.mx),
       by = .(sex, location, cause, age, intervention)]
    b2[pop2 < 0, pop2 := 0]

    b2[, all.mx2 := sum(dead2),
       by = .(sex, location, year, age, intervention)]
    b2[, all.mx2 := all.mx2 + (pop2 * BG.mx.all) + (pop2 * covid.mx)]
    b2[all.mx2 < 0, all.mx2 := 0]

    b2[, well2 := pop2 - all.mx2 - sick2]
    b2[well2 < 0, well2 := 0]

    b2 <- b2[
      year == 2017 + i & age2 < 96,
      .(age2, newcases2, sick2, dead2, well2, pop2, all.mx2,
        sex, location, cause, intervention)
    ]
    setnames(b2, "age2", "age")

    intervention_rates[year == 2017 + i & age > 20, `:=`(
      newcases = b2$newcases2,
      sick     = b2$sick2,
      dead     = b2$dead2,
      well     = b2$well2,
      pop      = b2$pop2,
      all.mx   = b2$all.mx2
    )]
  }

  cat("\n=== Projection Complete ===\n\n")

  intervention_rates[, .(
    age, cause, sex, year, well, sick, newcases,
    dead, pop, all.mx, intervention, location, eff_ir, eff_cf
  )]
}

###############################################################################
# SECTION 9: run_multiple_scenarios()  - resolves salteff PER COUNTRY
#
# Minimal edit vs 06: each scenario config now carries an 'interventions'
# character vector (empty = baseline) instead of a fixed scalar 'salteff'.
# Efficacy is resolved per country via compute_total_efficacy() using the
# (global) source_shares and intervention_effects tables, then passed to
# project.all() exactly as before.
###############################################################################

#' Run project.all() for multiple sodium scenarios for one country.
#'
#' @param Country          Location string.
#' @param scenario_configs Named list. Each element is a named list with:
#'   \describe{
#'     \item{interventions}{Character vector of intervention names (required;
#'       length 0 => baseline).}
#'     \item{saltmet}{Reduction method (optional; falls back to \code{saltmet}).}
#'     \item{saltyear1}{Scale-up start year (optional; falls back to \code{saltyear1}).}
#'     \item{saltyear2}{Full-implementation year (optional; falls back to \code{saltyear2}).}
#'     \item{label}{Human-readable label (optional; logging only).}
#'   }
#' @param saltmet   Default reduction method.
#' @param saltyear1 Default scale-up start year.
#' @param saltyear2 Default full-implementation year.
#' @param source_shares        Per-country source-share table (defaults to global).
#' @param intervention_effects (intervention, source, effect) table (defaults to global).
#' @return Combined data.table with an added 'scenario' column.
run_multiple_scenarios <- function(
    Country,
    scenario_configs,
    saltmet   = "percent",
    saltyear1 = 2026,
    saltyear2 = 2030,
    source_shares        = get("source_shares",        envir = .GlobalEnv),
    intervention_effects = get("intervention_effects", envir = .GlobalEnv)
) {
  results <- list()

  for (scenario_name in names(scenario_configs)) {
    cfg <- scenario_configs[[scenario_name]]

    s_interventions <- if (!is.null(cfg$interventions)) cfg$interventions else character(0)
    s_saltmet       <- if (!is.null(cfg$saltmet))       cfg$saltmet       else saltmet
    s_saltyear1     <- if (!is.null(cfg$saltyear1))     cfg$saltyear1     else saltyear1
    s_saltyear2     <- if (!is.null(cfg$saltyear2))     cfg$saltyear2     else saltyear2
    s_label         <- if (!is.null(cfg$label))         cfg$label         else scenario_name

    # Resolve the country-specific total-intake reduction fraction from the
    # per-source shares and this scenario's interventions.
    s_salteff <- compute_total_efficacy(s_interventions, Country,
                                        source_shares, intervention_effects)
    s_decomp  <- attr(s_salteff, "decomposition")

    cat("\n##########################################\n")
    cat("SCENARIO     :", scenario_name, "\n")
    cat("Label        :", s_label, "\n")
    cat("Country      :", Country, "\n")
    cat("Interventions:",
        if (length(s_interventions) == 0L) "(none - baseline)"
        else paste(s_interventions, collapse = " + "), "\n")
    cat(sprintf("salteff      = %.6f\n", as.numeric(s_salteff)))
    cat("Source decomposition (share x composed_effect = contribution):\n")
    print(s_decomp)
    cat("##########################################\n")

    results[[scenario_name]] <- project.all(
      Country   = Country,
      saltmet   = s_saltmet,
      salteff   = as.numeric(s_salteff),
      saltyear1 = s_saltyear1,
      saltyear2 = s_saltyear2
    )
  }

  rbindlist(results, idcol = "scenario")
}

# NOTE: 06's SECTION 10 (static scenario configurations) is intentionally gone:
# scenario_configs is now built dynamically from the user controls in SECTION 3
# (build_scenario_configs), and salteff is resolved per country in SECTION 9.

###############################################################################
# SECTION 11: Comparison and Validation Helpers             (unchanged from 06)
###############################################################################

#' Compare a scalar outcome across scenarios at selected years.
#'
#' The 'scenario' column (from run_multiple_scenarios) is the authoritative key.
#' 'intervention' is a binary arm flag and is carried through for information
#' only; it is deliberately NOT part of the reference join, since every non-
#' baseline scenario shares the same intervention label ("Sodium reduction").
compare_scenarios <- function(results_dt,
                              metric             = "dead",
                              years              = c(2030, 2040, 2050),
                              reference_scenario = "baseline") {
  comparison <- results_dt[year %in% years,
                           .(total       = sum(get(metric)),
                             intervention = first(intervention)),
                           by = .(scenario, year)]

  if (reference_scenario %in% comparison$scenario) {
    ref_values <- comparison[scenario == reference_scenario,
                             .(year, ref_total = total)]
    comparison <- merge(comparison, ref_values,
                        by = "year", all.x = TRUE)
    comparison[, `:=`(
      absolute_difference = total - ref_total,
      percent_change      = (total - ref_total) / ref_total * 100,
      averted             = ref_total - total
    )]
  }

  setorder(comparison, year, scenario)
  comparison
}

#' Cumulative impact over a time window relative to baseline.
#'
#' Keyed on 'scenario' (authoritative). 'intervention' is carried as an
#' informational label only.
calculate_cumulative_impact <- function(results_dt,
                                        metric     = "dead",
                                        start_year = 2026,
                                        end_year   = 2050) {
  cumulative <- results_dt[year >= start_year & year <= end_year,
                           .(cumulative_total = sum(get(metric)),
                             intervention     = first(intervention)),
                           by = .(scenario)]

  baseline_val <- cumulative[scenario == "baseline", cumulative_total]
  cumulative[, diff_vs_baseline     := abs(cumulative_total - baseline_val)]
  cumulative[, diff_pct_vs_baseline := abs(100 * (cumulative_total - baseline_val) / baseline_val)]

  setorder(cumulative, scenario)
  cumulative
}

#' Basic sanity checks on model output.
validate_intervention_results <- function(results_dt) {
  issues <- list()

  neg_cols <- c("well", "sick", "dead", "pop", "newcases")
  for (col in neg_cols) {
    if (results_dt[, any(get(col) < 0, na.rm = TRUE)]) {
      issues[[paste0("negative_", col)]] <-
        results_dt[get(col) < 0, .(scenario, year, age, sex, cause, value = get(col))]
    }
  }

  na_cols <- c("eff_ir", "eff_cf", "dead", "newcases")
  for (col in na_cols) {
    if (results_dt[, any(is.na(get(col)))]) {
      issues[[paste0("na_", col)]] <-
        results_dt[is.na(get(col)), .(scenario, year, age, sex, cause)]
    }
  }

  pop_check <- results_dt[, .(
    total_population = sum(well + sick, na.rm = TRUE),
    recorded_pop     = sum(pop,         na.rm = TRUE)
  ), by = .(scenario, year)]
  pop_check[, diff := abs(total_population - recorded_pop)]
  if (pop_check[, any(diff > 0.01 * recorded_pop)]) {
    issues[["population_mismatch"]] <- pop_check[diff > 0.01 * recorded_pop]
  }

  if (results_dt[, any(eff_ir < 0 | eff_ir > 2, na.rm = TRUE)]) {
    issues[["eff_ir_out_of_bounds"]] <-
      results_dt[eff_ir < 0 | eff_ir > 2, .(scenario, year, age, cause, eff_ir)]
  }

  if (results_dt[, any(eff_cf < 0 | eff_cf > 2, na.rm = TRUE)]) {
    issues[["eff_cf_out_of_bounds"]] <-
      results_dt[eff_cf < 0 | eff_cf > 2, .(scenario, year, age, cause, eff_cf)]
  }

  validation_result <- list(
    passed   = length(issues) == 0,
    n_issues = length(issues),
    issues   = issues
  )

  if (validation_result$passed) {
    cat("\n OK All validation checks passed!\n")
  } else {
    cat("\n Not OK Validation found", length(issues), "issue(s):\n")
    print(names(issues))
  }

  validation_result
}

###############################################################################
# SECTION 12: Parallel Execution  - all scenarios x all countries
###############################################################################

# --- Cluster parameters ------------------------------------------------------
ncores <- 6
cl     <- makeCluster(ncores)
registerDoParallel(cl)

# Export all objects required by workers.
#   Dropped vs 06 : default_sodium_policy_table, summarize_sodium_policy_package
#   Added   vs 06 : compute_total_efficacy, source_shares, intervention_effects
clusterExport(
  cl,
  varlist = c(
    # Core functions
    "project.all",
    "run_multiple_scenarios",
    # Epidemiological helpers
    "get.bp.prob",
    "get_gbd_relative_risks",
    "expand_to_single_year_ages",
    "calculate_baseline_incidence_gbd",
    "calculate_etihad_cumulative_rr",
    "calculate_sodium_impact_etihad",
    # Source-split intervention logic (NEW)
    "compute_total_efficacy",
    "source_shares",
    "intervention_effects",
    # Cost helpers (retained)
    "build_sodium_intake_table",
    "calc_sodium_policy_costs",
    # Utility
    "repYear",
    # Data objects
    "data.in",
    "b_rates",
    "inc",
    "dt_gbd_rr",
    "ETIHAD_RR",
    "ETIHAD_RR_BIN",
    # Scenario configuration
    "scenario_configs",
    # Output path
    "wd_outp"
  ),
  envir = globalenv()
)

clusterEvalQ(cl, {
  library(data.table)
  library(dplyr)
})

# --- Country list ------------------------------------------------------------
locs <- unique(data.in$location)
locs <- locs[!locs %in% c("Greenland", "Bermuda")]

# --- Parallel loop -----------------------------------------------------------
time_start <- Sys.time()

results_list <- foreach(
  country        = locs,
  .packages      = c("data.table", "dplyr"),
  .errorhandling = "pass",
  .verbose       = TRUE
) %dopar% {

  log_file <- file.path(
    wd_outp, "out_model",
    paste0("log_interventions_", country, ".txt")
  )
  sink(log_file, split = FALSE)

  cat("\n==============================\n")
  cat("Country:", country, "\n")
  cat("Time   :", as.character(Sys.time()), "\n")
  cat("==============================\n")

  res <- tryCatch({
    run_multiple_scenarios(
      Country          = country,
      scenario_configs = scenario_configs,
      saltmet          = "percent",
      saltyear1        = 2026,
      saltyear2        = 2030
    )
  }, error = function(e) {
    cat("ERROR in", country, ":", e$message, "\n")
    return(NULL)
  })

  if (!is.null(res)) {
    output_file <- file.path(
      wd_outp, "out_model",
      paste0("model_output_interventions_", country, ".rds")
    )
    saveRDS(res, file = output_file)
    cat("Saved:", output_file, "\n")
  } else {
    cat("No results to save for", country, "\n")
  }

  sink()
  res
}

time_end <- Sys.time()
cat("Total runtime:",
    round(difftime(time_end, time_start, units = "mins"), 1),
    "minutes\n")

stopCluster(cl)

# --- Completion summary ------------------------------------------------------
successful <- sapply(results_list, function(x) !is.null(x) && nrow(x) > 0)
cat("\nSuccessful runs:", sum(successful), "out of", length(locs), "\n")
if (any(!successful)) {
  cat("Failed countries:", paste(locs[!successful], collapse = ", "), "\n")
}

###############################################################################
# SECTION 13: Workspace Cleanup
###############################################################################

rm(list = Filter(
  function(x) is.data.table(get(x)) || is.data.frame(get(x)),
  ls()
))
rm(list = intersect(
  c("locs", "i", "time_start", "time_end", "successful", "results_list"),
  ls()
))
