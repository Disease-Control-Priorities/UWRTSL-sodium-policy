
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
# NOTE: media_campaign (behaviour change / mass media) is intentionally EXCLUDED
# from all reported/production scenarios (July 14 decision). The machinery is
# RETAINED in intervention_registry / intervention_effects (EFF_MEDIA) so it can
# be re-enabled later; it is simply not listed here, and Task 3g keeps it (and
# any placeholder) out of full_package.
selected_interventions <- c(
  "public_procurement",
  "fopl",
  "salt_targets",
  "fiscal",
  "lss"                # LSS variants (scenarios 2/4/5) are expanded in build_scenario_configs (Task 3)
)

# How to turn the selection into scenarios:
#   "individual" = one scenario per selected intervention   (+ baseline)
#   "combined"   = all selected interventions as ONE package (+ baseline)
#   "both"       = individual scenarios AND a combined package (+ baseline)
scenario_mode <- "both"

# Should placeholder interventions (e.g. LSS, whose subgroup + trial-RR
# implementation is not yet validation-hardened) be folded into the combined
# 'full_package' scenario? FALSE (Task 3g) keeps LSS (placeholder = TRUE) OUT of
# full_package: the LSS scenarios 2/4/5 run SEPARATELY until the subgroup work
# has passed validation. The documented package is then public_procurement +
# fopl + salt_targets + fiscal (media is excluded per Task 1). Set TRUE only once
# LSS is ready to compose inside a package without the trial-RR/sodium->SBP
# composition caveat (see calculate_sodium_impact_etihad).
include_placeholders_in_package <- FALSE

# Scale-up window (first year of scale-up -> year of full implementation).
SCALEUP_YEAR1 <- 2026
SCALEUP_YEAR2 <- 2030

# LSS coverage. Scenario 2 (whole-population) coverage is a manual toggle. The
# HTN variants (s4/s5) NO LONGER use the hardcoded HEARTS placeholders below:
# Task 1d replaces them with NCD-RisC country x sex POPULATION eligibility
# (diagnosed_pop / treated_pop), resolved per country in run_multiple_scenarios.
LSS_COVERAGE_ALL  <- 0.50   # whole-population uptake for scenario 2 (RTSL to confirm)
# Scenario-2 (whole-population) coverage/uptake sweep requested by RTSL: run the
# whole-population LSS scenario at each of these POPULATION-reach levels, each a
# distinct scenario lss_s2_<pct> (lss_s2_10 ... lss_s2_50). NB these are
# population-level reach/coverage variants -- they are NOT the product uptake
# among reached users, the adherence fraction, or the KCl substitution ratio
# (those remain LSS_UPTAKE / LSS_ADHERENCE / LSS_KCL_FRACTION and are unchanged;
# see the na_k_sbp composition below). LSS_COVERAGE_ALL is retained as the single
# default coverage used anywhere one representative s2 value is needed.
LSS_S2_COVERAGE_LEVELS <- c(0.10, 0.20, 0.30, 0.40, 0.50)
# DEPRECATED (Task 1d): retained only for reference / a manual override. The
# s4/s5 coverage now comes from htn_eligibility, NOT these constants.
HTN_DIAGNOSED_COV <- 0.33   # [deprecated] old HEARTS diagnosed placeholder
HTN_TREATED_COV   <- 0.25   # [deprecated] old HEARTS treated placeholder

# Task 5: this model is ADULTS-ONLY, but the documented public food-procurement
# share (0.05 flat, in build_sodium_source_shares) assumed school coverage.
# With ADULT_POPULATION_ONLY = TRUE the run FAILS LOUDLY (guard after
# source_shares is loaded) rather than silently using the stale school-inclusive
# value. Set FALSE ONLY for an explicitly-caveated provisional run until RTSL
# delivers the adults-only per-country shares.
# >>> TEMPORARY / PROVISIONAL (approved 2026-07-22 for the July-23 status run):
# FALSE so the run proceeds on the stale 5% school-inclusive share WITH a loud
# caveat printed to every run log. REVERT TO TRUE as soon as RTSL delivers the
# adults-only per-country public shares (insert them in
# build_sodium_source_shares()). Until then, public_procurement & full_package
# results are PROVISIONAL. <<<
ADULT_POPULATION_ONLY <- FALSE

# Analysis / impact years (single source; consumed by run_config + report, so the
# report never re-guesses 2017-2050). SCALEUP_YEAR1/2 above are the scale-up ramp.
IMPACT_YEARS         <- c(2030, 2040, 2050)  # years results are reported at
ANALYSIS_YEAR_MIN    <- 2017                 # first model year
PROJECTION_YEAR_MAX  <- 2050                 # last reported year
HTN_SOURCE_YEAR      <- get0("HTN_SOURCE_YEAR", ifnotfound = 2019)  # NCD-RisC year (Task 1a)

###############################################################################
# SECTION 0b: LSS method + Na/K->SBP mechanistic parameters (Task 1)----
###############################################################################
# LSS_METHOD selects the PRIMARY pathway for the LSS scenarios (s2/s4/s5):
#   "na_k_sbp"       (default/primary) mechanistic sodium + potassium -> SBP,
#                    following the Huang et al. 2026 LSS structure and using the
#                    Filippini 2020 Figure 2 CHANGE-in-urinary-K K->SBP dose-
#                    response with the Figure S16 baseline urinary-K subgroup
#                    modifier (potassium submethod
#                    "filippini_fig2_delta_uk_baseline_uk_modifier").
#                    BENEFIT-ONLY: this disease model has NO CKD/hyperkalaemia
#                    channel, so LSS harms are NOT modelled (unlike Huang) -- this
#                    is recorded in lss_audit and flagged in report limitations.
#   "ssass_trial_rr" benchmark: SSaSS stroke trial RRs (Neal et al., NEJM 2021).
#                    NOT valid for the whole population (scenario 2); the guard in
#                    build_scenario_configs() stop()s on ssass_trial_rr + reach
#                    "all" and directs the user to na_k_sbp for s2.
# These EXACT strings are written into the scenario registry (run_config) so the
# report can label "LSS-...(Na/K-SBP)" vs "LSS-SSaSS trial-RR benchmark".
LSS_METHOD <- "na_k_sbp"
stopifnot(LSS_METHOD %in% c("na_k_sbp", "ssass_trial_rr"))

# Also emit the SSaSS trial-RR BENCHMARK as DISTINCT scenarios (lss_s4_ssass,
# lss_s5_ssass) alongside the primary na_k_sbp s4/s5, so the report can compare
# the two pathways for SSaSS-comparable (diagnosed / treated HTN) populations.
# Never emitted for s2. Ignored if LSS_METHOD is already "ssass_trial_rr".
LSS_BENCHMARK_SSASS <- TRUE

# LSS composition (SSaSS 75% NaCl / 25% KCl by mass). Switching discretionary
# table salt to LSS cuts discretionary SODIUM by the KCl fraction and adds KCl.
LSS_NACL_FRACTION <- 0.75
LSS_KCL_FRACTION  <- 0.25

# Uptake (adoption among the reached/eligible) and adherence (fraction of a
# user's salt that is actually LSS). SSaSS yr-5 reported use ~= 0.92 -> adherence
# default. uptake defaults to 1.0 so it does NOT double-count the NCD-RisC
# population reach (lss_coverage); lower it only for a separate adoption discount.
LSS_UPTAKE    <- 1.00
LSS_ADHERENCE <- 0.92

# Additivity of the sodium- and potassium-mediated SBP changes (Task 1b(iv)).
LSS_NAK_ADDITIVE      <- TRUE   # TRUE = combine Na- and K-dSBP additively; FALSE = Na only
LSS_ADDITIVITY_FACTOR <- 1.00   # Huang "100% -> 80%" sensitivity is a one-line change here

# UPDATED vs previous implementation (baseline-potassium heterogeneity, 2026-08):
# the previous primary K->SBP pathway used a Filippini FIGURE 3 achieved-excretion
# U-curve DIFFERENCED at baseline vs post-LSS excretion, then scaled by baseline-
# SODIUM bands (LSS_K_NA_MODULATION) and attenuated in normotensives
# (LSS_K_NONHTN_FACTOR). All three of those modifiers are RETIRED from the primary
# pathway. The K effect is now the Filippini FIGURE 2 CHANGE-IN-URINARY-K dose-
# response, modified ONLY by the Filippini FIGURE S16 baseline urinary-K subgroup
# (<75 vs >=75 mmol/day). Stacking the retired modifiers on top would introduce
# multiplicative interactions Filippini never jointly estimated (double-counting),
# so LSS_K_NONHTN_*, LSS_K_NA_MODULATION and the Fig-3 curve are deleted, not
# merely disabled. run_config records previous_modifiers_active = FALSE.

# Unit conversions (Filippini 2020 conventions; both exposed for RTSL to swap).
# The 1.3 intake:excretion factor is applied ONLY to convert a DIETARY potassium
# value (g/day -- the Reddin baseline intake and the KCl-added potassium) into 24h
# urinary excretion. It must NEVER be applied to a value that is already urinary.
K_MG_PER_MMOL         <- 39.1  # potassium molar mass (mg per mmol); 1200 mg ~= 30 mmol
K_INTAKE_TO_EXCRETION <- 1.3   # dietary K intake ~= 1.3 x 24h urinary excretion
                               # => urinary excretion (mmol/d) = intake_mmol / 1.3
# Stoichiometry for K added when NaCl mass is replaced 1:1 by KCl mass in the LSS.
NA_PER_G_NACL <- 22.99 / 58.44  # g sodium per g NaCl   (~0.393)
K_PER_G_KCL   <- 39.10 / 74.55  # g potassium per g KCl (~0.524)
# => per gram of sodium removed, K added (g) = NA_removed * (K_PER_G_KCL/NA_PER_G_NACL) ~= 1.33 g

# Filippini (2020) FIGURE 2: CHANGE in 24h urinary potassium (mmol/day) ->
# treated-minus-control SBP difference (mmHg). NEGATIVE sbp_change = SBP LOWERING.
# These are the PUBLISHED anchors (no artificial 80 mmol point); a SWAPPABLE named
# table. Piecewise-linear interpolation implies an approximate zero-crossing at
#   60 + 30 * (2.0 / (2.0 + 1.1)) ~= 79.4 mmol/day
# (increasing benefit to ~30 mmol/day, diminishing thereafter, and a possible
# ADVERSE SBP response above ~79-80 mmol/day). High-dose estimates are imprecise;
# the primary analysis does NOT truncate the adverse arm to zero -- it is flagged.
LSS_K_DELTA_SBP_ANCHORS <- data.table(
  delta_uk_mmol   = c(0,  30,   60,  90,  120),
  sbp_change_mmhg = c(0, -3.3, -2.0, 1.1, 4.2)
)
LSS_K_DELTA_UK_ZERO_CROSSING <- 60 + 30 * (2.0 / (2.0 + 1.1))  # ~= 79.4 mmol/day

# Filippini (2020) FIGURE S16: pooled treated-minus-control SBP by BASELINE 24h
# urinary potassium subgroup. The Figure 2 curve is the population-AVERAGE
# relationship, so the subgroup effects are NORMALISED to the overall estimate to
# give a multiplier m_K(uK0) that preserves that average curve:
#   uK0 <  75 mmol/day (LOWER baseline K):  4.31 / 3.90 = 1.1051 (LARGER effect)
#   uK0 >= 75 mmol/day (HIGHER baseline K): 3.21 / 3.90 = 0.8231 (SMALLER, NOT zero)
# 75 mmol/day urinary ~= 2.93 g/day urinary ~= 3.81 g/day DIETARY (x1.3).
LSS_K_BASELINE_UK_THRESHOLD <- 75   # mmol/day 24h urinary K subgroup split (Fig S16)
LSS_K_SUBGROUP_SBP <- c(low = -4.31, high = -3.21, overall = -3.90)  # mmHg (Fig S16)
LSS_K_BASELINE_MULT_LOW  <- LSS_K_SUBGROUP_SBP[["low"]]  / LSS_K_SUBGROUP_SBP[["overall"]]  # 1.1051
LSS_K_BASELINE_MULT_HIGH <- LSS_K_SUBGROUP_SBP[["high"]] / LSS_K_SUBGROUP_SBP[["overall"]]  # 0.8231

###############################################################################
# SECTION 0c: Fiscal + Euromonitor controls (Tasks 2 & 3)----
###############################################################################
# Fiscal policy is EXPLORATORY/PROVISIONAL. Whether it sits inside full_package
# is a config value (Task 2c) the report reads from run_config -- it never
# assumes. Current behaviour = fiscal IS in the package.
include_fiscal_in_package <- TRUE

# Euromonitor packaged-share trend (Task 3). The trend is a change in source
# COMPOSITION (packaged share grows, others renormalise; TOTAL sodium unchanged)
# unless EUROMONITOR_COMPOSITION_ONLY is set FALSE (not implemented as intake
# growth here -> kept TRUE). WINDOW/RECOMBINE are also read by 03 via get0.
run_packaged_trend                <- get0("run_packaged_trend", ifnotfound = TRUE)
EUROMONITOR_WINDOW                <- get0("EUROMONITOR_WINDOW", ifnotfound = "predictions")
EUROMONITOR_RECOMBINE_FROM_LEAVES <- get0("EUROMONITOR_RECOMBINE_FROM_LEAVES", ifnotfound = FALSE)
EUROMONITOR_BASE_YEAR             <- 2025          # base year for (1+g)^(year-base)
EUROMONITOR_PACKAGED_CEILING      <- 0.80          # cap on packaged share (fraction of sodium sources)
EUROMONITOR_POST2030              <- get0("PACKAGED_TREND_AFTER_2030", ifnotfound = "hold_constant")
                                                   # "hold_constant" | "continue_trend" | "converge_to_ceiling"
EUROMONITOR_COMPOSITION_ONLY      <- TRUE           # TRUE = reweight sources, total intake unchanged
stopifnot(EUROMONITOR_POST2030 %in% c("hold_constant", "continue_trend", "converge_to_ceiling"))

###############################################################################
# SECTION 1: GBD Relative Risks Setup                       (unchanged from 06)----
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
# SECTION 2: ETIHAD Relative Risks Setup                    (unchanged from 06)----
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
# SECTION 3: Source-Split Intervention Logic  (REPLACES 06's policy tables)----
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
# Fiscal policy / salt tax -> 'packaged' (Task 2; EXPLORATORY/PROVISIONAL) -----
# The single opaque multiplier is REPLACED by an explicit, still-simple pathway:
#   tax_rate -> pass_through -> effective consumer price change
#            -> elasticity-based quantity response
#            -> substitution adjustment
#            -> change in TAXABLE packaged sodium
#            -> change in TOTAL packaged sodium (x taxable_packaged_share)
# Base case anchored on Saxena et al. 2025 (Philippines): a 20% tax produced a
# ~10% relative reduction in TAXED packaged sodium by ~yr 20, consumer (quantity)
# response dominant, reformulation minor. CAVEAT (peer review): applying broad
# food-category price elasticities to HIGH-SODIUM foods specifically is uncertain.
# taxable_packaged_share (<1, configurable) is the fraction of packaged sodium in
# taxed nutrient-profile categories -- we do NOT assume all packaged sodium is
# taxed, and we do NOT imply "10% tax => 10% sodium reduction". All results are
# labelled exploratory.
#
# compute_fiscal_packaged_effect(): map one parameter set to the fractional
# reduction in TOTAL packaged sodium. Substitution ENTERS as the fraction of the
# consumer response that shifts to other (untaxed) packaged foods, damping the
# net reduction (documented modelling choice).
compute_fiscal_packaged_effect <- function(tax_rate, pass_through, elasticity,
                                            taxable_packaged_share, substitution,
                                            reformulation = 0) {
  price_change    <- tax_rate * pass_through                 # effective % price rise
  quantity_change <- elasticity * price_change               # own-price demand response (<=0)
  consumer_red    <- -quantity_change * (1 - substitution)   # net consumption drop (>=0)
  taxed_red       <- max(0, min(1, consumer_red + reformulation))  # reduction in TAXED packaged sodium
  taxed_red * taxable_packaged_share                         # -> reduction in TOTAL packaged sodium
}

# Base-case parameters (Saxena 2025-anchored). elasticity is NEGATIVE (own-price).
FISCAL_TAX_RATE      <- 0.20    # 20% tax (Saxena)
FISCAL_PASS_THROUGH  <- 1.00    # full pass-through to consumer price
FISCAL_ELASTICITY    <- -0.50   # own-price elasticity of taxed packaged foods (broad-category proxy)
FISCAL_TAXABLE_SHARE <- 0.50    # fraction of packaged sodium in taxed categories (<1; NOT "all packaged")
FISCAL_SUBSTITUTION  <- 0.00    # fraction of the response substituting to other packaged foods
FISCAL_REFORMULATION <- 0.00    # extra reduction from reformulation of taxed foods (minor per Saxena)
# Base case: 0.20*1.0 = 20% price; *|-0.5| = 10% quantity drop in TAXED packaged
# sodium; *0.50 taxable share = 5% reduction in TOTAL packaged sodium. Echoed to
# the run log and written to fiscal_audit; NOT stated as a confirmed result.
EFF_FISCAL_PACKAGED <- compute_fiscal_packaged_effect(
  FISCAL_TAX_RATE, FISCAL_PASS_THROUGH, FISCAL_ELASTICITY,
  FISCAL_TAXABLE_SHARE, FISCAL_SUBSTITUTION, FISCAL_REFORMULATION)

# Sensitivity set (Task 2b): named alternative parameter combinations, emitted as
# fiscal_low / fiscal_base / fiscal_high scenarios in build_scenario_configs().
# Set to NULL to behave EXACTLY as a single 'fiscal' scenario (base case).
FISCAL_SENSITIVITY <- list(
  fiscal_low  = list(tax_rate = 0.10, pass_through = 0.80, elasticity = -0.30,
                     taxable_packaged_share = 0.40, substitution = 0.30, reformulation = 0.00),
  fiscal_base = list(tax_rate = FISCAL_TAX_RATE, pass_through = FISCAL_PASS_THROUGH,
                     elasticity = FISCAL_ELASTICITY, taxable_packaged_share = FISCAL_TAXABLE_SHARE,
                     substitution = FISCAL_SUBSTITUTION, reformulation = FISCAL_REFORMULATION),
  fiscal_high = list(tax_rate = 0.20, pass_through = 1.00, elasticity = -0.80,
                     taxable_packaged_share = 0.60, substitution = 0.00, reformulation = 0.05)
)

# --- Low-sodium salt substitutes (LSS) -> 'discretionary' (Task 1) ------------
# PRIMARY na_k_sbp method: the per-discretionary-user SODIUM displacement fraction
# is mechanistic -- switching table salt to a 75/25 NaCl/KCl substitute cuts
# discretionary sodium by the KCl fraction, scaled by uptake x adherence. The
# POTASSIUM benefit is added separately inside calculate_sodium_impact_etihad().
EFF_LSS_NA_K_DISCRETIONARY <- LSS_KCL_FRACTION * LSS_ADHERENCE * LSS_UPTAKE   # ~0.23
# RETAINED (labelled) simplified sodium-ONLY sensitivity value (lss_method
# "sodium_sbp"): SSaSS sodium-arm reconstruction, -382 mg/user / ~2554 mg
# discretionary ~= 0.15. NOT the base case; used only if the sensitivity path is run.
EFF_LSS_SODIUM_ONLY <- 0.15
# Active LSS discretionary sodium-displacement fraction that sizes salteff (Na
# channel). For ssass_trial_rr the salteff value only needs to be > 0 to gate the
# pathway -- the stroke effect comes from the trial RRs, not from salteff.
EFF_LSS_DISCRETIONARY <- EFF_LSS_NA_K_DISCRETIONARY

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

# Task 2b: add one (intervention, source) row per fiscal SENSITIVITY variant so
# compute_total_efficacy() can resolve each variant's salteff. Each variant is a
# distinct 'intervention' name (fiscal_low/base/high) targeting 'packaged'. The
# base 'fiscal' row above (= EFF_FISCAL_PACKAGED) is what full_package uses.
if (!is.null(FISCAL_SENSITIVITY)) {
  .fiscal_var_eff <- rbindlist(lapply(names(FISCAL_SENSITIVITY), function(nm) {
    p <- FISCAL_SENSITIVITY[[nm]]
    data.table(intervention = nm, source = "packaged",
               effect = do.call(compute_fiscal_packaged_effect, p))
  }))
  intervention_effects <- rbind(intervention_effects, .fiscal_var_eff)
  rm(.fiscal_var_eff)
}

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
# build_sodium_source_shares <- function(wd_data = NULL, write = TRUE) {
#   # Documented per-country source shares (fractions).
#   shares_raw <- data.table(
#     location      = c("Viet Nam", "Philippines", "Bangladesh", "China",
#                       "Ethiopia", "India", "Malaysia", "Thailand",
#                       "Nigeria", "Cameroon"),
#     discretionary = c(0.109, 0.320, 0.738, 0.594, 0.783, 0.738, 0.320, 0.235,
#                       0.231, 0.231),
#     packaged      = c(0.641, 0.430, 0.062, 0.173, 0.017, 0.062, 0.430, 0.141,
#                       0.415, 0.415),
#     restaurant    = c(0.100, 0.100, 0.050, 0.083, 0.050, 0.050, 0.100, 0.473,
#                       0.205, 0.205),
#     public        = c(0.050, 0.050, 0.050, 0.050, 0.050, 0.050, 0.050, 0.050,
#                       0.050, 0.050),
#     inherent      = c(0.100, 0.100, 0.100, 0.100, 0.100, 0.100, 0.100, 0.100,
#                       0.100, 0.100)
#   )
#   # Malaysia uses the Philippines source profile as a proxy (per document).
#   
#   # Default row = simple mean of each source across the documented countries.
#   default_row <- shares_raw[, lapply(.SD, mean), .SDcols = SODIUM_SOURCES]
#   default_row[, location := "default"]
#   setcolorder(default_row, c("location", SODIUM_SOURCES))
#   
#   shares <- rbind(shares_raw, default_row, use.names = TRUE)
#   
#   # Renormalise every row to sum to exactly 1.0.
#   rs <- rowSums(shares[, ..SODIUM_SOURCES])
#   shares[, (SODIUM_SOURCES) := lapply(.SD, function(x) x / rs), .SDcols = SODIUM_SOURCES]
#   
#   if (isTRUE(write)) {
#     if (is.null(wd_data)) stop("wd_data must be supplied when write = TRUE")
#     saveRDS(shares, file = paste0(wd_data, "sodium_source_shares.rds"))
#   }
#   shares[]
# }

build_sodium_source_shares <- function(wd_data = NULL, write = TRUE) {
  # Updated per-country sodium source shares from the summary table.
  # Nigeria and Cameroon retain their previous values.
  shares_raw <- data.table(
    location = c(
      "Viet Nam", "Philippines", "Bangladesh", "China",
      "Ethiopia", "India", "Malaysia", "Thailand",
      "Nigeria", "Cameroon"
    ),
    discretionary = c(
      0.277, 0.331, 0.769, 0.622,
      0.818, 0.769, 0.331, 0.244,
      0.231, 0.231
    ),
    packaged = c(
      0.166, 0.445, 0.065, 0.181,
      0.018, 0.065, 0.445, 0.146,
      0.415, 0.415
    ),
    restaurant = c(
      0.557, 0.104, 0.046, 0.077,
      0.044, 0.046, 0.104, 0.490,
      0.205, 0.205
    ),
    public = c(
      0.020, 0.020, 0.020, 0.020,
      0.020, 0.020, 0.020, 0.020,
      0.050, 0.050
    ),
    inherent = c(
      0.100, 0.100, 0.100, 0.100,
      0.100, 0.100, 0.100, 0.100,
      0.100, 0.100
    )
  )
  
  # Proxy profiles:
  # - Bangladesh uses India.
  # - Malaysia uses the Philippines.
  # - Cameroon uses Nigeria.
  #
  # Nigeria and Cameroon retain their previous profiles pending updated data.
  
  # Default row = simple mean across all documented/proxy country profiles.
  default_row <- shares_raw[
    , lapply(.SD, mean),
    .SDcols = SODIUM_SOURCES
  ]
  default_row[, location := "default"]
  setcolorder(default_row, c("location", SODIUM_SOURCES))
  
  shares <- rbind(shares_raw, default_row, use.names = TRUE)
  
  # Renormalize every row to sum exactly to 1.0.
  # This is necessary because the Vietnam summary-table values sum to 1.12.
  rs <- rowSums(shares[, ..SODIUM_SOURCES])
  shares[
    , (SODIUM_SOURCES) := lapply(.SD, function(x) x / rs),
    .SDcols = SODIUM_SOURCES
  ]
  
  if (isTRUE(write)) {
    if (is.null(wd_data)) {
      stop("wd_data must be supplied when write = TRUE")
    }
    
    saveRDS(
      shares,
      file = file.path(wd_data, "sodium_source_shares.rds")
    )
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

#' Task 3b: year-specific salteff for a country from YEAR-INDEXED source shares.
#'
#' Because the Euromonitor trend makes packaged (and, via renormalisation, every)
#' source share vary by year, the total-intake reduction fraction is now a
#' function of year: salteff(year) = sum_s share_s(country, year) x composed_s,
#' where composed_s = 1 - prod(1 - eff_i,s) depends ONLY on the interventions.
#' Returns a data.table(year, salteff). When the trend is flat (shares constant),
#' salteff(year) is constant and equals the scalar compute_total_efficacy() built
#' on the same (base-year) shares -> exact backward-compatibility.
compute_salteff_by_year <- function(intervention_names, country,
                                     source_shares_by_year, intervention_effects) {
  sources <- c("discretionary", "packaged", "restaurant", "public", "inherent")
  ssy <- source_shares_by_year[location == country]
  if (nrow(ssy) == 0L) ssy <- source_shares_by_year[location == "default"]
  if (nrow(ssy) == 0L) stop("No year-specific source shares for '", country, "'.")

  if (is.null(intervention_names) || length(intervention_names) == 0L) {
    return(ssy[, .(year, salteff = 0)])
  }
  unknown <- setdiff(intervention_names, unique(intervention_effects$intervention))
  if (length(unknown) > 0L) {
    stop("Unknown intervention(s) in compute_salteff_by_year(): ", paste(unknown, collapse = ", "))
  }
  eff <- intervention_effects[intervention %in% intervention_names]
  composed <- vapply(sources, function(s) {
    e <- eff[source == s, effect]; if (length(e) == 0L) 0 else 1 - prod(1 - e)
  }, numeric(1))
  ssy_long <- melt(ssy, id.vars = c("location", "year"), measure.vars = sources,
                   variable.name = "source", value.name = "share")
  ssy_long[, comp := composed[as.character(source)]]
  ssy_long[, .(salteff = sum(share * comp)), by = year][order(year)]
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
                                   saltyear2 = 2030,
                                   lss_method                = get0("LSS_METHOD", envir = .GlobalEnv, ifnotfound = "na_k_sbp"),
                                   lss_benchmark_ssass       = get0("LSS_BENCHMARK_SSASS", envir = .GlobalEnv, ifnotfound = FALSE),
                                   lss_coverage_all          = get0("LSS_COVERAGE_ALL", envir = .GlobalEnv, ifnotfound = 0.50),
                                   lss_s2_coverage_levels    = get0("LSS_S2_COVERAGE_LEVELS", envir = .GlobalEnv,
                                                                    ifnotfound = c(0.10, 0.20, 0.30, 0.40, 0.50)),
                                   fiscal_sensitivity        = get0("FISCAL_SENSITIVITY", envir = .GlobalEnv, ifnotfound = NULL),
                                   include_fiscal_in_package = get0("include_fiscal_in_package", envir = .GlobalEnv, ifnotfound = TRUE)) {
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

  # LSS reach -> NCD-RisC eligibility column (Task 1d). NA => scenario-2 whole-
  # population coverage (lss_coverage_all); the *_pop columns are resolved per
  # country x sex in run_multiple_scenarios (NOT here). No BP>=140 proxy.
  # s4/s5 reach defs (NCD-RisC eligibility). Scenario 2 (reach = "all") is emitted
  # separately below, once per whole-population coverage level in
  # lss_s2_coverage_levels, so the s2 uptake sweep is explicit in the registry.
  lss_reach_defs <- list(
    lss_s4 = list(reach = "htn_diagnosed", eligibility = "diagnosed_pop",
                  cov = NA_real_,          lab = "diagnosed hypertension"),
    lss_s5 = list(reach = "htn_treated",   eligibility = "treated_pop",
                  cov = NA_real_,          lab = "treated hypertension")
  )
  method_tag <- function(m) if (m == "ssass_trial_rr") "SSaSS trial-RR benchmark" else "Na/K-SBP"

  # Helper to add one LSS variant with a given method (guarded).
  add_lss <- function(vname, base, method) {
    # Guard (Task 1c/1d): SSaSS RRs are not applicable to the whole population.
    if (identical(method, "ssass_trial_rr") && base$reach == "all") {
      stop("build_scenario_configs(): LSS scenario 2 (reach = 'all') is invalid for ",
           "method 'ssass_trial_rr' -- SSaSS trial RRs are not applicable to the whole ",
           "population. Use LSS_METHOD = 'na_k_sbp' for s2.")
    }
    configs[[vname]] <<- list(
      interventions   = "lss",
      lss_reach       = base$reach,
      lss_eligibility = base$eligibility,
      lss_coverage    = base$cov,          # scalar for s2; NA (per-country) for s4/s5
      lss_method      = method,
      saltyear1       = saltyear1,
      saltyear2       = saltyear2,
      label           = sprintf("LSS - %s (%s)", base$lab, method_tag(method))
    )
  }

  # Individual scenarios.
  if (scenario_mode %in% c("individual", "both")) {
    for (nm in selected) {
      if (nm == "lss") {
        # PRIMARY method (Task 1b/1c). Scenario 2 = whole-population discretionary
        # LSS, emitted once per requested coverage level as lss_s2_<pct> so the
        # population-uptake sweep (10-50%) is explicit and discoverable.
        for (cvl in lss_s2_coverage_levels) {
          pct <- round(cvl * 100)
          add_lss(sprintf("lss_s2_%02d", pct),
                  list(reach = "all", eligibility = NA_character_, cov = cvl,
                       lab = sprintf("whole-population discretionary, %d%% coverage", pct)),
                  lss_method)
        }
        # Scenarios 4/5 (diagnosed/treated HTN). Distinct scenario names when a
        # benchmark is also emitted so the two methods never merge.
        for (vnm in names(lss_reach_defs)) add_lss(vnm, lss_reach_defs[[vnm]], lss_method)
        # SSaSS trial-RR BENCHMARK (Task 1c): distinct s4/s5 scenarios, only when
        # the primary method is not already ssass_trial_rr. Never s2 (guard).
        if (isTRUE(lss_benchmark_ssass) && lss_method != "ssass_trial_rr") {
          for (vnm in c("lss_s4", "lss_s5")) {
            add_lss(paste0(vnm, "_ssass"), lss_reach_defs[[vnm]], "ssass_trial_rr")
          }
        }
      } else if (nm == "fiscal" && !is.null(fiscal_sensitivity)) {
        # Task 2b: one scenario per fiscal sensitivity variant (fiscal_low/base/high).
        for (vnm in names(fiscal_sensitivity)) {
          configs[[vnm]] <- list(
            interventions = vnm, saltyear1 = saltyear1, saltyear2 = saltyear2,
            label = paste0("Fiscal policy - ", sub("^fiscal_", "", vnm),
                           " (EXPLORATORY salt tax)"))
        }
      } else {
        configs[[nm]] <- list(
          interventions = nm,
          saltyear1     = saltyear1,
          saltyear2     = saltyear2,
          label         = registry[intervention == nm, label]
        )
      }
    }
  }

  # Combined package scenario.
  if (scenario_mode %in% c("combined", "both")) {
    pkg <- selected
    if (!isTRUE(include_placeholders_in_package)) {
      pkg <- setdiff(pkg, registry[placeholder == TRUE, intervention])
      pkg <- defined[defined %in% pkg]  # re-impose canonical order
    }
    pkg_nonfiscal <- setdiff(pkg, "fiscal")  # public_procurement + fopl + salt_targets
    if (!isTRUE(include_fiscal_in_package)) {
      # Fiscal excluded: a single package of the non-fiscal policies.
      if (length(pkg_nonfiscal) > 0L) {
        configs$full_package <- list(
          interventions = pkg_nonfiscal, saltyear1 = saltyear1, saltyear2 = saltyear2,
          label = paste0("Full package (", paste(pkg_nonfiscal, collapse = " + "), ")"))
      }
    } else if (!is.null(fiscal_sensitivity) && "fiscal" %in% pkg) {
      # Task (client Table 1 "Total 1/2/3"): one package total per fiscal
      # sensitivity variant. The base 'full_package' keeps the plain 'fiscal'
      # effect (identical to fiscal_base) for backward-compatibility; the low/high
      # packages swap in the fiscal_low / fiscal_high effect. Package totals are
      # genuine model runs (deaths delayed is non-additive), never composed by hand.
      pkg_variants <- c(full_package_low  = "fiscal_low",
                        full_package      = "fiscal",
                        full_package_high = "fiscal_high")
      for (pname in names(pkg_variants)) {
        fv  <- pkg_variants[[pname]]
        ivs <- c(pkg_nonfiscal, fv)
        vlab <- if (pname == "full_package") "base"
                else sub("^full_package_", "", pname)
        configs[[pname]] <- list(
          interventions = ivs, saltyear1 = saltyear1, saltyear2 = saltyear2,
          label = if (pname == "full_package")
            paste0("Full package (", paste(c(pkg_nonfiscal, "fiscal"), collapse = " + "), ")")
          else paste0("Full package — ", vlab, " fiscal variant"))
      }
    } else if (length(pkg) > 0L) {
      # Fiscal in package but no sensitivity set: single base package.
      configs$full_package <- list(
        interventions = pkg, saltyear1 = saltyear1, saltyear2 = saltyear2,
        label = paste0("Full package (", paste(pkg, collapse = " + "), ")"))
    }
  }

  configs
}

# <<< END SOURCE-LOGIC (unit-testable definitions) <<<

# --- Build / write / load source shares (mirrors prepare_sodium_data) --------
build_sodium_source_shares(wd_data, write = TRUE)
source_shares <- readRDS(paste0(wd_data, "sodium_source_shares.rds"))

# --- Task 1: LSS Na/K->SBP inputs, K->SBP interpolator, parameter bundle ------
# Baseline potassium (Reddin 2023, region-mean fallback) and NCD-RisC eligibility
# are (re)built HERE as well as in 02 because the standard run recipe sources
# 01,03,04,05,07 and SKIPS 02. Builders live in 01_utils.R and are deterministic
# pure functions of the committed raw CSVs; the hard checks (all modelled
# country x sex resolve to non-NA) fire against PRIORITY_COUNTRIES.
htn_eligibility <- build_htn_eligibility(
  wd_raw, wd_data, name_map, source_year = HTN_SOURCE_YEAR,
  required_locations = PRIORITY_COUNTRIES, write = TRUE)
baseline_potassium <- build_baseline_potassium(
  wd_raw, wd_data, name_map,
  required_locations = PRIORITY_COUNTRIES, write = TRUE)

#' Potassium-mediated SBP reduction from Filippini (2020) FIGURE 2: the CHANGE in
#' 24h urinary potassium (mmol/day) -> SBP reduction (mmHg). The anchors store the
#' treated-minus-control SBP difference (negative = SBP lowering); this returns a
#' POSITIVE SBP reduction (the model's sign convention), so a negative return =
#' an adverse SBP increase at high delta uK. Piecewise-linear, constant
#' extrapolation beyond the anchor range (rule = 2).
# UPDATED vs previous implementation: replaces k_excretion_to_sbp(), which
# interpolated the Fig-3 ACHIEVED-excretion U-curve and was DIFFERENCED at
# baseline vs post-LSS excretion.
k_delta_to_sbp_reduction <- function(delta_uk_mmol,
                                     anchors = LSS_K_DELTA_SBP_ANCHORS) {
  -stats::approx(x = anchors$delta_uk_mmol, y = anchors$sbp_change_mmhg,
                 xout = delta_uk_mmol, method = "linear", rule = 2)$y
}

#' Filippini (2020) FIGURE S16 baseline urinary-K subgroup multiplier m_K(uK0):
#' larger below the threshold (potassium-deficient), smaller at/above it
#' (potassium-replete). Vectorised over uK0. Scalar defaults = the same global
#' constants the model uses, so the audit and the model share one helper.
k_baseline_uk_multiplier <- function(uk0_mmol,
                                     threshold = LSS_K_BASELINE_UK_THRESHOLD,
                                     mult_low  = LSS_K_BASELINE_MULT_LOW,
                                     mult_high = LSS_K_BASELINE_MULT_HIGH) {
  ifelse(uk0_mmol < threshold, mult_low, mult_high)
}

# Bundle the LSS Na/K mechanistic parameters so a SINGLE clusterExport reaches
# every worker. Per-scenario method/reach/coverage are NOT here -- they travel
# with the scenario config.
# UPDATED vs previous implementation: k_sbp_anchors/k_na_modulation/nonhtn_* are
# gone; the K channel now carries the Fig-2 delta-uK anchors + the Fig-S16
# baseline-uK threshold and subgroup multipliers.
LSS_PARAMS <- list(
  nacl_fraction       = LSS_NACL_FRACTION,   kcl_fraction        = LSS_KCL_FRACTION,
  uptake              = LSS_UPTAKE,          adherence           = LSS_ADHERENCE,
  additive            = LSS_NAK_ADDITIVE,    additivity_factor   = LSS_ADDITIVITY_FACTOR,
  mg_per_mmol         = K_MG_PER_MMOL,       intake_to_excretion = K_INTAKE_TO_EXCRETION,
  na_per_g_nacl       = NA_PER_G_NACL,       k_per_g_kcl         = K_PER_G_KCL,
  k_delta_sbp_anchors = LSS_K_DELTA_SBP_ANCHORS,
  baseline_uk_threshold = LSS_K_BASELINE_UK_THRESHOLD,
  baseline_uk_mult_low  = LSS_K_BASELINE_MULT_LOW,
  baseline_uk_mult_high = LSS_K_BASELINE_MULT_HIGH,
  delta_uk_zero_crossing = LSS_K_DELTA_UK_ZERO_CROSSING
)

# --- Task 5: adults-only public-procurement guard (fail loudly on stale 5%) ---
# The documented public food-procurement share is a flat 0.05 that ASSUMED
# school coverage, but this model is adults-only. Refuse to run silently on that
# stale value. This runs BEFORE the Task-4b trend (which would renormalise the
# public share away from 0.05) and BEFORE the structural validation, so it sees
# the value exactly as build_sodium_source_shares() produced it.
# ACTION FOR RTSL: replace the `public` column per country in
# build_sodium_source_shares() with the adults-only shares (row renormalisation
# there rebalances the other sources), AND update the Viet Nam sanity check
# below that asserts public_procurement salteff = 0.20 x 0.05, or it will fail.
# NB build_sodium_source_shares() renormalises each row to sum to 1, which
# perturbs the flat 0.05 by up to ~5e-5 (e.g. Thailand's documented shares sum
# to 0.999 -> public 0.050050; Nigeria/Cameroon sum to 1.001 -> 0.049950). The
# stale signature is therefore "public ~= 0.05 for EVERY country" (not exactly
# equal). A 1e-3 tolerance clears that jitter yet trips FALSE the moment RTSL's
# materially different, per-country adults-only shares are inserted.
.public_is_stale_5pct <- all(abs(source_shares$public - 0.05) < 1e-3)
if (isTRUE(.public_is_stale_5pct)) {
  if (isTRUE(ADULT_POPULATION_ONLY)) {
    stop("ADULT_POPULATION_ONLY = TRUE but public procurement is still the ",
         "school-inclusive 5% flat value. Insert the RTSL adults-only per-country ",
         "shares in build_sodium_source_shares() before running.")
  } else {
    cat("\n############################################################\n")
    cat("## PROVISIONAL RUN CAVEAT (ADULT_POPULATION_ONLY = FALSE)\n")
    cat("## Public food procurement uses the stale SCHOOL-INCLUSIVE 5%\n")
    cat("## flat share, which overstates adults-only procurement reach.\n")
    cat("## public_procurement (and full_package) results are PROVISIONAL\n")
    cat("## pending RTSL's adults-only per-country shares.\n")
    cat("############################################################\n")
  }
}
rm(.public_is_stale_5pct)

# --- Build scenario_configs from the user controls ---------------------------
scenario_configs <- build_scenario_configs(
  selected_interventions          = selected_interventions,
  scenario_mode                   = scenario_mode,
  registry                        = intervention_registry,
  include_placeholders_in_package = include_placeholders_in_package,
  saltyear1                       = SCALEUP_YEAR1,
  saltyear2                       = SCALEUP_YEAR2,
  lss_method                      = LSS_METHOD,
  lss_benchmark_ssass             = LSS_BENCHMARK_SSASS,
  lss_coverage_all                = LSS_COVERAGE_ALL,
  lss_s2_coverage_levels          = LSS_S2_COVERAGE_LEVELS,
  fiscal_sensitivity              = FISCAL_SENSITIVITY,
  include_fiscal_in_package       = include_fiscal_in_package
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
# .chk_pub <- as.numeric(compute_total_efficacy("public_procurement", "Viet Nam",
#                                               source_shares, intervention_effects))
# if (abs(.chk_pub - 0.20 * 0.05) > 1e-6) {
#   stop(sprintf("Sanity check failed: public_procurement salteff for Viet Nam = %.6f (expected %.6f)",
#                .chk_pub, 0.20 * 0.05))
# }
# cat(sprintf("\nStructural validation OK. Sanity check: public_procurement salteff (Viet Nam) = %.4f (= 0.20 x 0.05)\n",
#             .chk_pub))
# rm(.chk_sums, .bad_src, .chk_pub)

# --- Task 2: fiscal pathway trace + fiscal_audit -----------------------------
# Echo the EXPLORATORY fiscal pathway (never a buried constant) and build the
# audit the report consumes. One row per variant (or the single base case if
# FISCAL_SENSITIVITY is NULL). taxed_packaged_reduction is the % cut in the TAXED
# packaged category; packaged_reduction is the cut in TOTAL packaged sodium (x
# taxable share); total_sodium_change_ref multiplies by a REFERENCE packaged
# share (mean base-year across priority countries) -- the report recomputes the
# country-specific value from source_shares_by_year.
.fiscal_row <- function(name, p) {
  price_change    <- p$tax_rate * p$pass_through
  quantity_change <- p$elasticity * price_change
  consumer_red    <- -quantity_change * (1 - p$substitution)
  taxed_red       <- max(0, min(1, consumer_red + p$reformulation))
  data.table(
    variant = name, tax_rate = p$tax_rate, pass_through = p$pass_through,
    price_change = price_change, elasticity = p$elasticity,
    quantity_response = quantity_change, taxable_share = p$taxable_packaged_share,
    substitution = p$substitution, reformulation = p$reformulation,
    taxed_packaged_reduction = taxed_red,
    packaged_reduction = taxed_red * p$taxable_packaged_share)
}
.fiscal_param_sets <- if (is.null(FISCAL_SENSITIVITY)) {
  list(fiscal = list(tax_rate = FISCAL_TAX_RATE, pass_through = FISCAL_PASS_THROUGH,
                     elasticity = FISCAL_ELASTICITY, taxable_packaged_share = FISCAL_TAXABLE_SHARE,
                     substitution = FISCAL_SUBSTITUTION, reformulation = FISCAL_REFORMULATION))
} else FISCAL_SENSITIVITY
.pkg_share_ref <- mean(source_shares[location %in% PRIORITY_COUNTRIES, packaged])
fiscal_audit <- rbindlist(Map(.fiscal_row, names(.fiscal_param_sets), .fiscal_param_sets))
fiscal_audit[, `:=`(ramp_start = SCALEUP_YEAR1, ramp_end = SCALEUP_YEAR2,
                    packaged_share_ref = .pkg_share_ref,
                    total_sodium_change_ref = packaged_reduction * .pkg_share_ref,
                    status = "exploratory")]
saveRDS(fiscal_audit, file = paste0(wd_data, "fiscal_audit.rds"))
cat(sprintf("\nFiscal (EXPLORATORY): base EFF_FISCAL_PACKAGED = %.4f (reduction in TOTAL packaged sodium).\n",
            EFF_FISCAL_PACKAGED))
cat("  Fiscal pathway trace (variant -> effective packaged-sodium reduction):\n")
print(fiscal_audit[, .(variant, tax_rate, pass_through, elasticity,
                       taxable_share, substitution, packaged_reduction = round(packaged_reduction, 4))])
rm(.fiscal_row, .fiscal_param_sets, .pkg_share_ref)

# --- Task 3b/3c: YEAR-SPECIFIC source shares from the Euromonitor trend -------
# REPLACES the old single-2030-end-state factor (which forced salteff to stay a
# scalar). The packaged share now grows by (1+g)^(year - base_year) from
# EUROMONITOR_BASE_YEAR, is capped at EUROMONITOR_PACKAGED_CEILING, and after 2030
# follows EUROMONITOR_POST2030; the OTHER four sources renormalise each year to
# keep all five summing to 1 (COMPOSITION-ONLY: total sodium unchanged). The
# scalar `source_shares` above is LEFT as the documented base-year shares (used
# for the adults-only guard, structural validation, gating/logging, and as the
# base of the year table). salteff is resolved per (country, year) from
# source_shares_by_year in run_multiple_scenarios (compute_salteff_by_year), so a
# scalar salteff is NO LONGER assumed for packaged-source interventions. When the
# trend is flat (g = 0 or run_packaged_trend = FALSE) every year equals the
# documented shares -> EXACT backward-compatibility with the static run.
build_source_shares_by_year <- function(source_shares, packaged_trends,
                                         base_year, ceiling, post2030, years,
                                         run_trend = TRUE) {
  sources <- c("discretionary", "packaged", "restaurant", "public", "inherent")
  base <- copy(source_shares)[, c("location", sources), with = FALSE]
  if (isTRUE(run_trend) && !is.null(packaged_trends)) {
    base <- merge(base, packaged_trends[, .(location, g)], by = "location", all.x = TRUE)
  } else {
    base[, g := 0]
  }
  base[is.na(g), g := 0]   # countries absent from Euromonitor (incl. 'default') -> flat

  grid <- base[CJ(location = base$location, year = years),
               on = "location", allow.cartesian = TRUE]
  last_year <- max(years)

  # Packaged share by year: full trend, then post-2030 rule, then ceiling cap.
  grid[, pkg     := packaged * (1 + g)^pmax(0, year - base_year)]
  grid[, pkg2030 := packaged * (1 + g)^pmax(0, 2030 - base_year)]
  if (post2030 == "hold_constant") {
    grid[year > 2030, pkg := pkg2030]
  } else if (post2030 == "converge_to_ceiling") {
    grid[year > 2030, pkg := pmin(ceiling,
         pkg2030 + (ceiling - pkg2030) * (year - 2030) / max(1, last_year - 2030))]
  } # "continue_trend": leave pkg growing (still ceiling-capped below)
  grid[, pkg := pmin(pkg, ceiling)]

  # Renormalise the OTHER four sources to fill (1 - pkg), preserving their
  # relative split (others_base = 1 - packaged_base for that location).
  grid[, others_base := discretionary + restaurant + public + inherent]
  for (s in setdiff(sources, "packaged")) {
    grid[, (s) := get(s) * (1 - pkg) / others_base]
  }
  grid[, packaged := pkg]
  grid[, c("g", "pkg", "pkg2030", "others_base") := NULL]
  setcolorder(grid, c("location", "year", sources))
  setorder(grid, location, year)
  grid[]
}

.share_years   <- ANALYSIS_YEAR_MIN:2058     # superset of dt_baseline years
packaged_trends <- if (file.exists(paste0(wd_data, "packaged_food_trends.rds")))
  readRDS(paste0(wd_data, "packaged_food_trends.rds")) else NULL
if (isTRUE(run_packaged_trend) && is.null(packaged_trends)) {
  stop("run_packaged_trend = TRUE but packaged_food_trends.rds is missing -- run 03_clean_inputs.R first.")
}
source_shares_by_year <- build_source_shares_by_year(
  source_shares, packaged_trends,
  base_year = EUROMONITOR_BASE_YEAR, ceiling = EUROMONITOR_PACKAGED_CEILING,
  post2030  = EUROMONITOR_POST2030,  years    = .share_years,
  run_trend = run_packaged_trend)

# Assert all five shares sum to 1 in EVERY (location, year) (tol 1e-6).
.ssy_chk <- source_shares_by_year[, .(s = sum(.SD)), .SDcols = SODIUM_SOURCES,
                                  by = .(location, year)]
if (any(abs(.ssy_chk$s - 1) > 1e-6)) {
  stop("source_shares_by_year: shares do not sum to 1 (tol 1e-6) in ",
       sum(abs(.ssy_chk$s - 1) > 1e-6), " (location, year) cell(s).")
}
saveRDS(source_shares_by_year, file = paste0(wd_data, "source_shares_by_year.rds"))

# Trend diagnostic (Task 3c).
.diag <- merge(
  source_shares_by_year[year == EUROMONITOR_BASE_YEAR, .(location, pkg_base = packaged)],
  source_shares_by_year[year == 2030,                  .(location, pkg_2030 = packaged)], by = "location")
.diag <- merge(.diag, source_shares_by_year[year == 2050, .(location, pkg_2050 = packaged)], by = "location")
if (!is.null(packaged_trends)) {
  .diag <- merge(.diag, packaged_trends[, .(location, g, g_lcl, g_ucl, window)],
                 by = "location", all.x = TRUE)
} else {
  .diag[, `:=`(g = 0, g_lcl = NA_real_, g_ucl = NA_real_, window = "none")]
}
.diag[, `:=`(base_year = EUROMONITOR_BASE_YEAR, ceiling = EUROMONITOR_PACKAGED_CEILING,
             post2030 = EUROMONITOR_POST2030,
             growth_source = paste0("Euromonitor v002 combined (", EUROMONITOR_WINDOW, ")"))]
saveRDS(.diag, file = paste0(wd_data, "packaged_trend_diagnostic.rds"))

cat(sprintf("\nTask 3: YEAR-SPECIFIC source shares built (trend = %s, window = %s, base = %d, ceiling = %.2f, post-2030 = %s).\n",
            run_packaged_trend, EUROMONITOR_WINDOW, EUROMONITOR_BASE_YEAR,
            EUROMONITOR_PACKAGED_CEILING, EUROMONITOR_POST2030))
cat("  packaged share (base -> 2030 -> 2050) by country:\n")
print(.diag[order(-pkg_2030), .(location,
      pkg_base = round(pkg_base, 4), pkg_2030 = round(pkg_2030, 4),
      pkg_2050 = round(pkg_2050, 4), g = round(g, 4))])
rm(.share_years, .ssy_chk, .diag)

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
# SECTION 4: Epidemiological Helper Functions               (unchanged from 06)----
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
  
  # hstroke has no ETIHAD SBP-effect RR of its own, so the effect pathway
  # borrows istroke's (the intentional ETIHAD carry above). For a coherent CRA
  # the baseline decomposition must use the SAME RR as the effect, so hstroke
  # ALSO uses istroke's GBD RR here -- in both alpha and the bin split below.
  # (Using hstroke's own steeper GBD RR for the decomposition while the effect
  # stays on istroke's flatter RR concentrates baseline incidence in high-BP
  # bins and inflated the modelled hstroke cut so it dominated every sodium->SBP
  # scenario. Treating hstroke exactly like istroke removes that mismatch; the
  # CRA identity sum_b(prob * IR_bin) = IR still holds, asserted below.)
  alphas <- bp_prob[, .(
    ihd     = sum(prob * RRi_IHD),
    istroke = sum(prob * RRi_ISTROKE),
    hstroke = sum(prob * RRi_ISTROKE),
    hhd     = sum(prob * RRi_HHD),
    aod     = sum(prob * RRi_AOD)
  ), by = .(age, sex, location, Year)]
  
  alphas <- melt(alphas, id.vars = c("age", "sex", "location", "Year"),
                 variable.name = "cause", value.name = "alpha")
  
  rris <- bp_prob[, .(age, sex, Year, location, bp_cat, prob,
                      RRi_IHD, RRi_HHD, RRi_ISTROKE, RRi_HSTROKE, RRi_AOD)]
  # hstroke borrows istroke's RR for the bin split too (see the alpha note
  # above) so the decomposition RR matches the borrowed istroke effect RR.
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

  # Invariant: the BP-bin split must reconstruct the population-level incidence
  # for every cause (sum_b prob_b * IR_bin = IR). This is exactly what the old
  # hstroke RR mismatch violated (the ratio fell to ~0.79); it now holds for all
  # causes. Fail loud if a future RR/alpha inconsistency breaks it again.
  ir_check <- dt[, .(
    IR_input         = first(IR),
    IR_reconstructed = sum(prob * IR_bin)
  ), by = .(location, year, age, sex, cause)]

  tolerance <- 1e-10
  bad <- ir_check[
    !is.finite(IR_reconstructed) |
      abs(IR_reconstructed - IR_input) > tolerance * pmax(1, abs(IR_input))
  ]
  if (nrow(bad) > 0L) {
    stop(sprintf(
      "BP-bin incidence decomposition failed for %d location-year-age-sex-cause cells.",
      nrow(bad)
    ))
  }

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
# SECTION 5: Sodium Data Preparation                        (unchanged from 06)----
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
# SECTION 6: Core Sodium Intervention Model                 (unchanged from 06)----
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
    dt_gbd_rr,
    salteff_year = NULL           # Task 3b: optional data.table(year, salteff);
                                  # when supplied, salt_target uses the per-YEAR
                                  # salteff (packaged-source shares vary by year).
                                  # NULL -> the scalar salteff (exact old behaviour).
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
    if (!is.null(salteff_year)) {
      # Task 3b: year-specific total-intake reduction fraction (packaged-source
      # shares -- and, via renormalisation, all shares -- vary by year). Merge the
      # per-year salteff and use it row-wise. Years outside the table fall back to
      # the scalar salteff. When the trend is flat this equals `salt * salteff`.
      dt_baseline <- merge(dt_baseline, salteff_year[, .(year, .salteff_yr = salteff)],
                           by = "year", all.x = TRUE)
      dt_baseline[is.na(.salteff_yr), .salteff_yr := as.numeric(salteff)]
      dt_baseline[, salt_target := salt * .salteff_yr]
      dt_baseline[, .salteff_yr := NULL]
    } else {
      dt_baseline[, salt_target := salt * salteff]
    }
  } else if (saltmet == "target") {
    dt_baseline[, salt_target := pmin(salt, salteff)]
  } else if (saltmet == "app") {
    dt_baseline[, salt_target := pmax(0, salt - salteff)]
  }

  # --- LSS reach + coverage (Task 1d; NCD-RisC eligibility, NO BP>=140 proxy) --
  # Reach/coverage/method travel WITH the scenario config (attributes on salteff,
  # set in project.all), so the restriction fires INSIDE a package too. The old
  # BP>=140 bin PROXY for diagnosed/treated hypertension is REMOVED: coverage is
  # now the NCD-RisC POPULATION eligibility share (diagnosed_pop for s4,
  # treated_pop for s5; whole-population LSS_COVERAGE_ALL for s2), applied across
  # the whole population. lss_coverage is a scalar (s2) OR a per-sex
  # data.table(sex, cov) (s4/s5, country x sex specific). '.cov' is kept on
  # dt_baseline for the K channel and the SSaSS trial-RR 'reached' fraction.
  #   lss_reach: "none" | "all" | "htn_diagnosed" | "htn_treated"
  lss_reach    <- attr(salteff, "lss_reach")
  lss_coverage <- attr(salteff, "lss_coverage")
  lss_method   <- attr(salteff, "lss_method")
  # UPDATED vs previous implementation: hypertensive_bins was only used for the
  # retired non-HTN K attenuation; the Fig-2/Fig-S16 K channel no longer stratifies
  # by BP bin, so it is dropped here.
  if (!is.null(lss_reach) && lss_reach != "none") {
    if (is.data.table(lss_coverage)) {
      dt_baseline <- merge(dt_baseline, lss_coverage[, .(sex, .cov = cov)],
                           by = "sex", all.x = TRUE)
    } else {
      dt_baseline[, .cov := as.numeric(lss_coverage)]
    }
    if (any(is.na(dt_baseline$.cov))) {
      stop("LSS coverage unresolved for some sex in ", Country,
           " (reach = ", lss_reach, ").")
    }
    dt_baseline[, salt_target := salt_target * .cov]
    cat("  - LSS reach:", lss_reach, "| method:", lss_method,
        "| coverage(mean) =", round(mean(dt_baseline$.cov), 4),
        "(NCD-RisC population eligibility; no BP>=140 proxy)\n")
  }

  # Enforce WHO floor: minimum sodium intake 2 g/day (no further SBP effect below).
  dt_baseline[, salt_target := ifelse(salt - salt_target < 2, salt - 2, salt_target)]
  
  # Step 4: Progressive linear scale-up
  dt_baseline[year >= saltyear1 & year <= saltyear2,
              salt_reduction := salt_target * (year - saltyear1 + 1) /
                (saltyear2 - saltyear1 + 1)]
  dt_baseline[year > saltyear2, salt_reduction := salt_target]
  dt_baseline[year < saltyear1, salt_reduction := 0]
  dt_baseline[is.na(salt_reduction) | salt_reduction < 0, salt_reduction := 0]

  # ---- LSS SSaSS trial-RR BENCHMARK pathway (Task 1c) ------------------------
  # When lss_method == "ssass_trial_rr" the LSS effect on STROKE is taken from the
  # SSaSS trial (Neal et al., NEJM 2021;385:1067-77) rather than the mechanistic
  # Na/K->SBP channel. Mapped onto this model's (incidence IR, case-fatality CF)
  # structure with NO double-count:
  #   - incidence multiplier = nonfatal-stroke RR 0.90        (-> istroke, hstroke IR)
  #   - case-fatality mult.   = fatal/nonfatal 0.77/0.90 = 0.856 (-> istroke, hstroke CF)
  # The total-stroke RR 0.86 is NOT applied separately (0.90 x 0.856 reproduces
  # it). Non-stroke causes (ihd, hhd) get NO trial effect -- this is the key
  # contrast with na_k_sbp, which affects ALL CVD via SBP. "trial_rr" is accepted
  # as a legacy alias. The single-pathway-per-cause guardrail is preserved: stroke
  # is driven by the trial RR here and by the Na/K SBP channel under na_k_sbp,
  # NEVER both in one run.
  apply_trial_rr <- !is.null(lss_method) && lss_method %in% c("ssass_trial_rr", "trial_rr") &&
                    !is.null(lss_reach)  && lss_reach != "none"

  if (apply_trial_rr) {
    RR_STROKE_INCIDENCE <- 0.90            # nonfatal-stroke RR -> incidence
    RR_STROKE_CF        <- 0.77 / 0.90     # conditional CF effect (= 0.8556)
    stroke_causes       <- c("istroke", "hstroke")

    # Reached fraction = NCD-RisC population eligibility (.cov) x scale-up ramp,
    # applied across the whole population (NO BP>=140 bin restriction; Task 1d).
    dt_baseline[, ramp := fifelse(
      year < saltyear1, 0,
      fifelse(year > saltyear2, 1,
              (year - saltyear1 + 1) / (saltyear2 - saltyear1 + 1)))]
    dt_baseline[, reached := .cov * ramp]

    # Blend each RR toward 1 by the reached fraction: eff RR = 1 - reached*(1-RR).
    dt_baseline[, eff_ir_trial := 1]
    dt_baseline[, eff_cf_trial := 1]
    dt_baseline[cause %in% stroke_causes,
                eff_ir_trial := 1 - reached * (1 - RR_STROKE_INCIDENCE)]
    dt_baseline[cause %in% stroke_causes,
                eff_cf_trial := 1 - reached * (1 - RR_STROKE_CF)]
  }

  # Step 5: Filippini SODIUM dose-response -> Na-mediated SBP reduction (mmHg).
  # (salt = sodium g/day; 2.8 mmHg/g raised-BP, 1.0 mmHg/g normal-BP.)
  dt_baseline[, sbp_reduction := ((2.8 * raisedBP) + ((1 - raisedBP) * 1.0)) * salt_reduction]

  # ---- Step 5b: LSS POTASSIUM channel (Task 1b; na_k_sbp only) ---------------
  # Adds the Filippini (2020) potassium -> SBP effect to the Na-mediated dSBP,
  # then feeds the COMBINED dSBP through the SAME ETIHAD/GBD SBP->cause machinery
  # (Steps 6-9, sodium->SBP branch). BENEFIT-ONLY: no CKD/hyperkalaemia harm
  # channel exists in this disease model (unlike Huang et al.). K added is
  # stoichiometric to the sodium displaced (salt_reduction already carries reach x
  # coverage x ramp x WHO-floor), so the two channels stay consistent.
  #
  # UPDATED vs previous implementation: the previous version evaluated the
  # Filippini FIGURE 3 ACHIEVED-excretion U-curve at baseline and post-LSS
  # excretion and DIFFERENCED them, then scaled by baseline-SODIUM bands and a
  # non-hypertensive attenuation factor. This primary version uses the Filippini
  # FIGURE 2 CHANGE-IN-URINARY-K dose-response (k_delta_to_sbp_reduction) modified
  # ONLY by the Filippini FIGURE S16 baseline urinary-K subgroup multiplier
  # (k_baseline_uk_multiplier); the baseline-Na and non-HTN modifiers are retired.
  apply_na_k <- !is.null(lss_method) && lss_method == "na_k_sbp" &&
                !is.null(lss_reach)  && lss_reach != "none"
  if (apply_na_k) {
    lp <- get0("LSS_PARAMS", ifnotfound = NULL)
    if (is.null(lp)) stop("LSS_PARAMS not found for na_k_sbp pathway.")
    bpot <- get0("baseline_potassium", ifnotfound = NULL)
    if (is.null(bpot)) stop("baseline_potassium not found for na_k_sbp pathway.")

    bk <- bpot[location == Country, .(sex, k_intake_g)]
    if (nrow(bk) == 0L) stop("baseline potassium missing for ", Country)
    dt_baseline <- merge(dt_baseline, bk, by = "sex", all.x = TRUE)
    if (any(is.na(dt_baseline$k_intake_g))) stop("baseline potassium missing for some sex in ", Country)

    # (B) Baseline 24h urinary K excretion (mmol/d) from DIETARY intake (g/d):
    #     uK0 = intake_g * 1000 / (mg_per_mmol * intake:excretion). The /1.3 is
    #     applied here because k_intake_g is a dietary intake, never to a urinary
    #     value. Country x sex specific (drives the Fig-S16 subgroup below).
    dt_baseline[, baseline_urinary_potassium_mmol :=
                  (k_intake_g * 1000 / lp$mg_per_mmol) / lp$intake_to_excretion]
    # (C) K added (g/d) stoichiometric to Na displaced -> dietary mmol/d ->
    #     CHANGE in 24h urinary excretion (mmol/d) = dietary mmol / 1.3.
    dt_baseline[, k_added_g                   := salt_reduction * (lp$k_per_g_kcl / lp$na_per_g_nacl)]
    dt_baseline[, potassium_added_intake_mmol := k_added_g * 1000 / lp$mg_per_mmol]
    dt_baseline[, delta_urinary_potassium_mmol := potassium_added_intake_mmol / lp$intake_to_excretion]

    # (D) Figure-2 CHANGE-in-urinary-K dose-response -> POSITIVE SBP reduction
    #     (population-average curve; negative = adverse SBP rise at high delta uK,
    #     NOT truncated). (E) modified by the Figure-S16 baseline urinary-K
    #     subgroup multiplier m_K(uK0) (deficient populations get a LARGER effect).
    dt_baseline[, sbp_reduction_k_unmod :=
                  k_delta_to_sbp_reduction(delta_urinary_potassium_mmol, lp$k_delta_sbp_anchors)]
    dt_baseline[, k_baseline_mult :=
                  k_baseline_uk_multiplier(baseline_urinary_potassium_mmol,
                                           lp$baseline_uk_threshold,
                                           lp$baseline_uk_mult_low,
                                           lp$baseline_uk_mult_high)]
    dt_baseline[, sbp_reduction_k := sbp_reduction_k_unmod * k_baseline_mult]

    # (F) Combine Na- and K-mediated dSBP additively (a = additivity_factor; 1.0
    #     primary, 0.8 Huang-style sensitivity). The COMBINED dSBP feeds ETIHAD
    #     exactly once; the SSaSS trial-RR guardrail keeps this OFF for stroke
    #     whenever the trial-RR benchmark is the active LSS method.
    if (isTRUE(lp$additive)) {
      dt_baseline[, sbp_reduction := sbp_reduction + lp$additivity_factor * sbp_reduction_k]
    }  # else: Na-only (K ignored) -- documented non-additive alternative
    n_over <- dt_baseline[delta_urinary_potassium_mmol > lp$delta_uk_zero_crossing, .N]
    cat("  - LSS na_k_sbp: Fig-2 delta-uK x Fig-S16 baseline-uK modifier ACTIVE",
        "(benefit-only, no CKD). mean K-dSBP =",
        round(mean(dt_baseline$sbp_reduction_k, na.rm = TRUE), 3), "mmHg; rows above",
        round(lp$delta_uk_zero_crossing, 1), "mmol/d zero-crossing:", n_over,
        "-> COMBINED dSBP feeds ETIHAD.\n")
  }
  
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
  
  # Step 8-9: results assembly. Exactly ONE pathway sets eff_ir/eff_cf for stroke
  # (Task 3f guardrail: NEVER trial-RR AND sodium->SBP together).
  if (apply_trial_rr) {
    cat("  - LSS trial-RR ACTIVE (SSaSS approach B): stroke eff_ir/eff_cf set",
        "from trial RRs; sodium->SBP incidence cut NOT applied to stroke",
        "(single pathway for stroke).\n")

    # Incidence: apply the trial RR at the bin level, then express the effect
    # RELATIVE TO THE BIN-AGGREGATED BASELINE, DISCARDING the SBP-mediated cut.
    # sum(IR_bin*prob) now equals IR for every cause -- calculate_baseline_
    # incidence_gbd() uses each cause's own GBD RR in both the bin split and
    # alpha and asserts that identity -- so this is equivalent to normalising by
    # raw IR. Kept as a self-normalising guard: the trial effect stays a clean
    # 1 - reached*(1-RR), and for reach="all" eff_ir collapses to the trial
    # multiplier for BOTH stroke causes.
    dt_baseline[, IR_base_agg := sum(IR_bin * prob),
                by = .(age, sex, location, cause, year)]
    dt_baseline[, IR_agg_new  := sum(IR_bin * eff_ir_trial * prob),
                by = .(age, sex, location, cause, year)]
    dt_baseline[, eff_ir := IR_agg_new / IR_base_agg]
    dt_baseline[is.na(eff_ir) | IR_base_agg == 0, eff_ir := 1]
    dt_baseline[year < saltyear1, eff_ir := 1]
    dt_baseline[, IR_new := IR * eff_ir]

    # Case fatality: conditional trial effect on stroke CF (was a no-op).
    # Normalise by the bin-aggregated baseline (SAME as incidence above) so the
    # prob-weighted multiplier is exact: for reach="all", eff_cf collapses to the
    # trial CF multiplier for both stroke causes, and non-stroke eff_cf is exactly
    # 1 (no floating-point drift from sum(prob) != 1 exactly).
    dt_baseline[, CF_base_agg := sum(CF * prob),
                by = .(age, sex, location, cause, year)]
    dt_baseline[, CF_agg_new  := sum(CF * eff_cf_trial * prob),
                by = .(age, sex, location, cause, year)]
    dt_baseline[, eff_cf := CF_agg_new / CF_base_agg]
    dt_baseline[is.na(eff_cf) | CF_base_agg == 0, eff_cf := 1]
    dt_baseline[year < saltyear1, eff_cf := 1]
    dt_baseline[, CF_new := CF * eff_cf]
  } else {
    # Step 8: Population-weighted average incidence (sodium->SBP channel)
    dt_baseline[, IR_new := sum(IR_bin_new * prob),
                by = .(age, sex, location, cause, year)]
    dt_baseline[year < saltyear1, IR_new := IR]
    dt_baseline[, eff_ir := IR_new / IR]

    # Step 9: Case fatality - no secondary effect from the sodium->SBP channel.
    # SENSITIVITY HOOK (Task 3h): an LSS scenario with lss_method == "sodium_sbp"
    # lands here as a DOCUMENTED sensitivity path (not wired into the reported
    # 7/23 scenarios, which all use trial_rr). It currently reconstructs the SBP
    # reduction from the sodium reduction via Filippini (Step 5); a fuller LSS
    # sensitivity would instead inject the OBSERVED SSaSS -3.34 mmHg SBP effect
    # directly (the +803 mg/day potassium rise drove much of it) rather than the
    # sodium-only reconstruction. Left as a hook for the long-term refactor.
    dt_baseline[, CF_new := CF]
    dt_baseline[, eff_cf := 1]
  }
  
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
# SECTION 7: Baseline Rate Cleaning                         (unchanged from 06)----
###############################################################################

b_rates[CF >= 1, CF := 0.99]
b_rates[IR >= 1, IR := 0.99]
b_rates[CF < 0,  CF := 0]
b_rates[IR < 0,  IR := 0]

###############################################################################
# SECTION 8: project.all()  - sodium-only projection        (unchanged from 06)----
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
    saltmet      = "percent",
    salteff      = 0.0,
    saltyear1    = 2026,
    saltyear2    = 2030,
    lss_reach    = "none",          # "none" | "all" | "htn_diagnosed" | "htn_treated"
    lss_coverage = 1.0,             # scalar (s2) OR data.table(sex, cov) (s4/s5)
    lss_method   = "sodium_sbp",    # "na_k_sbp" | "ssass_trial_rr" | "sodium_sbp"
    salteff_year = NULL             # Task 3b: data.table(year, salteff) or NULL
) {
  .cov_desc <- if (is.data.frame(lss_coverage))
    paste0("per-sex[", paste(sprintf("%s=%.3f", lss_coverage$sex, lss_coverage$cov), collapse = ","), "]")
  else as.character(lss_coverage)
  cat("\n========================================\n")
  cat("STARTING PROJECTION FOR:", Country, "\n")
  cat("salteff  =", salteff,
      if (!is.null(salteff_year)) " (year-specific)" else "",
      " | method:", saltmet,
      if (!identical(lss_reach, "none"))
        paste0(" | LSS reach: ", lss_reach, " (", lss_method, ", coverage ", .cov_desc, ")")
      else "", "\n")
  cat("scale-up:", saltyear1, "-", saltyear2, "\n")
  cat("========================================\n\n")

  # Carry the LSS reach/coverage/method on salteff so
  # calculate_sodium_impact_etihad() can (a) restrict the reduction to the reached
  # population and (b) select the na_k_sbp / ssass_trial_rr / sodium_sbp pathway.
  # as.numeric() upstream strips attributes, so they are (re)attached here.
  # NB: the LSS effect is gated behind salteff > 0 below; every priority country
  # has discretionary share > 0, so salteff > 0 and the gate holds.
  attr(salteff, "lss_reach")    <- lss_reach
  attr(salteff, "lss_coverage") <- lss_coverage
  attr(salteff, "lss_method")   <- lss_method
  
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
      dt_gbd_rr,
      salteff_year = salteff_year
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
# SECTION 9: run_multiple_scenarios()  - resolves salteff PER COUNTRY----
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
    source_shares         = get("source_shares",         envir = .GlobalEnv),
    intervention_effects  = get("intervention_effects",  envir = .GlobalEnv),
    source_shares_by_year = get0("source_shares_by_year", envir = .GlobalEnv),
    htn_eligibility       = get0("htn_eligibility",       envir = .GlobalEnv),
    lss_coverage_all      = get0("LSS_COVERAGE_ALL",      envir = .GlobalEnv, ifnotfound = 0.50)
) {
  results <- list()

  for (scenario_name in names(scenario_configs)) {
    cfg <- scenario_configs[[scenario_name]]

    s_interventions <- if (!is.null(cfg$interventions)) cfg$interventions else character(0)
    s_saltmet       <- if (!is.null(cfg$saltmet))       cfg$saltmet       else saltmet
    s_saltyear1     <- if (!is.null(cfg$saltyear1))     cfg$saltyear1     else saltyear1
    s_saltyear2     <- if (!is.null(cfg$saltyear2))     cfg$saltyear2     else saltyear2
    s_label         <- if (!is.null(cfg$label))         cfg$label         else scenario_name
    # LSS attributes travel WITH the scenario config. Defaults make non-LSS
    # scenarios a no-op: reach "none" -> no restriction; method "sodium_sbp".
    s_lss_reach     <- if (!is.null(cfg$lss_reach))     cfg$lss_reach     else "none"
    s_lss_method    <- if (!is.null(cfg$lss_method))    cfg$lss_method    else "sodium_sbp"

    # --- Task 1d: resolve LSS coverage per country x sex from NCD-RisC ---------
    # s2 (reach "all") uses the whole-population LSS_COVERAGE_ALL scalar; s4/s5 use
    # the NCD-RisC diagnosed_pop / treated_pop POPULATION share (per sex), passed
    # as a data.table(sex, cov). No BP>=140 proxy, no silent fallback to old
    # constants (1a hard-checked every modelled country x sex).
    s_lss_coverage <- if (!is.null(cfg$lss_coverage)) cfg$lss_coverage else 1.0
    if (!is.null(cfg$lss_eligibility) && !is.na(cfg$lss_eligibility)) {
      if (is.null(htn_eligibility)) stop("htn_eligibility not available for LSS coverage.")
      he <- htn_eligibility[location == Country, .(sex, cov = get(cfg$lss_eligibility))]
      if (nrow(he) == 0L || any(is.na(he$cov))) {
        stop("NCD-RisC eligibility (", cfg$lss_eligibility, ") missing for ", Country)
      }
      s_lss_coverage <- he
    } else if (identical(s_lss_reach, "all") && is.null(cfg$lss_coverage)) {
      # Whole-population reach with no explicit per-scenario coverage: fall back to
      # the single default. The lss_s2_<pct> variants DO carry cfg$lss_coverage, so
      # each keeps its own population-coverage level here.
      s_lss_coverage <- lss_coverage_all
    }

    # Scalar salteff (base-year shares) for gating + logging + decomposition.
    s_salteff <- compute_total_efficacy(s_interventions, Country,
                                        source_shares, intervention_effects)
    s_decomp  <- attr(s_salteff, "decomposition")

    # Task 3b: year-specific salteff from the YEAR-INDEXED source shares (packaged
    # -- and via renormalisation, all -- shares vary by year). NULL when the year
    # table is unavailable, in which case project.all uses the scalar (old path).
    s_salteff_year <- if (!is.null(source_shares_by_year))
      compute_salteff_by_year(s_interventions, Country, source_shares_by_year, intervention_effects)
    else NULL

    cat("\n##########################################\n")
    cat("SCENARIO     :", scenario_name, "\n")
    cat("Label        :", s_label, "\n")
    cat("Country      :", Country, "\n")
    cat("Interventions:",
        if (length(s_interventions) == 0L) "(none - baseline)"
        else paste(s_interventions, collapse = " + "), "\n")
    cat(sprintf("salteff(base-year) = %.6f | method = %s | reach = %s\n",
                as.numeric(s_salteff), s_lss_method, s_lss_reach))
    if (!is.null(s_salteff_year)) {
      cat(sprintf("salteff(year-specific) range = [%.6f, %.6f] over %d-%d\n",
                  min(s_salteff_year$salteff), max(s_salteff_year$salteff),
                  min(s_salteff_year$year), max(s_salteff_year$year)))
    }
    cat("Source decomposition (share x composed_effect = contribution):\n")
    print(s_decomp)
    cat("##########################################\n")

    results[[scenario_name]] <- project.all(
      Country      = Country,
      saltmet      = s_saltmet,
      salteff      = as.numeric(s_salteff),
      saltyear1    = s_saltyear1,
      saltyear2    = s_saltyear2,
      lss_reach    = s_lss_reach,
      lss_coverage = s_lss_coverage,
      lss_method   = s_lss_method,
      salteff_year = s_salteff_year
    )
  }

  rbindlist(results, idcol = "scenario")
}

# NOTE: 06's SECTION 10 (static scenario configurations) is intentionally gone:
# scenario_configs is now built dynamically from the user controls in SECTION 3
# (build_scenario_configs), and salteff is resolved per country in SECTION 9.

###############################################################################
# SECTION 11: Comparison and Validation Helpers             (unchanged from 06)----
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
# SECTION 11b: LSS audit + run-config (Task 1e + GENERAL/glue)----
###############################################################################
# The LSS audit is the SINGLE SOURCE OF TRUTH for the report's LSS methods /
# results tables and the Na/K-SBP-vs-SSaSS comparison. One row per country x sex
# x LSS scenario, computed with the SAME constants + inputs the model uses, at
# FULL implementation (2030 shares, ramp = 1). SBP-change columns are the
# representative (population-average raisedBP) quantities; the model itself
# applies them bin-by-bin. For ssass_trial_rr rows the composition (Na displaced,
# K added) is reported for comparison but the SBP columns are NA (that method's
# effect is the trial stroke RRs, not an SBP change).
.nn <- function(x, d = NA) if (is.null(x)) d else x
# UPDATED vs previous implementation: the audit K math now calls the SAME helpers
# and constants the model uses (k_delta_to_sbp_reduction on the Fig-2 delta-uK
# anchors, k_baseline_uk_multiplier on the Fig-S16 baseline-uK subgroup) -- it no
# longer reimplements a Fig-3 achieved-excretion difference or baseline-Na bands.
build_lss_audit <- function(scenario_configs, countries = PRIORITY_COUNTRIES) {
  lss_names <- names(scenario_configs)[
    vapply(scenario_configs, function(c) identical(.nn(c$interventions, ""), "lss"), logical(1))]
  rows <- list()
  for (nm in lss_names) {
    cfg <- scenario_configs[[nm]]
    method <- .nn(cfg$lss_method, "na_k_sbp")
    for (C in countries) {
      base_na    <- as.numeric(data.in[location == C, salt][1])
      disc_share <- as.numeric(source_shares_by_year[location == C & year == 2030, discretionary])
      for (S in c("Male", "Female")) {
        rbp <- data.in[location == C & sex == S, mean(raisedBP, na.rm = TRUE)]
        bk  <- baseline_potassium[location == C & sex == S]
        # eligibility group + coverage (Task 1d)
        if (!is.null(cfg$lss_eligibility) && !is.na(cfg$lss_eligibility)) {
          elig_grp <- cfg$lss_eligibility
          cov <- as.numeric(htn_eligibility[location == C & sex == S, get(elig_grp)])
        } else {
          elig_grp <- "whole_population"
          cov <- as.numeric(.nn(cfg$lss_coverage, LSS_COVERAGE_ALL))
        }
        # Na displacement at full implementation (+ WHO 2 g/day floor).
        salt_target <- base_na * disc_share * EFF_LSS_NA_K_DISCRETIONARY * cov
        if (base_na - salt_target < 2) salt_target <- max(0, base_na - 2)
        na_disp   <- salt_target
        # (B) baseline dietary K (g/d) -> baseline 24h URINARY K (mmol/d); the /1.3
        #     is applied because k_intake_g is a DIETARY intake.
        uk0        <- (bk$k_intake_g * 1000 / K_MG_PER_MMOL) / K_INTAKE_TO_EXCRETION
        # (C) K added (g/d) -> dietary mmol/d -> CHANGE in urinary K (mmol/d).
        k_added_g  <- na_disp * (K_PER_G_KCL / NA_PER_G_NACL)
        k_add_intake_mmol <- k_added_g * 1000 / K_MG_PER_MMOL
        delta_uk   <- k_add_intake_mmol / K_INTAKE_TO_EXCRETION
        uk_group   <- if (uk0 < LSS_K_BASELINE_UK_THRESHOLD) "<75" else ">=75"
        uk_mult    <- k_baseline_uk_multiplier(uk0)                # (E) Fig-S16 modifier
        na_dsbp    <- ((2.8 * rbp) + (1 - rbp) * 1.0) * na_disp    # (A) Na channel
        k_dsbp_un  <- k_delta_to_sbp_reduction(delta_uk)           # (D) Fig-2 unmodified
        k_dsbp     <- k_dsbp_un * uk_mult                          # (E) modified
        is_nak    <- method == "na_k_sbp"
        rows[[length(rows) + 1L]] <- data.table(
          scenario = nm, location = C, sex = S, method = method,
          eligibility_group = elig_grp, eligible_pop_share = cov,
          baseline_sodium_g = base_na, baseline_potassium_g = bk$k_intake_g,
          baseline_urinary_potassium_mmol = uk0,
          baseline_uk_group = uk_group, baseline_uk_multiplier = uk_mult,
          potassium_source = bk$source, discretionary_share = disc_share,
          nacl_fraction = LSS_NACL_FRACTION, kcl_fraction = LSS_KCL_FRACTION,
          uptake = LSS_UPTAKE, adherence = LSS_ADHERENCE,
          sodium_displaced_g = na_disp, potassium_added_g = k_added_g,
          potassium_added_intake_mmol = k_add_intake_mmol,
          # documented backward-compat alias (dietary mmol/d, == potassium_added_intake_mmol)
          potassium_added_mmol = k_add_intake_mmol,
          delta_urinary_potassium_mmol = delta_uk,
          delta_uk_exceeds_zero_crossing = delta_uk > LSS_K_DELTA_UK_ZERO_CROSSING,
          sbp_delta_na_mmHg      = if (is_nak) na_dsbp   else NA_real_,
          sbp_delta_k_unmod_mmHg = if (is_nak) k_dsbp_un else NA_real_,
          sbp_delta_k_mmHg       = if (is_nak) k_dsbp    else NA_real_,
          sbp_delta_combined_mmHg = if (is_nak)
            na_dsbp + (if (isTRUE(LSS_NAK_ADDITIVE)) LSS_ADDITIVITY_FACTOR else 0) * k_dsbp
            else NA_real_,
          additivity_factor = LSS_ADDITIVITY_FACTOR,
          causes_affected = if (is_nak) "ihd, istroke, hstroke, hhd (all CVD via SBP)"
                            else "istroke, hstroke (stroke only, trial RR)",
          harms_modelled = "none (no CKD/hyperkalaemia channel)")
      }
    }
  }
  rbindlist(rows)
}
lss_audit <- build_lss_audit(scenario_configs)
saveRDS(lss_audit, file = paste0(wd_data, "lss_audit.rds"))
cat(sprintf("\nTask 1e: lss_audit written (%d rows: %d LSS scenarios x %d countries x 2 sexes).\n",
            nrow(lss_audit), length(unique(lss_audit$scenario)), length(PRIORITY_COUNTRIES)))

# --- run_config.rds: the values report.RMD must NOT re-guess (GENERAL) --------
scenario_registry <- rbindlist(lapply(names(scenario_configs), function(nm) {
  c <- scenario_configs[[nm]]
  ivs <- .nn(c$interventions, character(0))
  data.table(
    scenario        = nm,
    label           = .nn(c$label, nm),
    interventions   = if (length(ivs) == 0L) "" else paste(ivs, collapse = " + "),
    is_lss          = identical(.nn(c$interventions, ""), "lss"),
    is_fiscal       = grepl("^fiscal", nm),
    is_package      = grepl("^full_package", nm),
    lss_method      = .nn(c$lss_method),
    lss_reach       = .nn(c$lss_reach),
    lss_eligibility = .nn(c$lss_eligibility),
    # Scalar whole-population coverage for the lss_s2_<pct> variants (NA for s4/s5,
    # whose coverage is the per-country x sex NCD-RisC eligibility, and for non-LSS
    # scenarios). Lets the report build the Scenario-2 uptake sweep dynamically.
    lss_coverage    = if (is.null(c$lss_coverage) || is.data.frame(c$lss_coverage))
                        NA_real_ else as.numeric(c$lss_coverage))
}))
run_config <- list(
  note        = "Written by 07_run_interventions.R; read by report.RMD. Do not re-guess these.",
  lss = list(
    method = LSS_METHOD, benchmark_ssass = LSS_BENCHMARK_SSASS,
    # Overall LSS scenario identifier stays "na_k_sbp"; the K submethod names the
    # updated potassium pathway so readers can distinguish it without renaming.
    potassium_method = "filippini_fig2_delta_uk_baseline_uk_modifier",
    nacl_fraction = LSS_NACL_FRACTION, kcl_fraction = LSS_KCL_FRACTION,
    uptake = LSS_UPTAKE, adherence = LSS_ADHERENCE,
    additive = LSS_NAK_ADDITIVE, additivity_factor = LSS_ADDITIVITY_FACTOR,
    coverage_all = LSS_COVERAGE_ALL, s2_coverage_levels = LSS_S2_COVERAGE_LEVELS,
    k_mg_per_mmol = K_MG_PER_MMOL, k_intake_to_excretion = K_INTAKE_TO_EXCRETION,
    na_per_g_nacl = NA_PER_G_NACL, k_per_g_kcl = K_PER_G_KCL,
    # UPDATED vs previous implementation: Fig-2 change-in-urinary-K anchors +
    # Fig-S16 baseline urinary-K subgroup modifier (replacing the Fig-3 achieved-
    # excretion curve, baseline-Na bands and non-HTN attenuation).
    k_delta_sbp_anchors    = LSS_K_DELTA_SBP_ANCHORS,
    baseline_uk_threshold  = LSS_K_BASELINE_UK_THRESHOLD,
    baseline_uk_mult_low   = LSS_K_BASELINE_MULT_LOW,
    baseline_uk_mult_high  = LSS_K_BASELINE_MULT_HIGH,
    baseline_uk_subgroup_sbp = LSS_K_SUBGROUP_SBP,
    delta_uk_zero_crossing = LSS_K_DELTA_UK_ZERO_CROSSING,
    # The retired Fig-3 / baseline-Na / non-HTN modifiers are NOT active in the
    # primary pathway (deleted, not merely disabled).
    previous_modifiers_active = FALSE,
    eff_lss_na_k_discretionary = EFF_LSS_NA_K_DISCRETIONARY,
    eff_lss_sodium_only = EFF_LSS_SODIUM_ONLY,
    harms_modelled = "none (no CKD/hyperkalaemia channel)"),
  htn_source_year = HTN_SOURCE_YEAR,
  fiscal = list(
    sensitivity = FISCAL_SENSITIVITY, include_in_package = include_fiscal_in_package,
    base = list(tax_rate = FISCAL_TAX_RATE, pass_through = FISCAL_PASS_THROUGH,
                elasticity = FISCAL_ELASTICITY, taxable_share = FISCAL_TAXABLE_SHARE,
                substitution = FISCAL_SUBSTITUTION, reformulation = FISCAL_REFORMULATION),
    base_effect = EFF_FISCAL_PACKAGED, status = "exploratory"),
  euromonitor = list(
    window = EUROMONITOR_WINDOW, recombine_from_leaves = EUROMONITOR_RECOMBINE_FROM_LEAVES,
    base_year = EUROMONITOR_BASE_YEAR, ceiling = EUROMONITOR_PACKAGED_CEILING,
    post2030 = EUROMONITOR_POST2030, composition_only = EUROMONITOR_COMPOSITION_ONLY,
    run_packaged_trend = run_packaged_trend),
  scaleup_window   = c(SCALEUP_YEAR1, SCALEUP_YEAR2),
  impact_years     = IMPACT_YEARS,
  analysis_years   = c(ANALYSIS_YEAR_MIN, PROJECTION_YEAR_MAX),
  priority_countries = PRIORITY_COUNTRIES,
  scenarios        = scenario_registry,
  # Authoritative source-split inputs so the report can DISPLAY the salteff
  # decomposition using the SAME effect sizes + base shares the pipeline used,
  # WITHOUT maintaining a second copy of the model (Task 4f).
  intervention_effects = copy(intervention_effects),
  source_shares_base   = copy(source_shares),
  sodium_sources       = SODIUM_SOURCES
)
saveRDS(run_config, file = paste0(wd_data, "run_config.rds"))
cat("GENERAL: run_config.rds written (",
    nrow(scenario_registry), " scenarios; LSS method = ", LSS_METHOD,
    "; fiscal in package = ", include_fiscal_in_package, ").\n", sep = "")

###############################################################################
# SECTION 12: Parallel Execution  - all scenarios x all countries----
###############################################################################

# --- Cluster parameters ------------------------------------------------------
ncores <- 6
cl     <- makeCluster(ncores)
registerDoParallel(cl)

# Export all objects required by workers.
#   Dropped vs 06 : default_sodium_policy_table, summarize_sodium_policy_package
#   Added   vs 06 : compute_total_efficacy, source_shares, intervention_effects
#   Added (Tasks 1/3): compute_salteff_by_year, k_delta_to_sbp_reduction,
#     k_baseline_uk_multiplier, source_shares_by_year, htn_eligibility,
#     baseline_potassium, LSS_PARAMS, LSS_COVERAGE_ALL -- anything a worker touches
#     must be here (run_multiple_scenarios resolves year-specific salteff + per-sex
#     coverage on the worker, and calculate_sodium_impact_etihad reads LSS_PARAMS/
#     baseline_potassium and calls k_delta_to_sbp_reduction + k_baseline_uk_multiplier).
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
    # Source-split intervention logic
    "compute_total_efficacy",
    "compute_salteff_by_year",
    "source_shares",
    "source_shares_by_year",
    "intervention_effects",
    # LSS Na/K->SBP inputs + helpers (Task 1). UPDATED: the Fig-2 delta-uK dose-
    # response + Fig-S16 baseline-uK multiplier helpers replace k_excretion_to_sbp.
    "k_delta_to_sbp_reduction",
    "k_baseline_uk_multiplier",
    "LSS_PARAMS",
    "baseline_potassium",
    "htn_eligibility",
    "LSS_COVERAGE_ALL",
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

# Prioritized locations
locs <- c("Viet Nam", "Philippines", "Bangladesh", "China","Ethiopia",
          "India", "Malaysia", "Thailand","Cameroon","Nigeria")

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

