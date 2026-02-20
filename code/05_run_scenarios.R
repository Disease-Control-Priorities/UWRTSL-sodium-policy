
# #...........................................................
# # Interventions and Targets ----
# #...........................................................

#...........................................................
## GBD Relative Risks Setup ----
#...........................................................

# Load and prepare GBD relative risks (same as OLD model)
dt_gbd_rr <- as.data.table(read_excel(paste0(wd_raw, "IHME_GBD_2019_RELATIVE_RISKS_Y2020M10D15_HTN.xlsx"),
                                      sheet = "Sheet1", range = "A3:AB20"))

dt_gbd_rr[, c("Category / Units", "Morbidity / Mortality", "Sex", "All-age") := NULL]
dt_gbd_rr[, `20-24 years` := `25-29 years`]
dt_gbd_rr[, (2:8) := NULL]

dt_gbd_rr <- melt(dt_gbd_rr,
                  id.vars = c("Risk-Outcome"),
                  variable.name = "age",
                  value.name = "rr_per_10mmhg")

dt_gbd_rr[, age := gsub(" years", "", age)]
dt_gbd_rr[, rr_per_10mmhg := as.numeric(sub("^\\s*([0-9.]+).*", "\\1", rr_per_10mmhg))]

dt_gbd_rr[, cause := fcase(
  `Risk-Outcome` == "Ischaemic heart disease", "ihd",
  `Risk-Outcome` == "Ischaemic stroke", "istroke",
  `Risk-Outcome` == "Intracerebral hemorrhage", "hstroke",
  `Risk-Outcome` == "Hypertensive heart disease", "hhd",
  default = NA_character_
)]
dt_gbd_rr[, `Risk-Outcome` := NULL]
dt_gbd_rr <- dt_gbd_rr[cause %in% c("ihd", "istroke", "hstroke", "hhd")]

# Expand age groups to single years
expand_age <- function(age_group) {
  if (grepl("\\+", age_group)) {
    start <- as.numeric(sub("\\+", "", age_group))
    return(start:95)
  } else {
    bounds <- as.numeric(unlist(strsplit(age_group, "-")))
    return(bounds[1]:bounds[2])
  }
}

dt_expanded <- dt_gbd_rr[, .(age_single = expand_age(age)), by = .(age, rr_per_10mmhg, cause)]
dt_expanded[, age_single := as.integer(age_single)]
dt_expanded[, age := age_single]
dt_expanded[, age_single := NULL]
dt_gbd_rr <- copy(dt_expanded)

#...........................................................
## Helpers - Reusable components ----
#...........................................................

# # ETIHAD relative risks for 10 mmHg BP reduction
# ETIHAD_RR <- data.table(
#   cause = c("ihd", "hhd", "istroke", "hstroke", "aod"),
#   rr_per_10mmhg = c(0.83, 0.72, 0.73, 0.73, 0.93)
# )

ETIHAD_RR <- fread(paste0(wd_data, "ettehad_rr_bp_reduction_10mmHg.csv"))

# rename to cause column
ETIHAD_RR[, cause := fcase(
  Cause == "Coronary heart disease", "ihd",
  Cause == "Heart failure", "hhd",
  Cause == "Stroke", "istroke",
  default = NA_character_
)]

# keep only relevant causes
ETIHAD_RR <- ETIHAD_RR[cause %in% c("ihd", "hhd", "istroke","hstroke"),
                       c("cause","SBP_Category","RR"),with=F]

# hstroke from istroke
etihad_hstroke_rr <- ETIHAD_RR[cause=="istroke",]
etihad_hstroke_rr[, cause := "hstroke"]
ETIHAD_RR <- rbind(ETIHAD_RR, etihad_hstroke_rr)

#remove bp total
ETIHAD_RR <- ETIHAD_RR[SBP_Category != "Total", ]

#rename columns
setnames(ETIHAD_RR, c("SBP_Category", "RR"), c("bp_cat", "rr_per_10mmhg"))

# Standard BP categories used in model
bp_full <- c("<120", "120-129", "130-139", "140-149",
             "150-159", "160-169", "170-179", "180+")

# mapping function
map_bp <- function(x){
  fcase(
    x %in% c("<120", "120-129", "<130")        , "<130",
    x == "130-139"                             , "130-139",
    x == "140-149"                             , "140-149",
    x == "150-159"                             , "150-159",
    x %in% c("160-169","170-179","180+","≥160"), ">=160"
  )
}

# build mapping table
bp_map <- data.table(
  bp_cat_full = bp_full,
  bp_cat = map_bp(bp_full)
)

# merge only on matching categories (carry forward RR)
expanded <- bp_map[
  ETIHAD_RR,
  on = .(bp_cat),
  allow.cartesian = TRUE
][
  , .(cause, bp_cat_full, rr = rr_per_10mmhg)
][
  order(cause, bp_cat_full)
]

ETIHAD_RR <- copy(expanded)

#rename etihad
setnames(ETIHAD_RR, c("bp_cat_full", "rr"), c("bp_cat", "rr_per_10mmhg"))

# cleaning
rm(expanded, etihad_hstroke_rr)

# Calculate coverage for a given year based on linear scale-up
# from start_year (0% coverage) to target_year (target_coverage%)
calculate_coverage_by_year <- function(year, 
                                       start_year = 2026, 
                                       target_year = 2050,
                                       target_coverage = 0.50) {
  
  # Years elapsed since start
  
  # # original
  # years_elapsed <- pmax(0, year - start_year)
  
  # no delay
  years_elapsed <- pmax(0, year - start_year + 1)
  
  # Total years in scale-up period
  # # original
  # total_years <- target_year - start_year
  
  # no delay
  total_years <- target_year - start_year + 1
  
  # Linear interpolation
  coverage <- pmin(target_coverage * years_elapsed / total_years, 
                   target_coverage)
  
  # Before start_year, coverage is 0
  coverage[year < start_year] <- 0
  
  # After target_year, coverage is at target
  coverage[year > target_year] <- target_coverage
  
  return(coverage)
}

# Vectorized version for data.table
add_coverage_by_year <- function(dt, 
                                 year_col = "year",
                                 start_year = 2026,
                                 target_year = 2050,
                                 target_coverage = 0.50,
                                 coverage_col = "coverage_t") {
  
  dt[, (coverage_col) := calculate_coverage_by_year(
    get(year_col), 
    start_year, 
    target_year, 
    target_coverage
  )]
  
  return(dt)
}

# Calculate weighted average coverage across hypertensive BP bins
# Used for case fatality calculations
calculate_aggregate_coverage <- function(dt, 
                                         hypertensive_bins = c("140-149", "150-159", 
                                                               "160-169", "170-179", "180+"),
                                         bp_col = "bp_cat",
                                         coverage_col = "coverage_t",
                                         prob_col = "prob",
                                         grouping_vars = c("age", "sex", "location", 
                                                           "cause", "year"),
                                         hypertensive_only = NULL) {
  
  # hypertensive_only = NULL then use all bins (default behavior)
  # hypertensive_only = TRUE then use only hypertensive bins
  # hypertensive_only = FALSE then explicitly use all bins
  
  
  dt_use <- copy(dt)
  
  if (!is.null(hypertensive_only) && hypertensive_only == TRUE) {
    dt_use <- dt_use[get(bp_col) %in% hypertensive_bins]
  }
  
  # Compute weighted mean coverage
  coverage_agg <- dt_use[, 
                         .(
                           coverage_agg = weighted.mean(get(coverage_col), 
                                                        get(prob_col), 
                                                        na.rm = TRUE)
                         ),
                         by = grouping_vars
  ]
  
  return(coverage_agg)
}

# Import from excel .xlsx file ettehad_rr_bp_reduction_effects
ETIHAD_RR_BIN<- as.data.table(read_excel(paste0(wd_data, "ettehad_rr_bp_reduction_effects.xlsx"), 
                                         sheet = "Sheet1")) 

# Calculate cumulative ETIHAD effect size for BP bins
# For each bin, calculate cumulative effect of reducing BP by 10 mmHg steps

calculate_etihad_cumulative_rr <- function(bp_cat, 
                                           cause_name, 
                                           diabetes_weight = 0.1,
                                           etihad_rr_table = ETIHAD_RR_BIN) {
  
  # Input lengths must match
  if (length(bp_cat) != length(cause_name)) {
    stop("bp_cat and cause_name must have the same length")
  }
  
  # Build key for matching
  lookup_key <- paste(cause_name, bp_cat, sep = "_")
  table_key  <- paste(etihad_rr_table$cause, etihad_rr_table$bp_cat, sep = "_")
  
  # Indices in lookup table
  idx <- match(lookup_key, table_key)
  
  if (any(is.na(idx))) {
    stop("Some bp_cat–cause combinations were not found in ETIHAD_RR")
  }
  
  # Extract effect sizes from table
  effect_no_diab <- etihad_rr_table$effect_size_nodiabetes[idx]
  effect_diab    <- etihad_rr_table$effect_size_diabetes[idx]
  
  # Weighted average
  effect_weighted <- 
    (1 - diabetes_weight) * effect_no_diab +
    diabetes_weight   * effect_diab
  
  return(effect_weighted)
}


# Load GBD relative risks (per 10 mmHg increase)
# Expected format: columns for age, sex, cause, rr_per_10mmhg
# dt_gbd_rr <- readRDS("path/to/gbd_rr_data.rds")

# Assign GBD relative risks based on BP category
# RRs are relative to <120 mmHg reference
get_gbd_relative_risks <- function(bp_cat, age, cause, dt_gbd_rr = NULL) {
  
  if (is.null(dt_gbd_rr)) {
    dt_gbd_rr <- get("dt_gbd_rr", envir = .GlobalEnv)
  }
  
  # Calculate midpoint SBP for each category
  sbp_midpoint <- case_when(
    bp_cat == "<120" ~ 110,
    bp_cat == "120-129" ~ 125,
    bp_cat == "130-139" ~ 135,
    bp_cat == "140-149" ~ 145,
    bp_cat == "150-159" ~ 155,
    bp_cat == "160-169" ~ 165,
    bp_cat == "170-179" ~ 175,
    bp_cat == "180+" ~ 185,
    TRUE ~ NA_real_
  )
  
  # 2. Increment from 120
  inc_10 <- (sbp_midpoint - 120) / 10
  
  # 3. Merge single-row-by-row using a fast join
  # Create temporary table with input values
  tmp <- data.table(age = age, cause = cause)
  
  # Join RRs
  tmp <- dt_gbd_rr[tmp, on = c("age", "cause")]
  
  rr10 <- tmp$rr_per_10mmhg
  
  # 4. Compute RR
  rr <- ifelse(inc_10 > 0, rr10^inc_10, 1)
  
  return(rr)
}

# Expand age groups to single-year ages
expand_to_single_year_ages <- function(dt) {
  dt[, age := as.numeric(substr(age, 1, 2))]
  dt <- dt[rep(seq_len(nrow(dt)), each = 5)]
  dt[, age2 := rep(1:5, nrow(dt)/5)][, age := age + age2 - 1]
  
  over90 <- dt[age == 89]
  over90 <- over90[rep(seq_len(nrow(over90)), each = 6)]
  over90[, age2 := rep(1:6, nrow(over90)/6)][, age := age + age2]
  
  rbindlist(list(dt, over90))[, age2 := NULL]
}

# Calculate BP probabilities with optional treatment effect
get.bp.prob <- function(DT, rx, drugaroc = "baseline") {
  # Select appropriate coverage increment variable
  cov_var <- switch(
    drugaroc,
    "baseline" = "aroc2",
    "p75" = "p_change2",
    "p975" = "a_change2",
    "ideal" = "ideal",
    stop("Invalid 'drugaroc' argument. Must be one of: baseline, p75, p975, ideal")
  )
  
  # Apply coverage increment only if antihypertensive treatment (rx == 1)
  DT[, covinc := if (rx == 1) get(cov_var) else aroc2]
  
  # Patch missing coverage increments to 0 ommit aroc
  DT[, covinc := 0]
  
  # Define BP cutpoints
  bp_breaks <- c(-Inf, 120, 130, 140, 150, 160, 170, 180, Inf)
  bp_labels <- c("<120", "120-129", "130-139", "140-149", 
                 "150-159", "160-169", "170-179", "180+")
  
  # Compute BP category probabilities using vectorized operations
  for (i in seq_along(bp_labels)) {
    lower <- bp_breaks[i]
    upper <- bp_breaks[i + 1]
    DT[bp_cat == bp_labels[i], 
       prob := pnorm(upper, Mean, stdev) - pnorm(lower, Mean, stdev)]
  }
  
  # Adjust probabilities for antihypertensive treatment coverage
  if (rx == 1) {
    DT[, shift := prob * covinc]
    DT[bp_cat %in% c("<120", "120-129", "130-139"), shift := 0]
    
    # Compute reallocation between BP bins by diabetes status
    DT[, add130 := sum(shift * diabetes), by = .(age, sex, Year)]
    DT[, add140 := sum(shift * (1 - diabetes)), by = .(age, sex, Year)]
    
    # Update category probabilities
    DT[, prob := prob - shift]
    DT[bp_cat == "120-129", prob := prob + add130]
    DT[bp_cat == "130-139", prob := prob + add140]
  }
  
  # Return relevant variables
  return(DT[, .(age, sex, Year, bp_cat, prob, location)])
}

# Calculate baseline incidence rates using GBD RRs
calculate_baseline_incidence_gbd <- function(bp_prob, intervention_rates, 
                                             Country, dt_gbd_rr) {
  cat("  - Calculating baseline incidence with GBD RRs\n")
  
  # Expand to single-year ages
  bp_prob <- expand_to_single_year_ages(bp_prob)
  
  # Add GBD relative risks for all causes
  causes <- c("ihd", "hhd", "istroke", "hstroke", "aod")
  
  for (cause in causes) {
    col_name <- paste0("RRi_", toupper(cause))
    bp_prob[, (col_name) := get_gbd_relative_risks(bp_cat, age,
                                                   cause, dt_gbd_rr)]
  }
  
  # Calculate alphas (normalization factors)
  # alpha = sum(prob * RR) across all BP categories
  alphas <- bp_prob[, .(
    ihd = sum(prob * RRi_IHD),
    istroke = sum(prob * RRi_ISTROKE),
    hstroke = sum(prob * RRi_HSTROKE),
    hhd = sum(prob * RRi_HHD),
    aod = sum(prob * RRi_AOD)
  ), by = .(age, sex, location, Year)]
  
  alphas <- melt(alphas, id.vars = c("age", "sex", "location", "Year"),
                 variable.name = "cause", value.name = "alpha")
  
  # Prepare RRi data (long format)
  rris <- bp_prob[, .(age, sex, Year, location, bp_cat, prob, 
                      RRi_IHD, RRi_HHD, RRi_ISTROKE, RRi_AOD)]
  rris[, RRi_HSTROKE := RRi_ISTROKE]
  
  setnames(rris, 
           c("RRi_IHD", "RRi_HHD", "RRi_ISTROKE", "RRi_HSTROKE", "RRi_AOD"),
           c("ihd", "hhd", "istroke", "hstroke", "aod"))
  
  rris <- melt(rris, id.vars = c("age", "sex", "location", "bp_cat", "prob", "Year"),
               variable.name = "cause", value.name = "RRi")
  
  # Merge with alphas
  bp_prob_full <- merge(rris, alphas, 
                        by = c("age", "sex", "location", "cause", "Year"))
  setnames(bp_prob_full, "Year", "year")
  
  # Merge with intervention rates
  dt <- merge(intervention_rates[location == Country], bp_prob_full,
              by = c("age", "sex", "location", "cause", "year"))
  
  # Calculate BP bin-specific baseline incidence: IR_bin = (RRi * IR) / alpha
  dt[, IR_bin := (RRi * IR) / alpha]
  
  return(dt)
}

# # Calculate Ettehad-based effect size for BP reduction

apply_coverage_adjustment <- function(effect_size, 
                                      coverage_t, 
                                      coverage_0 = 0) {
  
  if (all(coverage_0 == 0)) {
    # Simplified formula when baseline coverage is 0
    return(effect_size * coverage_t)
  } else {
    # Full formula when baseline coverage > 0
    numerator <- effect_size * (coverage_t - coverage_0)
    denominator <- 1 - effect_size * coverage_0
    return(numerator / denominator)
  }
}
#...........................................................
## Anti hypertensive therapy ----
#...........................................................

dt_hbp_control <- readRDS(file = paste0(wd_data,"hbp_control_data.rds"))

calculate_antihypertensive_impact_etihad <- function(intervention_rates, 
                                                     Country, 
                                                     DT.in,
                                                     dt_gbd_rr,
                                                     target_control = 0.50,
                                                     drugcov = "p75",
                                                     start_year = 2026,
                                                     target_year = 2050,
                                                     baseline_ctrl = 0) {
  cat(" - Calculating antihypertensive impact using ETIHAD effect sizes\n")
  
  # Clamp baseline control to [0,1]
  baseline_ctrl <- max(min(baseline_ctrl, 1), 0)
  
  # Step 1: Get baseline BP distribution (no treatment)
  bp_prob_base <- get.bp.prob(DT.in, rx = 0, drugaroc = "baseline")
  
  # Step 2: Calculate baseline bin-specific incidence using GBD RRs
  dt_baseline <- calculate_baseline_incidence_gbd(
    copy(bp_prob_base), intervention_rates, Country, dt_gbd_rr
  )
  
  # Step 3: Add ETIHAD effect sizes for each BP bin and cause
  causes  <- c("ihd", "hhd", "istroke", "hstroke", "aod")
  bp_cats <- c("<120", "120-129", "130-139", "140-149", 
               "150-159", "160-169", "170-179", "180+")
  
  # Build ETIHAD effect-size table (via diabetes-weighted effects)
  etihad_effects <- dt_baseline[, .(N = mean(pop)),
                                by = .(location, year, age, sex, bp_cat, cause)]
  
  diabetes_prop <- expand_to_single_year_ages(DT.in)
  diabetes_prop <- diabetes_prop[, .(location, Year, age, sex, bp_cat, diabetes)]
  setnames(diabetes_prop, "Year", "year")
  
  etihad_effects <- merge(etihad_effects, diabetes_prop, all.x = TRUE)
  etihad_effects[, etihad_effect := 
                   calculate_etihad_cumulative_rr(bp_cat, cause,
                                                  diabetes_weight = diabetes)]
  etihad_effects[, c("diabetes", "N") := NULL]
  
  dt_baseline <- merge(
    dt_baseline, etihad_effects,
    by = c("location", "year", "age", "sex", "bp_cat", "cause"),
    all.x = TRUE
  )
  
  # # Step 4: Coverage Scale-Up
  # 4. Coverage scale-up (NEW LOGIC)
  
  # Increment relative to baseline control
  incr_target <- max(target_control - baseline_ctrl, 0)
  
  # Build scale-up curve as if baseline = 0 then incr_target
  dt_baseline <- add_coverage_by_year(
    dt_baseline,
    year_col        = "year",
    start_year      = start_year,
    target_year     = target_year,
    target_coverage = incr_target,
    coverage_col    = "coverage_increment"
  )
  
  # Hypertensive BP bins eligible for treatment
  hypertensive_bins <- c("140-149", "150-159",
                         "160-169", "170-179", "180+")
  
  # Initialize coverage variables
  dt_baseline[, `:=`(coverage_0 = 0, coverage_t = 0)]
  
  # Apply baseline and scale-up to hypertensive bins only
  dt_baseline[bp_cat %in% hypertensive_bins,
              `:=`(
                coverage_0 = baseline_ctrl,
                coverage_t = baseline_ctrl + coverage_increment
              )]
  
  # After target year, hold at final coverage
  dt_baseline[year > target_year & bp_cat %in% hypertensive_bins,
              coverage_t := baseline_ctrl + incr_target]
  
  # Bound between baseline and 1
  dt_baseline[bp_cat %in% hypertensive_bins,
              coverage_t := pmin(pmax(coverage_t, baseline_ctrl), 1)]
  
  # Non-hypertensive bins: no coverage
  dt_baseline[!bp_cat %in% hypertensive_bins,
              `:=`(coverage_t = 0, coverage_0 = 0)]
  
  dt_baseline[, coverage_increment := NULL]
  
  
  # Step 5: Apply coverage-adjusted ETIHAD effect sizes using helper
  # effect_size_t is the *incremental* effect from moving from coverage_0 to coverage_t
  dt_baseline[, effect_size_t := apply_coverage_adjustment(
    etihad_effect,
    coverage_t,
    coverage_0 = coverage_0
  )]
  
  # Step 6: New bin-specific incidence
  dt_baseline[, IR_bin_new := IR_bin * (1 - effect_size_t)]
  
  # Step 7: Population-weighted average incidence
  dt_baseline[, IR_new := sum(IR_bin_new * prob),
              by = .(age, sex, location, cause, year)]
  
  # If no scale up keep flat
  #baseline_ctrl
  if(baseline_ctrl>=target_control){
    dt_baseline[, IR_new := IR]
  }
  
  # Prior to the intervention start, force baseline
  dt_baseline[year < start_year, IR_new := IR]
  
  # Step 8: Effect ratio for incidence
  
  dt_baseline[, eff_ir := IR_new / IR]
  
  # Step 9: Case fatality reduction – use *incremental* coverage,
  # not the absolute coverage, so baseline control isn't double counted.
  cf_etihad <- data.table(
    cause = c("ihd", "istroke", "hstroke", "hhd", "aod"),
    cf_reduction_per_control = c(0.24, 0.36, 0.76, 0.20, 0.047)
  )
  
  dt_baseline <- merge(dt_baseline, cf_etihad, by = "cause", all.x = TRUE)
  
  # incremental coverage above baseline
  dt_baseline[, coverage_delta := pmax(coverage_t - coverage_0, 0)]
  
  coverage_aggregate <- calculate_aggregate_coverage(
    dt_baseline,
    hypertensive_bins = hypertensive_bins,
    bp_col       = "bp_cat",
    coverage_col = "coverage_delta",
    prob_col     = "prob",
    grouping_vars = c("age", "sex", "location", "cause", "year"),
    hypertensive_only = TRUE
  )
  
  dt_baseline <- merge(
    dt_baseline, coverage_aggregate,
    by = c("age", "sex", "location", "cause", "year"),
    all.x = TRUE
  )
  dt_baseline[is.na(coverage_agg), coverage_agg := 0]
  
  # Apply CF reduction only to the incremental coverage
  dt_baseline[, CF_new := CF * (1 - cf_reduction_per_control * coverage_agg)]
  dt_baseline[cause == "aod" & age < 60, CF_new := CF]  # keep your original restriction
  dt_baseline[, eff_cf := CF_new / CF]
  
  # Step 10: Collapse to final output (remove BP bin dimension)
  dt_final <- unique(dt_baseline[, .(
    age, sex, location, cause, year,
    IR = IR_new, CF = CF_new,
    BG.mx, BG.mx.all, PREVt0, DIS.mx.t0, Nx, ALL.mx,
    eff_ir, eff_cf
  )])
  
  setorder(dt_final, year, sex, location, cause, age)
  
  cat("  - ETIHAD effect sizes applied successfully\n")
  cat("  - Baseline control =", baseline_ctrl, 
      "; target control =", target_control,
      "by", target_year, "\n")
  
  return(dt_final)
  
}


#...........................................................
## Sodium reduction  ----
#...........................................................
# Prepare sodium data (run once at setup)
## Sodium reduction
# prepare_sodium_data <- function(data.in, wd_data) {
#   dt_sodium_scenarios <- readRDS(file = paste0(wd_data, "Sodium/", "sodium_policy_scenarios.rds"))
#   dt_sodium_scenarios <- dt_sodium_scenarios[year == 2024, .(location, sodium_current)]
#   
#   data.in <- merge(data.in, dt_sodium_scenarios, by = "location", all.x = TRUE)
#   data.in[!is.na(sodium_current), salt := sodium_current * 2.5]
#   data.in[, sodium_current := NULL]
#   
#   return(data.in)
# }

prepare_sodium_data <- function(data.in, wd_data) {
  dt_sodium_scenarios <- readRDS(file = paste0(wd_data,"sodium_policy_scenarios.rds"))
  dt_sodium_scenarios <- dt_sodium_scenarios[year == 2024, .(location, sodium_current)]
  
  data.in <- merge(data.in, dt_sodium_scenarios, by = "location", all.x = TRUE)
  data.in[!is.na(sodium_current), salt := sodium_current]
  data.in[, sodium_current := NULL]
  
  return(data.in)
}

data.in <- prepare_sodium_data(data.in,wd_data)

apply_salt_reduction <- function(DT.in, salteff, saltmet, saltyear1 = 2026, saltyear2) {
  if (saltmet == "percent") {
    DT.in[, salt_target := salt * (1 - salteff)]
    DT.in[salt_target<2, salt_target:=2]
    DT.in[salt > 0, salt := salt - salt_target]
    DT.in[salt < 2, salt := 2]
  } else if (saltmet == "target") {
    DT.in[, salt := salt - salteff]
    DT.in[salt < 0, salt := 0]
  } else if (saltmet == "app") {
    DT.in[, salt := salteff]
  }
  
  if (salteff != 0) {
    DT.in[Year >= saltyear1 & Year <= saltyear2, 
          Mean := Mean - (((2.8 * raisedBP) + ((1 - raisedBP) * 1.0)) * 
                            salt * (Year - saltyear1 + 1) / (saltyear2 - saltyear1 + 1))]
    
    DT.in[Year > saltyear2, 
          Mean := Mean - (((2.8 * raisedBP) + ((1 - raisedBP) * 1.0)) * salt)]
  }
  
  return(DT.in)
}


# Sodium should also use ETIHAD effect sizes for consistency
calculate_sodium_impact_etihad <- function(intervention_rates, 
                                           Country, 
                                           DT.in, 
                                           salteff,
                                           saltmet,
                                           saltyear1 = 2026,
                                           saltyear2 = 2050,
                                           dt_gbd_rr) {
  cat(" - Calculating sodium impact using ETIHAD effect sizes\n")
  
  # Step 1: Get baseline BP distribution (no sodium intervention)
  bp_prob_base <- get.bp.prob(DT.in, rx = 0, drugaroc = "baseline")
  
  # Step 2: Calculate baseline bin-specific incidence using GBD RRs
  dt_baseline <- calculate_baseline_incidence_gbd(
    copy(bp_prob_base), intervention_rates, Country, dt_gbd_rr
  )
  
  # Step 3: Calculate sodium reduction and BP shift over time
  # Merge salt data from DT.in
  salt_info <- unique(DT.in[, .(age, sex, salt, raisedBP, Year,aroc)])
  setnames(salt_info, "Year", "year")
  
  # function to split age "20-24" into 20:24
  expand_age <- function(x){
    if (x == "85plus") return(85:95)  # adjust as needed
    bounds <- as.numeric(unlist(strsplit(x, "-")))
    seq(bounds[1], bounds[2])
  }
  
  # expand table
  dt_expanded <- salt_info[, .(
    age_single = expand_age(age)
  ), by = .(age, sex, salt, raisedBP,aroc,year)]
  
  # reorder columns
  dt_expanded <- dt_expanded[, .(age = age_single, sex, salt, raisedBP,aroc,year)]
  
  dt_baseline <- merge(dt_baseline, dt_expanded, by = c("age", "sex", "year"), all.x = TRUE)
  
  # Calculate target salt reduction based on method
  if (saltmet == "percent") {
    # salteff is percentage reduction (e.g., 0.3 = 30% reduction)
    dt_baseline[, salt_target := salt * salteff]
  } else if (saltmet == "target") {
    # salteff is absolute target reduction in grams
    dt_baseline[, salt_target := pmin(salt, salteff)]
  } else if (saltmet == "app") {
    # salteff is target intake level
    dt_baseline[, salt_target := pmax(0, salt - salteff)]
  }
  
  # Apply minimum salt intake of 2g
  
  dt_baseline[, salt_target := ifelse(salt - salt_target < 2, salt - 2, salt_target)]
  
  # Step 4: Apply linear progressive decline in sodium intake
  # During scale-up period (saltyear1 to saltyear2): linear progression
  dt_baseline[year >= saltyear1 & year <= saltyear2,
              salt_reduction := salt_target * (year - saltyear1 + 1) / (saltyear2 - saltyear1 + 1)]
  
  # After scale-up period: full reduction achieved
  dt_baseline[year > saltyear2,
              salt_reduction := salt_target]
  
  # Before intervention: no reduction
  dt_baseline[year < saltyear1,
              salt_reduction := 0]
  
  dt_baseline[is.na(salt_reduction) | salt_reduction < 0, salt_reduction := 0]
  
  # # Step 4: Apply progressive decline in sodium intake
  
  # Apply Filippini dose-response to get SBP reduction
  # Progressive BP lowering as sodium intake decreases
  dt_baseline[, sbp_reduction := ((2.8 * raisedBP) + ((1 - raisedBP) * 1.0)) * salt_reduction]
  
  # Step 5: Calculate ETIHAD effect sizes based on BP reduction
  # Number of 10 mmHg reductions achieved through sodium intervention
  #dt_baseline[, n_steps_sodium := sbp_reduction / 10]
  
  # Get ETIHAD relative risks per cause
  dt_baseline <- merge(dt_baseline, ETIHAD_RR, by = c("bp_cat","cause"), all.x = TRUE)
  
  etihad_effects <- dt_baseline[,list(N=mean(pop)),by=list(location,year,age,sex,bp_cat, cause)]
  
  diabetes_prop <- expand_to_single_year_ages(DT.in)
  diabetes_prop <- diabetes_prop[,c("location","Year","age","sex","bp_cat", "diabetes"),with=F]
  
  setnames(diabetes_prop, "Year", "year")
  
  # merge diabetes proportion
  etihad_effects <- merge(etihad_effects,diabetes_prop,all.x = T)
  
  etihad_effects[, etihad_effect := calculate_etihad_cumulative_rr(bp_cat, cause,diabetes_weight = diabetes)]
  
  etihad_effects[,c("diabetes","N"):=NULL]
  # Merge ETIHAD effects into baseline data
  dt_baseline <- merge(dt_baseline, etihad_effects, 
                       by = c("location","year","age","sex","bp_cat", "cause"), all.x = TRUE)
  
  # Effect size from sodium intervention
  
  dt_baseline[, etihad_effect := (1-rr_per_10mmhg)]
  dt_baseline[, etihad_effect_sodium := etihad_effect * 0.1 * sbp_reduction]
  
  # Step 6: Apply effect sizes to incidence
  # IR_bin_new = IR_bin * (1 - effect_size)
  dt_baseline[, IR_bin_new := IR_bin * (1 - etihad_effect_sodium)]
  
  # Step 7: Calculate population-weighted average incidence
  # Using BASELINE population proportions (prob) - these stay constant
  dt_baseline[, IR_new := sum(IR_bin_new * prob), 
              by = .(age, sex, location, cause, year)]
  
  # Before intervention: no effect
  dt_baseline[year < saltyear1, IR_new := IR]
  
  # Step 8: Calculate effect ratio
  dt_baseline[, eff_ir := IR_new / IR]
  
  # Step 9: Apply case fatality reduction
  # CF reduction factors from ETIHAD (different from IR reductions)
  cf_etihad <- data.table(
    cause = c("ihd", "istroke", "hstroke", "hhd", "aod"),
    cf_reduction_per_control = c(0.24, 0.36, 0.76, 0.20, 0.047)
  )
  
  dt_baseline <- merge(dt_baseline, cf_etihad, by = "cause", all.x = TRUE)
  
  # Apply CF Trend AROC reduction (except for AOD in younger ages)
  #dt_baseline[, CF_new := CF * (1 - cf_reduction_per_control * control_agg)] 
  #dt_baseline[, CF_new := CF * (1 - cf_reduction_per_control * aroc)]
  
  # ?? Test
  # dt_baseline[, CF_new := CF * (1 - cf_reduction_per_control * (1 - eff_ir))]
  
  #dt_baseline[, CF_new := (CF * eff_ir) + ((CF * (1 - cf_reduction_per_control)) * (1 - eff_ir))]
  #dt_baseline[cause == "aod" & age < 60, CF_new := CF]
  
  # No secondary effect on case fatality from sodium reduction
  dt_baseline[, CF_new := CF]
  dt_baseline[, eff_cf := CF_new / CF]
  
  # Step 10: Collapse to final output (remove BP bin dimension)
  dt_final <- unique(dt_baseline[, .(
    age, sex, location, cause, year,
    IR = IR_new, CF = CF_new,
    BG.mx, BG.mx.all, PREVt0, DIS.mx.t0, Nx, ALL.mx,
    eff_ir, eff_cf
  )])
  
  setorder(dt_final, year, sex, location, cause, age)
  
  cat("  - ETIHAD effect sizes applied successfully to sodium intervention\n")
  cat("  - Sodium reduction scales linearly from 0 (", saltyear1, ") to full reduction (", saltyear2, ")\n")
  cat("  - Using", saltmet, "method with salteff =", salteff, "\n")
  
  return(dt_final)
}

#...........................................................
## TFA Policy ----
#...........................................................

dt_tfa_scenarios <- as.data.table(readRDS(file = paste0(wd_data,"tfa_policy_scenarios.rds")))

# subset from base year   
dt_tfa_scenarios <- dt_tfa_scenarios[year>=2017,]

# Convert to percent scale
dt_tfa_scenarios[, tfa_current := tfa_current * 100]
dt_tfa_scenarios[, tfa_target  := tfa_target * 100]

# Function to calculate IHD mortality reduction from trans fat intake reduction

calculate_tfa_impact <- function(dt_tfa_scenarios,
                                 intervention_rates,
                                 Country,
                                 target_tfa = 0,
                                 policy_start_year = 2027) {
  cat("  - Calculating TFA impact\n")
  
  #..................................
  # STEP 1: Subset country-specific intervention table
  #..................................
  
  dt <- intervention_rates[location == Country]
  
  #..................................
  # STEP 2: Merge in country-specific trans-fat exposure levels
  #..................................
  
  dt <- merge(dt, dt_tfa_scenarios[location == Country],
              by = c("location", "year"), all.x = TRUE)
  
  #..................................
  # STEP 3: Compute reduction in TFA exposure (delta)
  #  - Before policy start year then no reduction
  #  - After policy start year  then  difference between current and target
  #..................................
  
  dt[, delta := 0]
  dt[year >= policy_start_year, delta := pmax(tfa_current - target_tfa, 0)]
  
  #..................................
  # STEP 4: Assign age-specific relative risk (RR) per 1% of energy from TFA
  #  These correspond to GBD-based RR gradients across age groups
  #..................................
  
  dt[, rr_per_1percent := fcase(
    age >= 20 & age <= 24, 1.21,
    age >= 25 & age <= 29, 1.20,
    age >= 30 & age <= 34, 1.19,
    age >= 35 & age <= 39, 1.18,
    age >= 40 & age <= 44, 1.17,
    age >= 45 & age <= 49, 1.16,
    age >= 50 & age <= 54, 1.15,
    age >= 55 & age <= 59, 1.14,
    age >= 60 & age <= 64, 1.13,
    age >= 65 & age <= 69, 1.11,
    age >= 70 & age <= 74, 1.10,
    age >= 75 & age <= 79, 1.09,
    age >= 80, 1.07,
    default = 1
  )]
  
  #..................................
  # STEP 5: Convert TFA reduction to mortality effect size
  #
  # The denominator ensures:
  #  - Effect size stays in [0,1]
  #  - Consistent scaling across heterogeneous baseline exposures
  #..................................
  
  dt[, effect_size := (delta * (rr_per_1percent - 1)) /
       ((tfa_current * (rr_per_1percent - 1)) + 1)]
  
  dt[is.na(effect_size) | effect_size < 0, effect_size := 0]
  dt[effect_size > 1, effect_size := 1]
  
  #..................................
  # STEP 6: Apply case fatality (CF) reduction ONLY for Ischemic Heart Disease
  #
  # TFA policy has no measured effect on:
  #   - incidence
  #   - non-IHD mortality
  #
  # So: apply effect ONLY to CF of IHD.
  #..................................
  
  dt[, CF_0 := CF]
  dt[cause == "ihd", CF := CF * (1 - effect_size)]
  
  dt[, eff_cf := eff_cf * (1 - effect_size)]
  
  dt[, c("tfa_current", "tfa_target", "CF_0", "delta", 
         "effect_size", "rr_per_1percent") := NULL]
  
  setorder(dt, year, sex, location, cause, age)
  return(dt)
}


#...........................................................
## Statins ----
#...........................................................

# Compute scale up scenario targeting DM high risk population
dt_statin_scenarios <- readRDS(file = paste0(wd_data,"statin_data.rds"))

# # Add Diabetes Proportion
# dt_diabetes <- data.in[,c("location","age","sex","diabetes"),with=F]
# dt_diabetes <- unique(dt_diabetes)
# setnames(dt_diabetes, c("age"), c("age_group"))

# High Fasting Plasma Glucose Attributable Fraction 
# for IHD and ischaemic stroke, using country- & cause-specific AFs supplied in dt_af_statins.

dt_af_statins <- readRDS(file = paste0(wd_data,"af_statins.rds"))

# Function to calculate statins impact on IHD and stroke

# RR of major CVD events per 1.0 mmol/L reduction in LDL cholesterol at 1 year after randomization:
# Statin vs. control incidence 0.78 (0.77 - 0.81)
# RR coronary heart disease 0·80, 99% CI 0·74–0·87
# Source: Trials 2015
# https://doi.org/10.1016/S0140-6736(10)61350-5
# This is applied directly to incidence of IHD and stroke


# Attributable fraction for statins
# GBD 2021 Risk factor attribution High Fasting Plasma glucose
# https://vizhub.healthdata.org/gbd-compare/

## Statins

calculate_statins_impact <- function(dt_statin_scenarios,
                                     intervention_rates,
                                     Country,
                                     dt_af_statins,
                                     adherence_ir = 1,   # primary prevention adherence (IR)
                                     adherence_cf = 1,   # secondary prevention adherence (CF)
                                     prop_athero_stroke = 0.60,
                                     statin_target_coverage = 0.60,
                                     statin_start_year = 2026,
                                     statin_target_year = 2050,
                                     baseline_statin_coverage = NULL) {
  cat("  - Calculating statins impact\n")

  #..........................................................
  # STEP 1: Define relative risks from major statin trials
  #..........................................................

  rr_ir_ihd      <- 0.74
  rr_ir_istroke  <- 0.80
  rr_cf_ihd      <- 0.80
  rr_cf_istroke  <- 0.96

  #..........................................................
  # STEP 2: Default attributable fractions for IHD & ischaemic stroke
  #..........................................................

  af_ihd     <- 0.1497
  af_istroke <- 0.1161

  # Subset country-specific intervention table
  dt <- intervention_rates[location == Country]

  # Age groups (kept just in case needed downstream)
  gbd_breaks <- c(seq(20, 85, 5), Inf)
  gbd_labels <- c(paste0(seq(20, 80, 5), "-", seq(24, 84, 5)), "85plus")
  dt[, age_group := as.character(
    cut(age, breaks = gbd_breaks, labels = gbd_labels,
        right = FALSE, include.lowest = TRUE)
  )]

  #..........................................................
  # STEP 3: Merge statin baseline scenario
  #  We will OVERRIDE statins_uptake_delta with linear scale-up.
  #..........................................................

  dt <- merge(dt,
              dt_statin_scenarios[location == Country],
              by = c("location", "year"),
              all.x = TRUE)

  #..........................................................
  # STEP 4: Determine baseline statin coverage and increment
  #   baseline_statin_coverage: coverage at statin_start_year
  #   statin_target_coverage  : total coverage by statin_target_year
  #   incr_target             : additional coverage above baseline
  #..........................................................

  if (!is.null(baseline_statin_coverage)) {
    baseline_cov <- baseline_statin_coverage
  } else {
    baseline_cov <- dt[year == statin_start_year & cause == "ihd",
                       mean(statins_current, na.rm = TRUE)]
  }

  if (is.na(baseline_cov)) baseline_cov <- 0
  baseline_cov <- max(min(baseline_cov, 1), 0)

  incr_target <- max(statin_target_coverage - baseline_cov, 0)

  # Build linear scale-up in incremental coverage (relative to baseline)
  # Using the same helper as antihypertensives: coverage_t in [0, incr_target]
  dt[, statins_uptake_delta := calculate_coverage_by_year(
    year,
    start_year      = statin_start_year,
    target_year     = statin_target_year,
    target_coverage = incr_target
  )]

  # Set baseline coverage (current coverage used in denominator)
  dt[, statins_current := baseline_cov]

  # Before start year: ensure no additional coverage
  dt[year < statin_start_year, statins_uptake_delta := 0]

  # Safety: clamp to [0, 1]
  dt[is.na(statins_uptake_delta) | statins_uptake_delta < 0, statins_uptake_delta := 0]
  dt[statins_uptake_delta > 1, statins_uptake_delta := 1]

  #..........................................................
  # STEP 5: Merge attributable fractions
  #..........................................................

  dt <- merge(dt, dt_af_statins, by = c("location", "cause"), all.x = TRUE)

  dt[is.na(af_statins) & cause == "ihd",     af_statins := af_ihd]
  dt[is.na(af_statins) & cause == "istroke", af_statins := af_istroke]

  dt[, IR_0     := IR]
  dt[, CF_0     := CF]
  dt[, eff_ir_0 := eff_ir]
  dt[, eff_cf_0 := eff_cf]

  #..........................................................
  # STEP 6: Compute effect sizes for IR (CF do not use AF)
  #
  #   effect_size = AF × 1-RR × Δcoverage × adherence
  #                  ______________________________
  #                  (1 − 1-RR × baseline_coverage × adherence)
  #
  # where Δcoverage = statins_uptake_delta and baseline_coverage = statins_current
  #..........................................................

  dt[, `:=`(
    effect_size_cf = fcase(
      cause == "ihd",
      (1- rr_cf_ihd) * statins_uptake_delta * adherence_cf /
        (1 - (1-rr_cf_ihd) * (statins_current * adherence_cf)),
      
      cause == "istroke",
      prop_athero_stroke * (1-rr_cf_istroke) *
        statins_uptake_delta * adherence_cf /
        (1 - (1-rr_cf_istroke) * (statins_current * adherence_cf)),
      
      default = NA_real_
    ),
    
    effect_size_ir = fcase(
      cause == "ihd",
      af_statins * (1-rr_ir_ihd) * (statins_uptake_delta * adherence_ir) /
        (1 - (1-rr_ir_ihd) * (statins_current * adherence_ir)),
      
      cause == "istroke",
      af_statins * (1-rr_ir_istroke) * statins_uptake_delta * adherence_ir /
        (1 - (1-rr_ir_istroke) * (statins_current * adherence_ir)),
      
      default = NA_real_
    )
  )]
  
  # If baseline already >= target, incr_target == 0 → effect_size_* == 0 automatically
  # but just in case numerical noise:
  if (incr_target == 0 || baseline_cov >= statin_target_coverage) {
    dt[, `:=`(effect_size_ir = 0, effect_size_cf = 0)]
  }

  #..........................................................
  # STEP 7: Apply statin effects ONLY for adults ≥40
  #   CF_new = CF × (1 − effect_size_cf)
  #   IR_new = IR × (1 − effect_size_ir)
  #..........................................................

  dt[age >= 40 & cause %in% c("ihd", "istroke"),
     `:=`(
       CF = CF * (1 - effect_size_cf),
       IR = IR * (1 - effect_size_ir)
     )]

  #..........................................................
  # STEP 8: Update effect ratios for tracking
  #..........................................................

  dt[!is.na(effect_size_ir), eff_ir := eff_ir_0 * (1 - effect_size_ir)]
  dt[!is.na(effect_size_cf), eff_cf := eff_cf_0 * (1 - effect_size_cf)]

  dt[, c("statins_uptake", "statins_target",
         "statins_uptake_lag", "statins_uptake_delta_lag",
         "CF_0", "IR_0", "eff_ir_0", "eff_cf_0",
         "effect_size_ir", "effect_size_cf",
         "af_statins", "age_group") := NULL]

  # keep statins_current & statins_uptake_delta if you want diagnostics
  # otherwise you can also drop them:
  # dt[, c("statins_current", "statins_uptake_delta") := NULL]

  setorder(dt, year, sex, location, cause, age)

  if (dt[, any(is.na(CF))] || dt[, any(is.na(IR))]) {
    stop("Computation produced NA values in CF or IR.", call. = FALSE)
  }

  cat("    Baseline statin coverage:", round(baseline_cov, 3), "\n")
  cat("    Target coverage:", round(statin_target_coverage, 3),
      "by", statin_target_year, "\n")

  dt[]
}

#...........................................................
# Model. Project.all function ----
#...........................................................
# 

# #Test
# Country <-"China"
# saltmet <-"percent"
# salteff <- 0.3
# saltyear1 <- 2025
# saltyear2 <- 2030
# drugcov <- "p75"
# intervention <- "sodium"
# interventions <- c("statins","tfa")
# 
# baseline_ctrl  <- 0.1585683
# #baseline_ctrl  <- 0
# target_control <- 0.5
# 
# control_start_year  <- 2025
# control_target_year <- 2030
# 
# coverage_0 <- baseline_ctrl
# target_year <- control_target_year
# start_year <- control_start_year
# 
# 
# tfa
# tfa_target_tfa        <- 0         # target % energy from TFA
# tfa_policy_start_year <- 2026      # flexible start year

# baseline_statin_coverage <- 0.04737402
# statin_target_coverage <- 0.60
# statin_start_year      <- 2025
# statin_target_year     <- 2050
# adherence <- 1
# prop_athero_stroke <- 0.6

project.all <- function(Country, 
                        interventions = c("antihypertensive", "sodium", "tfa", "statins"),
                        #explicit hypertension control parameters
                        target_control,
                        control_start_year,
                        control_target_year,
                        # explicit Statins params
                        statin_target_coverage,
                        statin_start_year,
                        statin_target_year,
                        adherence_ir = adherence_ir,
                        adherence_cf = adherence_cf,
                        #explicit sodium reduction parameters
                        saltmet = "percent", 
                        salteff = 0.3, 
                        saltyear1 = 2026, 
                        saltyear2 = 2030,
                        # explicit TFA policy parameters (NEW)
                        tfa_target_tfa        = 0,      # target %E from TFA
                        tfa_policy_start_year = 2027,   # first year policy is active
                        #Implicit hypertension control parameters
                        drugcov = "p75",  ## this not binding but keep temporally
                        baseline_ctrl = NULL,   # use provided or extract from dt_hbp_control
                        # implicit statins parameters
                        baseline_statin_coverage = NULL
) {
  
  cat("\n========================================\n")
  cat("STARTING PROJECTION FOR:", Country, "\n")
  cat("Interventions:", paste(interventions, collapse = ", "), "\n")
  cat("========================================\n\n")
  
  # Validate interventions
  valid_interventions <- c("antihypertensive", "sodium", "tfa", "statins")
  if (!all(interventions %in% valid_interventions)) {
    stop("Invalid intervention(s). Must be one or more of: ", 
         paste(valid_interventions, collapse = ", "))
  }
  
  # Preliminaries
  base_rates <- b_rates[location == Country & year>= 2017]
  
  # Get BP distribution data
  DT <- unique(data.in[location == Country][, Year := 2017][, -c("Lower95", "Upper95")])
  DT.in <- as.data.table(left_join(
    DT[rep(seq(1, nrow(DT)), 34)][, Year := repYear(.I)], 
    inc %>% select(-location), 
    by = c("iso3", "Year")
  ))
  
  #...............................................................
  # Baseline for htn control
  
  if (!is.null(baseline_ctrl)) {
    baseline_ctrl_loc <- baseline_ctrl
  } else {
    baseline_ctrl_loc <- dt_hbp_control[
      year == 2024 & location == Country,
      mean(baseline_ctrl, na.rm = TRUE)
    ]
  }
  baseline_ctrl_loc <- max(min(baseline_ctrl_loc, 1), 0)
  
  # Force all AROC-related variables in DT.in to zero (no checks)
  
  DT.in[, c("aroc",
            "aroc2",
            "p_change",
            "p_change2",
            "a_change",
            "a_change2",
            "ideal",
            "drugaroc") := 0]
  
  #...............................................................
  # Baseline for sodium intervention
  
  DT.in.sodium <- copy(DT.in)
  
  #...............................................................
  # Baseline for statins intervention
  
  # Determine baseline statin coverage
  if (!is.null(baseline_statin_coverage)) {
    
    baseline_statin_cov <- baseline_statin_coverage
    
  } else {
    
    baseline_statin_cov <- dt_statin_scenarios[
      location == Country & year == 2024,
      mean(statins_current, na.rm = TRUE)
    ]
  }
  
  # Clamp to [0,1]
  baseline_statin_cov <- max(min(baseline_statin_cov, 1), 0)
  
  #...............................................................
  # Initialize baseline scenario
  intervention_rates <- copy(base_rates)
  intervention_rates[, `:=`(
    eff_ir = 1,
    eff_cf = 1,
    intervention = "baseline"
  )]
  
  # Store baseline for combining BP-related interventions
  intervention_rates_bau <- copy(intervention_rates)
  
  # Track which interventions have been applied
  applied_interventions <- character()
  intervention_label <- "baseline"
  
  # Store individual intervention effects for multiplicative combination
  intervention_effects <- list()
  
  #..................................
  ## Apply Antihypertensive Intervention ----
  #..................................
  
  if ("antihypertensive" %in% interventions) {
    cat("\n=== Applying Antihypertensive Therapy ===\n")
    
    # Calculate with treatment using new method
    intervention_rates_drug <- calculate_antihypertensive_impact_etihad(
      intervention_rates_bau, 
      Country, 
      DT.in, 
      dt_gbd_rr,
      # NEW PARAMETERIZATION
      target_control      = target_control,
      baseline_ctrl       = baseline_ctrl_loc,
      drugcov             = drugcov,
      start_year          = control_start_year,
      target_year         = control_target_year
    )
    
    # Store effects
    intervention_effects[["antihypertensive"]] <- 
      intervention_rates_drug[, .(age, sex, location, cause, year, 
                                  eff_ir_bp = eff_ir, eff_cf_bp = eff_cf)]
    
    applied_interventions <- c(applied_interventions, "BP")
  }
  
  #..................................
  ## Apply Sodium Intervention ----
  #..................................
  
  if ("sodium" %in% interventions) {
    cat("\n=== Applying Sodium Intervention ===\n")
    
    intervention_rates_sodium <- calculate_sodium_impact_etihad(
      intervention_rates_bau, Country, DT.in.sodium, salteff, saltmet,
      saltyear1, saltyear2, dt_gbd_rr)
    
    # Store effects
    intervention_effects[["sodium"]] <- 
      intervention_rates_sodium[, .(age, sex, location, cause, year, 
                                    eff_ir_salt = eff_ir, eff_cf_salt = eff_cf)]
    
    applied_interventions <- c(applied_interventions, "Salt")
  }
  
  #..................................
  ## Combine BP-related interventions (Antihypertensive + Sodium) ----
  #..................................
  
  if (length(intervention_effects) > 0) {
    cat("\n=== Combining BP-related intervention effects ===\n")
    
    # Start with baseline
    intervention_rates <- copy(intervention_rates_bau)
    
    # Merge all BP-related effects
    for (int_name in names(intervention_effects)) {
      intervention_rates <- merge(
        intervention_rates,
        intervention_effects[[int_name]],
        by = c("age", "sex", "location", "cause", "year"),
        all.x = TRUE
      )
    }
    
    # Calculate combined multiplicative effects
    if ("antihypertensive" %in% interventions && "sodium" %in% interventions) {
      # Both interventions present
      intervention_rates[, `:=`(
        eff_ir = eff_ir_bp * eff_ir_salt,
        eff_cf = eff_cf_bp * eff_cf_salt
      )]
    } else if ("antihypertensive" %in% interventions) {
      # Only antihypertensive
      intervention_rates[, `:=`(
        eff_ir = eff_ir_bp,
        eff_cf = eff_cf_bp
      )]
    } else if ("sodium" %in% interventions) {
      # Only sodium
      intervention_rates[, `:=`(
        eff_ir = eff_ir_salt,
        eff_cf = eff_cf_salt
      )]
    }
    
    # Handle missing values (e.g., small countries)
    intervention_rates[is.na(eff_cf), eff_cf := 1]
    intervention_rates[is.na(eff_ir), eff_ir := 1]
    
    # Apply combined effects to rates
    intervention_rates[, `:=`(
      CF = CF * eff_cf,
      IR = IR * eff_ir
    )]
    
    # Clean up temporary effect columns
    effect_cols <- grep("^eff_(ir|cf)_(bp|salt)$", names(intervention_rates), value = TRUE)
    intervention_rates[, (effect_cols) := NULL]
    
    cat("  Combined effects applied to CF and IR\n")
  }
  
  #..................................
  ## Apply TFA Intervention ----
  #..................................
  
  if ("tfa" %in% interventions) {
    cat("\n=== Applying TFA Intervention ===\n")
    
    intervention_rates <- calculate_tfa_impact(
      dt_tfa_scenarios      = dt_tfa_scenarios,
      intervention_rates    = intervention_rates,
      Country               = Country,
      target_tfa            = tfa_target_tfa,
      policy_start_year     = tfa_policy_start_year
    )
    
    applied_interventions <- c(applied_interventions, "TFA")
  }
  
  #..................................
  ## Apply Statins Intervention ----
  #..................................
  
  if ("statins" %in% interventions) {
    cat("\n=== Applying Statins Intervention ===\n")
    
    intervention_rates <- calculate_statins_impact(
      dt_statin_scenarios,
      intervention_rates,
      Country,
      dt_af_statins,
      adherence_ir = adherence_ir,
      adherence_cf = adherence_cf,
      prop_athero_stroke     = 0.60,
      statin_target_coverage = statin_target_coverage,
      statin_start_year      = statin_start_year,
      statin_target_year     = statin_target_year,
      # you can also pass baseline_statin_coverage = some_value if needed
      baseline_statin_coverage  = baseline_statin_cov
    )
    
    applied_interventions <- c(applied_interventions, "Statins")
  }
  
  # Create intervention label
  if (length(applied_interventions) > 0) {
    intervention_label <- paste(applied_interventions, collapse = " + ")
  }
  intervention_rates[, intervention := intervention_label]
  
  #..................................
  ## Initial States ----
  #..................................
  
  cat("\n=== Setting Initial Population States ===\n")
  
  intervention_rates[year == 2017 | age == 20, `:=`(
    sick = Nx * PREVt0,
    dead = Nx * DIS.mx.t0,
    well = Nx * (1 - (PREVt0 + BG.mx)),
    pop = Nx,
    all.mx = Nx * DIS.mx.t0 + Nx * BG.mx
  )]
  
  intervention_rates[CF > 0.99, CF := 0.99]
  intervention_rates[IR > 0.99, IR := 0.99]
  
  setorder(intervention_rates, sex, location, cause, age)
  
  #..................................
  ## STATE TRANSITIONS ----
  #..................................
  
  cat("\n=== Running State Transition Model ===\n")
  cat("Projecting from 2017 to 2058...\n")
  
  for(i in 1:41) {
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
    
    b2 <- b2[year == 2017 + i & age2 < 96, 
             .(age2, newcases2, sick2, dead2, well2, pop2, all.mx2, 
               sex, location, cause, intervention)]
    setnames(b2, "age2", "age")
    
    intervention_rates[year == 2017 + i & age > 20, `:=`(
      newcases = b2$newcases2,
      sick = b2$sick2,
      dead = b2$dead2,
      well = b2$well2,
      pop = b2$pop2,
      all.mx = b2$all.mx2
    )]
  }
  
  cat("\n=== Projection Complete ===\n")
  cat("Final intervention label:", intervention_label, "\n\n")
  
  out.df <- intervention_rates[, .(
    age, cause, sex, year, well, sick, newcases,
    dead, pop, all.mx, intervention, location, eff_ir, eff_cf
  )]
  
  return(out.df)
}

# #...........................................................
# # Checking inputs ----
# #...........................................................

# # Check location names (the key to merge))

b_rates[CF>=1, CF:=0.99]
b_rates[IR>=1, IR:=0.99]
b_rates[CF<0, CF:=0]
b_rates[IR<0, IR:=0]

# #...........................................................
# # Example Usage ----
# #...........................................................
# 
# # Run just antihypertensive therapy
# results_bp_only <- project.all(
#   Country = "China",
#   interventions = c("antihypertensive"),
#   drugcov = "p75"
# )
# 
# # Run antihypertensive + statins
# results_bp_statins <- project.all(
#   Country = "China",
#   interventions = c("antihypertensive", "statins"),
#   drugcov = "p75"
# )
# 
# # Run sodium + TFA + statins
# results_sodium_tfa_statins <- project.all(
#   Country = "China",
#   interventions = c("sodium", "tfa", "statins"),
#   saltmet = "percent",
#   salteff = 0.3,
#   saltyear1 = 2025,
#   saltyear2 = 2030
# )
# 
# ##Run all four interventions
# results_all <- project.all(
#   Country = "China",
#   interventions = c("antihypertensive", "sodium", "tfa", "statins"),
#   saltmet = "percent",
#   salteff = 0.3,
#   saltyear1 = 2025,
#   saltyear2 = 2030,
#   drugcov = "p75"
# )
# # 
# # # Run baseline (no interventions) - useful for comparison
# results_baseline <- project.all(
#   Country = "China",
#   interventions = character(0)  # Empty vector = no interventions
# )

#...........................................................
## Batch Runner for Multiple Scenarios ----
#...........................................................

run_multiple_scenarios <- function(Country, 
                                   scenario_list,
                                   target_control,
                                   control_start_year,
                                   control_target_year,
                                   # explicit Statins params
                                   statin_target_coverage,
                                   statin_start_year,
                                   statin_target_year,
                                   adherence_ir = 1,
                                   adherence_cf = 1,
                                   #explicit sodium reduction parameters
                                   saltmet = "percent", 
                                   salteff = 0.3, 
                                   saltyear1 = 2026, 
                                   saltyear2 = 2030,
                                   # explicit TFA policy parameters (NEW)
                                   tfa_target_tfa        = 0,
                                   tfa_policy_start_year = 2027,
                                   #Implicit hypertension control parameters
                                   drugcov = "p75",  ## this not binding but keep temporally
                                   baseline_ctrl       = NULL,   # use provided or extract from dt_hbp_control
                                   baseline_statin_coverage = NULL
                                   ) {
  #' Run multiple intervention scenarios in one call
  #' 
  #' @param Country Character, country name
  #' @param scenario_list List of character vectors, each defining intervention combinations
  #' @param ... Other parameters passed to project.all()
  #' 
  #' @return List of data.tables, one per scenario
  #' 
  #' @examples
  #' scenarios <- list(
  #'   baseline = character(0),
  #'   bp_only = "antihypertensive",
  #'   bp_salt = c("antihypertensive", "sodium"),
  #'   all = c("antihypertensive", "sodium", "tfa", "statins")
  #' )
  #' results <- run_multiple_scenarios("China", scenarios)
  
  results <- list()
  
  for (scenario_name in names(scenario_list)) {
    cat("\n##########################################\n")
    cat("RUNNING SCENARIO:", scenario_name, "\n")
    cat("##########################################\n")
    
    results[[scenario_name]] <- project.all(
      Country = Country,
      interventions = scenario_list[[scenario_name]],
      #explicit hypertension control parameters
      target_control = target_control,
      control_start_year = control_start_year,
      control_target_year = control_target_year,
      # explicit Statins params
      statin_target_coverage = statin_target_coverage,
      statin_start_year = statin_start_year, 
      statin_target_year = statin_target_year,
      adherence_ir = adherence_ir,
      adherence_cf = adherence_cf,
      #explicit sodium reduction parameters
      saltmet = saltmet,
      salteff = salteff,
      saltyear1 = saltyear1,
      saltyear2 = saltyear2,
      # explicit TFA policy parameters (NEW)
      tfa_target_tfa        = tfa_target_tfa,
      tfa_policy_start_year = tfa_policy_start_year,
      #Implicit hypertension control parameters
      drugcov = drugcov,
      baseline_ctrl       = NULL,   # use provided or extract from dt_hbp_control
      baseline_statin_coverage = NULL
    )
  }
  
  # Combine all results into single data.table with scenario identifier
  combined_results <- rbindlist(results, idcol = "scenario")
  
  return(combined_results)
}

# # Example: Run multiple scenarios
scenarios <- list(
  baseline = character(0),
  bp_only = "antihypertensive",
  sodium_only = "sodium",
  tfa_only = "tfa",
  statins_only = "statins",
  bp_sodium = c("antihypertensive", "sodium"),
  bp_sodium_tfa = c("antihypertensive", "sodium", "tfa"),
  all_four = c("antihypertensive", "sodium", "tfa", "statins")
)

# all_results <- run_multiple_scenarios(
#   Country = "China",
#   scenario_list = scenarios,
#   #explicit hypertension control parameters
#   target_control = 0.5,
#   control_start_year = 2026,
#   control_target_year = 2040,
#   # explicit statins parameters
#   statin_target_coverage = 0.60,
#   statin_start_year      = 2026,
#   statin_target_year     = 2050,
#   adherence_ir = 0.575,
#   adherence_cf = 0.664,
#   #explicit sodium reduction parameters
#   saltmet = "percent",
#   salteff = 0.3,
#   saltyear1 = 2026,
#   saltyear2 = 2030,
#   # explicit TFA parameters (NEW)
#   tfa_target_tfa        = 0,    # 0% of energy from TFA
#   tfa_policy_start_year = 2028, # <-- flexible start year (2026 lagged two years)
#   #Implicit hypertension control parameters
#   drugcov = "p75",
#   baseline_ctrl       = NULL,
#   baseline_statin_coverage = NULL
# )

#...........................................................
## Comparison Helper Functions ----
#...........................................................

compare_scenarios <- function(results_dt, 
                              metric = "dead",
                              years = c(2030, 2040, 2050),
                              reference_scenario = "baseline") {
  #' Compare outcomes across scenarios
  #' 
  #' @param results_dt Data.table with results from run_multiple_scenarios()
  #' @param metric Character, which metric to compare (dead, newcases, sick, etc.)
  #' @param years Numeric vector, which years to compare
  #' @param reference_scenario Character, scenario to use as reference
  #' 
  #' @return Data.table with comparisons
  
  comparison <- results_dt[year %in% years, 
                           .(total = sum(get(metric))),
                           by = .(scenario, year, intervention)]
  
  if (reference_scenario %in% comparison$scenario) {
    ref_values <- comparison[scenario == reference_scenario, 
                             .(year, intervention, ref_total = total)]
    
    comparison <- merge(comparison, ref_values, 
                        by = c("year", "intervention"), 
                        all.x = TRUE)
    
    comparison[, `:=`(
      absolute_difference = total - ref_total,
      percent_change = (total - ref_total) / ref_total * 100,
      averted = ref_total - total
    )]
  }
  
  setorder(comparison, year, scenario)
  return(comparison)
}

# # Example usage:
# deaths_comparison <- compare_scenarios(
#   all_results,
#   metric = "dead",
#   years = c(2030, 2040, 2050),
#   reference_scenario = "baseline"
# )

calculate_cumulative_impact <- function(results_dt,
                                        metric = "dead",
                                        start_year = 2025,
                                        end_year = 2050) {
  #' Calculate cumulative impact over time period
  #' 
  #' @param results_dt Data.table with results from run_multiple_scenarios()
  #' @param metric Character, which metric to sum
  #' @param start_year Numeric, starting year
  #' @param end_year Numeric, ending year
  #' 
  #' @return Data.table with cumulative totals and differences vs baseline
  
  # Compute cumulative totals
  cumulative <- results_dt[year >= start_year & year <= end_year,
                           .(cumulative_total = sum(get(metric))),
                           by = .(scenario, intervention)]
  
  # Get baseline value
  baseline_val <- cumulative[scenario == "baseline", cumulative_total]
  
  # Add difference columns
  cumulative[, diff_vs_baseline := abs(cumulative_total - baseline_val)]
  cumulative[, diff_pct_vs_baseline := abs(100 * (cumulative_total - baseline_val) / baseline_val)]
  
  # Order output
  setorder(cumulative, scenario)
  
  return(cumulative)
}

# # # Example:
# cumulative_deaths <- calculate_cumulative_impact(
#   all_results,
#   metric = "dead",
#   start_year = 2025,
#   end_year = 2050
# )


#...........................................................
## Validation Helper ----
#...........................................................

validate_intervention_results <- function(results_dt) {
  #' Run basic validation checks on results
  #' 
  #' @param results_dt Data.table with model results
  #' 
  #' @return List with validation results and any issues found
  
  issues <- list()
  
  # Check for negative values
  neg_cols <- c("well", "sick", "dead", "pop", "newcases")
  for (col in neg_cols) {
    if (results_dt[, any(get(col) < 0, na.rm = TRUE)]) {
      issues[[paste0("negative_", col)]] <- 
        results_dt[get(col) < 0, .(scenario, year, age, sex, cause, value = get(col))]
    }
  }
  
  # Check for NA values
  na_cols <- c("eff_ir", "eff_cf", "dead", "newcases")
  for (col in na_cols) {
    if (results_dt[, any(is.na(get(col)))]) {
      issues[[paste0("na_", col)]] <- 
        results_dt[is.na(get(col)), .(scenario, year, age, sex, cause)]
    }
  }
  
  # Check population consistency
  pop_check <- results_dt[, .(
    total_population = sum(well + sick, na.rm = TRUE),
    recorded_pop = sum(pop, na.rm = TRUE)
  ), by = .(scenario, year)]
  
  pop_check[, diff := abs(total_population - recorded_pop)]
  if (pop_check[, any(diff > 0.01 * recorded_pop)]) {
    issues[["population_mismatch"]] <- pop_check[diff > 0.01 * recorded_pop]
  }
  
  # Check that effects are bounded
  if (results_dt[, any(eff_ir < 0 | eff_ir > 2, na.rm = TRUE)]) {
    issues[["eff_ir_out_of_bounds"]] <- 
      results_dt[eff_ir < 0 | eff_ir > 2, .(scenario, year, age, cause, eff_ir)]
  }
  
  if (results_dt[, any(eff_cf < 0 | eff_cf > 2, na.rm = TRUE)]) {
    issues[["eff_cf_out_of_bounds"]] <- 
      results_dt[eff_cf < 0 | eff_cf > 2, .(scenario, year, age, cause, eff_cf)]
  }
  
  validation_result <- list(
    passed = length(issues) == 0,
    n_issues = length(issues),
    issues = issues
  )
  
  if (validation_result$passed) {
    cat("\n Ok All validation checks passed!\n")
  } else {
    cat("\n Not Ok Validation found", length(issues), "issue(s):\n")
    print(names(issues))
  }
  
  return(validation_result)
}

# Example:
#validation <- validate_intervention_results(all_results)


#...........................................................
# Parallel execution across countries----
#...........................................................

# 1. Define your intervention parameters BEFORE starting cluster

# explicit hypertension control parameters
target_control <- 0.5
control_start_year <- 2026
control_target_year <- 2040

# explicit sodium reduction parameters
saltmet <- "percent"
salteff <- 0.3
saltyear1 <- 2026
saltyear2 <- 2030
drugcov <- "p75"

## TFA (NEW explicit params)
tfa_target_tfa        <- 0         # target % energy from TFA
tfa_policy_start_year <- 2028      # flexible start year (effect laget two years)

## Statins (NEW explicit params to match calculate_statins_impact)
statin_target_coverage <- 0.60
statin_start_year      <- 2026
statin_target_year     <- 2050
adherence_ir <-  0.575
adherence_cf <- 0.664

#baseline_statin_coverage <- NULL   # let project.all infer from dt_statin_scenarios

# 2. Detect and start cluster
#ncores <- max(1, parallel::detectCores() - 1)
ncores <- 6
cl <- makeCluster(ncores)
registerDoParallel(cl)

# 3. Export necessary objects and functions to workers
clusterExport(
  cl,
  varlist = c(
    ## Main drivers ----
    "project.all",
    "run_multiple_scenarios",
    
    ## BP probability + RR calcs ----
    "get.bp.prob",
    "get_gbd_relative_risks",
    "expand_to_single_year_ages",
    "calculate_baseline_incidence_gbd",
    
    ## ETIHAD helpers ----
    "calculate_etihad_cumulative_rr",
    "calculate_coverage_by_year",
    "add_coverage_by_year",
    "calculate_aggregate_coverage",
    "apply_coverage_adjustment",
    
    ## Intervention modules ----
    "calculate_antihypertensive_impact_etihad",
    "calculate_sodium_impact_etihad",
    "calculate_tfa_impact",
    "calculate_statins_impact",
    
    ## Utility ----
    "repYear",
    
    ## Data objects ----
    "data.in",
    "b_rates",
    "inc",
    "dt_hbp_control",
    "dt_gbd_rr",
    "ETIHAD_RR",
    "ETIHAD_RR_BIN",
    "dt_tfa_scenarios",
    "dt_statin_scenarios",
    "dt_af_statins",
    "scenarios",   # list of scenarios for run_multiple_scenarios
    "wd_outp"
  ),
  envir = globalenv()
)

# 4. Load required packages on each worker
clusterEvalQ(cl, {
  library(data.table)
  library(dplyr)
})

# 5. Define your countries and scenarios
locs <- unique(data.in$location)
locs <- locs[!locs %in% c("Greenland", "Bermuda")]  # Exclusions

#locs <- c("China", "India", "Indonesia", "Russian Federation", "Pakistan", "Bangladesh")

scenarios <- list(
  baseline = character(0),
  bp_only = "antihypertensive",
  sodium_only = "sodium",
  tfa_only = "tfa",
  statins_only = "statins",
  bp_sodium = c("antihypertensive", "sodium"),
  bp_sodium_tfa = c("antihypertensive", "sodium", "tfa"),
  all_four = c("antihypertensive", "sodium", "tfa", "statins")
)

# 6. Parallel execution
time_start <- Sys.time()

results_list <- foreach(country = locs,
                        .packages = c("data.table", "dplyr"),
                        .errorhandling = "pass",
                        .verbose = TRUE) %dopar% {
                          # Redirect output to log file
                          log_file <- file.path(wd_outp, "out_model", 
                                                paste0("log_", country, ".txt"))
                          sink(log_file, split = FALSE)
                          
                          cat("\n==============================\n")
                          cat("Running location:", country, "\n")
                          cat("Time:", as.character(Sys.time()), "\n")
                          cat("==============================\n")
                          
                          # Run multiple scenarios for each country
                          res <- tryCatch({
                            run_multiple_scenarios(
                              Country = country,
                              scenario_list = scenarios,
                              #explicit hypertension control parameters
                              target_control = target_control,
                              control_start_year = control_start_year,
                              control_target_year = control_target_year,
                              ## explicit statin params
                              statin_target_coverage = statin_target_coverage,
                              statin_start_year      = statin_start_year,
                              statin_target_year     = statin_target_year,
                              adherence_ir = adherence_ir,
                              adherence_cf = adherence_cf,
                              #explicit sodium reduction parameters
                              saltmet = saltmet,
                              salteff = salteff,
                              saltyear1 = saltyear1,
                              saltyear2 = saltyear2,
                              # explicit TFA parameters
                              tfa_target_tfa        = tfa_target_tfa,
                              tfa_policy_start_year = tfa_policy_start_year,
                              #Implicit hypertension control parameters
                              drugcov = drugcov,
                              baseline_ctrl       = NULL,   # use provided or extract from dt_hbp_control
                              baseline_statin_coverage = NULL
                            )
                          }, error = function(e) {
                            cat("Not OK Error in", country, ":", e$message, "\n")
                            cat(e$message, "\n")
                            cat("Traceback:\n")
                            print(traceback())
                            return(NULL)
                          }, warning = function(w) {
                            cat("WARNING in", country, ":\n")
                            cat(w$message, "\n")
                          })
                          
                          # Save results per country
                          if (!is.null(res)) {
                            output_file <- file.path(wd_outp, "out_model",
                                                     paste0("model_output_part_", country, ".rds"))
                            saveRDS(res, file = output_file)
                            cat("Successfully saved output to:", output_file, "\n")
                          } else {
                            cat("No results to save for", country, "\n")
                          }
                          
                          sink()  # Close log file
                          
                          res
                        }

time_end <- Sys.time()
cat("OK Total runtime:", difftime(time_end, time_start, units = "mins"), "minutes\n")

# Stop cluster
stopCluster(cl)

# Check which countries succeeded
successful <- sapply(results_list, function(x) !is.null(x))
cat("\nSuccessful runs:", sum(successful), "out of", length(locs), "\n")
cat("Failed countries:", paste(locs[!successful], collapse = ", "), "\n")

# Combine all results (if not too large)
#all_results <- rbindlist(results_list, fill = TRUE)

#...........................................................
# Cleaning up the workspace ----
#...........................................................

rm(list = ls()[sapply(ls(), function(x) is.data.frame(get(x)))])
rm(is,bpcats,locs,i,time1,time2)
