

#...........................................................
## BP Control ----
#...........................................................


get.bp.prob<-function(DT, salteff, saltmet, saltyear1, saltyear2, rx, drugaroc){
  
  if(rx==1 & drugaroc =="baseline"){
    DT[,covinc:=aroc]
    DT[,covtrt:=aroc]
    #DT[,target_year:=ifelse(reach_base>2022, reach_base, 2022)]
  }
  
  if(rx==1 & drugaroc=="p75"){
    DT[,covinc:=htn_ctrl]
    DT[,covtrt:=htn_trt-htn_ctrl]
    #DT[,target_year:=ifelse(refwsalt>2022, refwsalt, 2022)]
  }
  
  if(rx==1 & drugaroc=="p975"){
    DT[,covinc:=htn_ctrl]
    DT[,covtrt:=htn_trt-htn_ctrl]
    #DT[,target_year:=ifelse(aspwsalt>2022, aspwsalt, 2022)]
  }
  
  if(rx==1 & drugaroc=="ideal"){
    DT[,covinc:=htn_ctrl]
    DT[,covtrt:=htn_trt-htn_ctrl]
    #DT[,target_year:=2030]
  }
  
  else{}
  
  
  #make salt variable represent salt gap
  if(saltmet=="percent"){
    DT[,salt_target:=salt*(1-salteff)]
    DT[salt_target<5.04, salt_target:=5.04]
    DT[salt<5.04, salt:=0]
    DT[salt>0,salt:=salt-salt_target]
    DT[salt<0, salt:=0]
  }
  
  if(saltmet=="target"){
    DT[,salt:=salt-salteff]
    DT[salt<0, salt:=0]
  }
  
  if(saltmet=="app"){
    DT[,salt:=salteff]
  }
  
  else{}
  
  if(salteff!=0){
    DT[Year>=saltyear1 & Year<=saltyear2, Mean:=Mean-(((1.12*raisedBP)+((1-raisedBP)*0.58))*salt*(Year-saltyear1+1)/(saltyear2-saltyear1+1))]
    DT[Year>saltyear2, Mean:=Mean-(((1.12*raisedBP)+((1-raisedBP)*0.58))*salt)]
  }
  
  else{}
  
  DT[bp_cat=="<120", prob:=pnorm(120,Mean,stdev)]
  DT[bp_cat=="120-129", prob:=pnorm(130,Mean,stdev)-pnorm(120,Mean,stdev)]
  DT[bp_cat=="130-139", prob:=pnorm(140,Mean,stdev)-pnorm(130,Mean,stdev)]
  DT[bp_cat=="140-149", prob:=pnorm(150,Mean,stdev)-pnorm(140,Mean,stdev)]
  DT[bp_cat=="150-159", prob:=pnorm(160,Mean,stdev)-pnorm(150,Mean,stdev)]
  DT[bp_cat=="160-169", prob:=pnorm(170,Mean,stdev)-pnorm(160,Mean,stdev)]
  DT[bp_cat=="170-179", prob:=pnorm(180,Mean,stdev)-pnorm(170,Mean,stdev)]
  DT[bp_cat=="180+", prob:=1-pnorm(180,Mean,stdev)]
  
  if(rx==1){
    
    #control
    DT[,shift:=prob*(covinc)] 
    DT[bp_cat=="<120" | bp_cat=="120-129" | bp_cat=="130-139", shift:=0]
    DT[, add130:=sum(shift*diabetes), by=.(age, sex, Year)]
    DT[, add140:=sum(shift*(1-diabetes)), by=.(age, sex, Year)]
    DT[,prob:=prob-shift]
    DT[bp_cat=="120-129", prob:=prob+add130]
    DT[bp_cat=="130-139", prob:=prob+add140]
    
    #treatment
    DT[,shift2:=ifelse(bp_cat=="<120" | bp_cat=="120-129", 0, prob*covtrt)]
    DT[,prob2:=prob+shift(shift2, type=c("lead")), by=.(age, sex, Year)]
    DT[bp_cat=="180+", prob2:=prob]
    DT[,prob2:=prob2-shift2]
    #DT[,check2:=sum(prob2), by=.(age, sex, Year)]
    DT[,prob:=prob2]
    
  }
  
  else{}
  
  DT[,c("age", "sex", "Year", "bp_cat" ,"prob", "location")]
  
}


#...........................................................
## TFA Policy ----
#...........................................................

# Parameters
RR_per_2_percent <- 1.28  # RR for 2% TFA increase
RR_per_1_percent <- RR_per_2_percent ^ 0.5  # RR for 1% TFA increase
target_tfa <- 0.5  # Target TFA intake (%E)
default_tfa <- 1.5  # Default TFA intake for "Unknown" values
default_mortality <- 5.0  # Default IHD mortality rate per 100,000

# Function to calculate mortality reduction
calc_mortality_reduction <- function(tfa_current, mortality_rate) {
  # Handle "Unknown" values
  if (tfa_current == "Unknown") {
    tfa_current <- default_tfa
  } else {
    # Handle ranges (e.g., "1.0-2.0") by taking the midpoint
    if (grepl("-", tfa_current)) {
      range_vals <- as.numeric(unlist(strsplit(tfa_current, "-")))
      tfa_current <- mean(range_vals)
    } else {
      # Handle cases like "0.5 (estimated)" or direct numbers
      tfa_current <- as.numeric(gsub("[^0-9.]", "", tfa_current))
    }
  }
  
  # Calculate change in TFA intake
  delta_tfa <- tfa_current - target_tfa
  if (delta_tfa <= 0) {
    return(0.0)  # No reduction if already below target
  }
  
  # Calculate adjusted relative risk and mortality reduction
  rr_adjusted <- RR_per_1_percent ^ delta_tfa
  rr_reduction <- 1 / rr_adjusted
  adjusted_mortality <- mortality_rate * rr_reduction
  reduction <- mortality_rate - adjusted_mortality
  return(reduction)
}

# Age Categories GBD-----

# Vectorized age‐grouping function
create_age_groups <- function(age) {
  # define breaks and labels
  breaks <- c(20, seq(25, 85, by = 5), Inf)
  labels <- c(
    paste0(seq(20, 80, by = 5), "-", seq(24, 84, by = 5)),
    "85plus"
  )
  
  # cut into factor
  cut(
    x              = age,
    breaks         = breaks,
    labels         = labels,
    right          = FALSE,
    include.lowest = TRUE
  )
}

#...........................................................
## Canonical name map + priority countries (shared) ----
#...........................................................
# SINGLE SOURCE OF TRUTH for the raw-source -> model-location name mapping.
# Historically this lived only in 05_build_baseline.R; it is defined here (01,
# sourced first) so the input builders below (Task 1a: htn_eligibility,
# baseline_potassium) can reuse the SAME mapping, and 05 reuses it too. Keys are
# the raw spellings; values are the model's canonical location names.
name_map <- c(
  "Brunei"                            = "Brunei Darussalam",
  "Cape Verde"                        = "Cabo Verde",
  "Cote d'Ivoire"                     = "Ivory Coast",
  "Czech Republic"                    = "Czechia",
  "Federated States of Micronesia"    = "Micronesia (Federated States of)",
  "Iran"                              = "Iran (Islamic Republic of)",
  "Laos"                              = "Lao People's Democratic Republic",
  "Macedonia"                         = "North Macedonia",
  "Moldova"                           = "Republic of Moldova",
  "South Korea"                       = "Republic of Korea",
  "Swaziland"                         = "Eswatini",
  "Syria"                             = "Syrian Arab Republic",
  "The Bahamas"                       = "Bahamas",
  "The Gambia"                        = "Gambia",
  "Venezuela"                         = "Venezuela (Bolivarian Republic of)",
  "Vietnam"                           = "Viet Nam",
  "North Korea"                       = "Democratic People's Republic of Korea"
)

# The 10 RTSL priority countries the model is run for (canonical model names).
# Mirrors the `locs` vector hard-coded in 07_run_interventions.R SECTION 12; kept
# here so the Task-1a input builders can hard-check coverage against the exact
# modelled set.
PRIORITY_COUNTRIES <- c(
  "Viet Nam", "Philippines", "Bangladesh", "China", "Ethiopia",
  "India", "Malaysia", "Thailand", "Cameroon", "Nigeria"
)

#...........................................................
## Task 1a: NCD-RisC hypertension eligibility ----
#...........................................................
#' Build the per-country-and-sex hypertension eligibility table used to size the
#' LSS diagnosed/treated scenarios (Task 1d), replacing the old BP>=140 proxy.
#'
#' Source: NCD-RisC Lancet 2021 age-standardised country file. IMPORTANT: these
#' are AGE-STANDARDISED 30-79 estimates, NOT age-specific; they are used as a
#' single per-country-and-sex population share and carried forward to later model
#' years (there is no post-2019 dx/tx series). Sexes Men/Women are mapped to the
#' model's Male/Female.
#'
#' Derived POPULATION shares (fed to LSS eligibility as coverage):
#'   diagnosed_pop = htn_prev * diagnosed_cond    (P(HTN) x P(dx | HTN))
#'   treated_pop   = htn_prev * treated_cond      (P(HTN) x P(tx | HTN))
#'
#' UI handling (documented choice): the conditional and prevalence 95% UIs are
#' carried through; the *_pop UIs are a simple product of the corresponding
#' bounds (prev_lcl*cond_lcl, prev_ucl*cond_ucl). That is a transparent lower/upper
#' envelope, NOT a formally propagated CI (independence/quantile assumptions do
#' not hold); the report labels it as such.
#'
#' @param wd_raw,wd_data  Path vars (no absolute paths baked in).
#' @param name_map        Shared raw->model name map (defaults to the one above).
#' @param source_year     Year used and carried forward (HTN_SOURCE_YEAR = 2019).
#' @param required_locations If non-NULL, stop() unless every location x
#'        {Male,Female} has non-NA diagnosed_pop AND treated_pop (no silent
#'        fallback to old constants).
#' @param write           Save htn_eligibility.rds to wd_data.
#' @return data.table (location, country_raw, iso3, sex, source_year, htn_prev
#'         (+lcl/ucl), diagnosed_cond (+lcl/ucl), treated_cond (+lcl/ucl),
#'         control_cond, diagnosed_pop (+lcl/ucl), treated_pop (+lcl/ucl)).
build_htn_eligibility <- function(wd_raw, wd_data,
                                  name_map           = get0("name_map"),
                                  source_year        = 2019,
                                  required_locations = NULL,
                                  write              = TRUE) {
  stopifnot(!is.null(name_map))
  f <- file.path(wd_raw, "NCD-RisC_Lancet_2021_Hypertension_age_standardised_countries.csv")
  if (!file.exists(f)) stop("build_htn_eligibility(): missing input ", f)
  d <- data.table::fread(f)

  d <- d[Year == source_year]
  d[, sex := data.table::fifelse(Sex == "Men", "Male",
             data.table::fifelse(Sex == "Women", "Female", NA_character_))]
  d <- d[!is.na(sex)]

  elig <- d[, .(
    country_raw        = `Country/Region/World`,
    iso3               = ISO,
    sex                = sex,
    source_year        = Year,
    htn_prev           = `Prevalence of hypertension`,
    htn_prev_lcl       = `Prevalence of hypertension lower 95% uncertainty interval`,
    htn_prev_ucl       = `Prevalence of hypertension upper 95% uncertainty interval`,
    diagnosed_cond     = `Proportion of diagnosed hypertension among all hypertension`,
    diagnosed_cond_lcl = `Proportion of diagnosed hypertension among all hypertension lower 95% uncertainty interval`,
    diagnosed_cond_ucl = `Proportion of diagnosed hypertension among all hypertension upper 95% uncertainty interval`,
    treated_cond       = `Proportion of treated hypertension among all hypertension`,
    treated_cond_lcl   = `Proportion of treated hypertension among all hypertension lower 95% uncertainty interval`,
    treated_cond_ucl   = `Proportion of treated hypertension among all hypertension upper 95% uncertainty interval`,
    control_cond       = `Proportion of controlled hypertension among all hypertension`
  )]

  # Map raw -> model location names (NCD-RisC already uses "Viet Nam"; fcoalesce
  # leaves any name not in the map unchanged).
  elig[, location := data.table::fcoalesce(name_map[country_raw], country_raw)]

  # Derived population shares + transparent UI envelope (see docstring).
  elig[, diagnosed_pop     := htn_prev     * diagnosed_cond]
  elig[, treated_pop       := htn_prev     * treated_cond]
  elig[, diagnosed_pop_lcl := htn_prev_lcl * diagnosed_cond_lcl]
  elig[, diagnosed_pop_ucl := htn_prev_ucl * diagnosed_cond_ucl]
  elig[, treated_pop_lcl   := htn_prev_lcl * treated_cond_lcl]
  elig[, treated_pop_ucl   := htn_prev_ucl * treated_cond_ucl]

  data.table::setcolorder(elig, c("location", "country_raw", "iso3", "sex", "source_year"))

  # Hard check: no silent fallback to the old 0.33/0.25 constants (Task 1a/1d).
  if (!is.null(required_locations)) {
    need <- data.table::CJ(location = required_locations, sex = c("Male", "Female"))
    have <- elig[!is.na(diagnosed_pop) & !is.na(treated_pop), .(location, sex)]
    miss <- need[!have, on = c("location", "sex")]
    if (nrow(miss) > 0L) {
      stop("build_htn_eligibility(): missing non-NA diagnosed_pop/treated_pop for modelled ",
           "country x sex:\n",
           paste(sprintf("  - %s / %s", miss$location, miss$sex), collapse = "\n"))
    }
  }

  if (isTRUE(write)) {
    if (missing(wd_data) || is.null(wd_data)) stop("wd_data must be supplied when write = TRUE")
    saveRDS(elig, file.path(wd_data, "htn_eligibility.rds"))
  }
  elig[]
}

#...........................................................
## Task 1a: Baseline potassium intake (Reddin 2023) ----
#...........................................................
#' Build baseline potassium intake (g/day) by model location x sex from Reddin
#' et al. 2023 (eTable III), with a sex-specific REGION-MEAN fallback for any
#' modelled country absent from the source. Viet Nam and Ethiopia are absent and
#' inherit their GBD region mean (Viet Nam -> "Southeast Asia"; Ethiopia ->
#' "Eastern Sub Saharan Africa"), per instruction. Feeds the LSS Na/K->SBP
#' potassium channel (Task 1b).
#'
#' @param absent_region_map Named vector model-location -> Reddin region for
#'        countries absent from Reddin (extend here for new modelled countries).
#' @return data.table (location, sex, k_intake_g, k_lcl, k_ucl, region, source
#'         in {"reddin2023","reddin2023_region_fallback"}).
build_baseline_potassium <- function(wd_raw, wd_data,
                                      name_map           = get0("name_map"),
                                      required_locations = NULL,
                                      absent_region_map  = c(
                                        "Viet Nam" = "Southeast Asia",
                                        "Ethiopia" = "Eastern Sub Saharan Africa"),
                                      write              = TRUE) {
  stopifnot(!is.null(name_map))
  f <- file.path(wd_raw, "Reddin2023_potassium_intake_etable_III_LSS.csv")
  if (!file.exists(f)) stop("build_baseline_potassium(): missing input ", f)
  d <- data.table::fread(f)
  d <- d[sex %in% c("Male", "Female")]                    # drop "Both" (model is sexed)
  d[, location := data.table::fcoalesce(name_map[country], country)]

  # Sex-specific region means (fallback source).
  region_means <- d[, .(k_intake_g = mean(estimate),
                        k_lcl       = mean(lower_ci),
                        k_ucl       = mean(upper_ci)),
                    by = .(region, sex)]

  # Direct country values.
  direct <- d[, .(location, sex, region,
                  k_intake_g = estimate, k_lcl = lower_ci, k_ucl = upper_ci,
                  source = "reddin2023")]

  targets <- if (is.null(required_locations)) unique(direct$location) else required_locations
  grid <- data.table::CJ(location = targets, sex = c("Male", "Female"))

  out <- merge(grid, direct, by = c("location", "sex"), all.x = TRUE)

  # Region-mean fallback for absent locations.
  miss_idx <- out[is.na(k_intake_g), which = TRUE]
  if (length(miss_idx) > 0L) {
    for (i in miss_idx) {
      loc <- out$location[i]; sx <- out$sex[i]
      reg <- absent_region_map[[loc]]
      if (is.null(reg)) {
        stop("build_baseline_potassium(): '", loc, "' is absent from Reddin and has no ",
             "entry in absent_region_map (add its GBD region to enable the region-mean fallback).")
      }
      rm_row <- region_means[region == reg & sex == sx]
      if (nrow(rm_row) == 0L) {
        stop("build_baseline_potassium(): no region-mean for region '", reg, "' / ", sx)
      }
      data.table::set(out, i, "region",     reg)
      data.table::set(out, i, "k_intake_g", rm_row$k_intake_g)
      data.table::set(out, i, "k_lcl",      rm_row$k_lcl)
      data.table::set(out, i, "k_ucl",      rm_row$k_ucl)
      data.table::set(out, i, "source",     "reddin2023_region_fallback")
    }
  }

  # Diagnostic: which countries used the region fallback (Viet Nam, Ethiopia expected).
  fb <- out[source == "reddin2023_region_fallback", unique(location)]
  cat(sprintf("build_baseline_potassium(): %d location x sex rows; region-mean fallback used for: %s\n",
              nrow(out), if (length(fb)) paste(fb, collapse = ", ") else "(none)"))

  # Hard check: every modelled country x sex must resolve to a non-NA value.
  if (!is.null(required_locations)) {
    miss <- out[is.na(k_intake_g), .(location, sex)]
    if (nrow(miss) > 0L) {
      stop("build_baseline_potassium(): unresolved baseline potassium for:\n",
           paste(sprintf("  - %s / %s", miss$location, miss$sex), collapse = "\n"))
    }
  }

  if (isTRUE(write)) {
    if (missing(wd_data) || is.null(wd_data)) stop("wd_data must be supplied when write = TRUE")
    saveRDS(out, file.path(wd_data, "baseline_potassium.rds"))
  }
  out[]
}