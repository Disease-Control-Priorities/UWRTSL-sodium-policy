###############################################################################
# 1) HELPER: format large numbers
###############################################################################
so_formatter <- function(x) {
  fifelse(
    x < 1e3, as.character(x),
    fifelse(
      x < 1e6, paste0(signif(x / 1e3, 3), "K"),
      fifelse(
        x < 1e9, paste0(signif(x / 1e6, 3), "M"),
        fifelse(
          x < 1e12, paste0(signif(x / 1e9, 3), "B"),
          paste0(signif(x / 1e12, 3), "T")
        )
      )
    )
  )
}

###############################################################################
# 2) HELPER: BP-category-specific RR multiplier
#    RR is interpreted as per 10 mmHg lower SBP ratio from original app logic
###############################################################################
add_rr <- function(rr, bp_cat) {
  fcase(
    bp_cat == "<120",    1,
    bp_cat == "120-129", 1 / rr,
    bp_cat == "130-139", 1 / rr^2,
    bp_cat == "140-149", 1 / rr^3,
    bp_cat == "150-159", 1 / rr^4,
    bp_cat == "160-169", 1 / rr^5,
    bp_cat == "170-179", 1 / rr^6,
    default              = 1 / rr^7
  )
}

###############################################################################
# 3) SODIUM -> BP DISTRIBUTION MODEL
#
# Required columns in DT:
#   location, age, sex, Year, Mean, stdev, raisedBP, bp_cat
#
# Notes:
# - salteff_g = achieved sodium reduction in grams/day
# - original app converts sodium(g) to salt(g) with * 2.54 before calling
# - BP shift formula preserved from original app:
#     ((1.23 * raisedBP) + ((1 - raisedBP) * 0.55)) * salteff
###############################################################################
get_bp_prob_salt <- function(DT,
                             salteff_g,
                             start_year,
                             end_year) {
  DT <- copy(as.data.table(DT))
  
  # Apply linear scale-up in mean SBP reduction
  if (salteff_g != 0) {
    DT[
      Year >= start_year & Year <= end_year,
      Mean := Mean - (
        ((1.23 * raisedBP) + ((1 - raisedBP) * 0.55)) *
          salteff_g *
          (Year - start_year + 1) / (end_year - start_year + 1)
      )
    ]
    
    DT[
      Year > end_year,
      Mean := Mean - (
        ((1.23 * raisedBP) + ((1 - raisedBP) * 0.55)) * salteff_g
      )
    ]
  }
  
  # Gamma distribution parameters
  DT[, Scale := stdev^2 / Mean]
  DT[, Shape := (Mean / stdev)^2]
  
  # BP category probabilities
  DT[bp_cat == "<120",    prob := pgamma(120, shape = Shape, scale = Scale)]
  DT[bp_cat == "120-129", prob := pgamma(130, shape = Shape, scale = Scale) -
       pgamma(120, shape = Shape, scale = Scale)]
  DT[bp_cat == "130-139", prob := pgamma(140, shape = Shape, scale = Scale) -
       pgamma(130, shape = Shape, scale = Scale)]
  DT[bp_cat == "140-149", prob := pgamma(150, shape = Shape, scale = Scale) -
       pgamma(140, shape = Shape, scale = Scale)]
  DT[bp_cat == "150-159", prob := pgamma(160, shape = Shape, scale = Scale) -
       pgamma(150, shape = Shape, scale = Scale)]
  DT[bp_cat == "160-169", prob := pgamma(170, shape = Shape, scale = Scale) -
       pgamma(160, shape = Shape, scale = Scale)]
  DT[bp_cat == "170-179", prob := pgamma(180, shape = Shape, scale = Scale) -
       pgamma(170, shape = Shape, scale = Scale)]
  DT[bp_cat == "180+",    prob := 1 - pgamma(180, shape = Shape, scale = Scale)]
  
  DT[, .(age, sex, Year, bp_cat, prob, location)]
}

###############################################################################
# 4) EXPAND AGE GROUPS TO SINGLE YEARS
#
# Original app assumed age labels like "20-24", "25-29", ..., "89"
# and expanded 5-year groups to single-year ages, then 90+ tail.
###############################################################################
expand_bp_probs_to_single_age <- function(bp_probs) {
  bp_probs <- copy(as.data.table(bp_probs))
  
  bp_probs[, age := as.numeric(substr(age, 1, 2))]
  bp_probs <- bp_probs[rep(seq_len(.N), each = 5)]
  bp_probs[, age2 := rep(1:5, .N / 5)]
  bp_probs[, age := age + age2 - 1]
  bp_probs[, age2 := NULL]
  
  over90 <- bp_probs[age == 89]
  over90 <- over90[rep(seq_len(.N), each = 6)]
  over90[, age2 := rep(1:6, .N / 6)]
  over90[, age := age + age2]
  over90[, age2 := NULL]
  
  rbindlist(list(bp_probs, over90), use.names = TRUE)
}

###############################################################################
# 5) CALCULATE INTERVENTION-SPECIFIC INCIDENCE RATES
#
# Required inputs:
# - bp_prob_int: output from get_bp_prob_salt(), expanded to single-year ages
# - bp_prob_base: baseline version of same
# - base_rates columns expected:
#   age, sex, location, cause, year, IR, CF, BG.mx, BG.mx.all,
#   PREVt0, DIS.mx.t0, Nx, ALL.mx, sick, well, dead, pop, all.mx
###############################################################################
build_intervention_rates_sodium <- function(bp_prob_int,
                                            bp_prob_base,
                                            base_rates) {
  bp_prob_int  <- copy(as.data.table(bp_prob_int))
  bp_prob_base <- copy(as.data.table(bp_prob_base))
  base_rates   <- copy(as.data.table(base_rates))
  
  setnames(bp_prob_base, "prob", "prob_0")
  
  bp_probs <- merge(
    bp_prob_int,
    bp_prob_base[, .(age, sex, bp_cat, Year, location, prob_0)],
    by = c("age", "sex", "bp_cat", "Year", "location"),
    all.x = TRUE
  )
  
  # Relative risks by cause
  bp_probs[, RRi_IHD    := add_rr(0.83, bp_cat)]
  bp_probs[, RRi_HHD    := add_rr(0.72, bp_cat)]
  bp_probs[, RRi_stroke := add_rr(0.73, bp_cat)]
  
  # Alpha denominators under baseline BP distribution
  alphas <- bp_probs[
    ,
    .(
      ihd     = sum(prob_0 * RRi_IHD),
      istroke = sum(prob_0 * RRi_stroke),
      hstroke = sum(prob_0 * RRi_stroke),
      hhd     = sum(prob_0 * RRi_HHD)
    ),
    by = .(age, sex, location, Year)
  ]
  
  alphas_long <- melt(
    alphas,
    id.vars = c("age", "sex", "location", "Year"),
    variable.name = "cause",
    value.name = "alpha"
  )
  
  rris <- bp_probs[
    ,
    .(age, sex, Year, location, bp_cat, prob, RRi_IHD, RRi_HHD, RRi_stroke)
  ]
  rris[, hstroke := RRi_stroke]
  setnames(rris, c("RRi_IHD", "RRi_HHD", "RRi_stroke"), c("ihd", "hhd", "istroke"))
  
  rris_long <- melt(
    rris,
    id.vars = c("age", "sex", "location", "bp_cat", "prob", "Year"),
    variable.name = "cause",
    value.name = "RRi"
  )
  
  bp_long <- merge(
    rris_long,
    alphas_long,
    by = c("age", "sex", "location", "cause", "Year"),
    all.x = TRUE
  )
  setnames(bp_long, "Year", "year")
  
  intervention_rates <- merge(
    bp_long,
    base_rates,
    by = c("age", "sex", "location", "cause", "year"),
    all.x = TRUE
  )
  
  # Re-weight incidence by intervention-altered BP distribution
  intervention_rates[, yixpi := (RRi * IR / alpha) * prob]
  
  intervention_rates[
    ,
    IR := sum(yixpi),
    by = .(
      age, sex, location, cause, year,
      CF, BG.mx, BG.mx.all, PREVt0, DIS.mx.t0, Nx, ALL.mx,
      sick, well, dead, pop, all.mx
    )
  ]
  
  intervention_rates <- unique(
    intervention_rates[
      ,
      !c("prob", "bp_cat", "yixpi", "RRi", "alpha", "prob_0")
    ]
  )
  
  intervention_rates[CF > 0.99, CF := 0.99]
  intervention_rates[IR > 0.99, IR := 0.99]
  
  intervention_rates[]
}

###############################################################################
# 6) RUN MULTIYEAR STATE-TRANSITION MODEL
###############################################################################
run_state_transitions_sodium <- function(intervention_rates,
                                         start_year = 2017,
                                         end_year = 2040) {
  intervention_rates <- copy(as.data.table(intervention_rates))
  
  for (i in seq_len(end_year - start_year)) {
    yr <- start_year + i
    
    b2 <- intervention_rates[year %in% c(yr - 1, yr)]
    b2[, age2 := age + 1]
    
    # Sick next year
    b2[
      ,
      sick2 := shift(sick) * (1 - (CF + BG.mx)) + shift(well) * IR,
      by = .(sex, location, cause, age)
    ]
    
    # Cause-specific deaths next year
    b2[
      ,
      dead2 := shift(sick) * CF,
      by = .(sex, location, cause, age)
    ]
    
    # Population next year
    b2[
      ,
      pop2 := shift(pop) - shift(all.mx),
      by = .(sex, location, cause, age)
    ]
    b2[pop2 < 0, pop2 := 0]
    
    # All-cause mortality
    b2[
      ,
      all.mx2 := sum(dead2),
      by = .(sex, location, year, age)
    ]
    b2[, all.mx2 := all.mx2 + (pop2 * BG.mx.all)]
    
    # Well next year
    b2[, well2 := pop2 - all.mx2 - sick2]
    b2[well2 < 0, well2 := 0]
    
    # Push updated states into current year
    b2 <- b2[
      year == yr & age2 < 96,
      .(age = age2, sick2, dead2, well2, pop2, all.mx2, sex, location, cause)
    ]
    
    intervention_rates[year == yr & age > 20, sick   := b2$sick2]
    intervention_rates[year == yr & age > 20, dead   := b2$dead2]
    intervention_rates[year == yr & age > 20, well   := b2$well2]
    intervention_rates[year == yr & age > 20, pop    := b2$pop2]
    intervention_rates[year == yr & age > 20, all.mx := b2$all.mx2]
  }
  
  intervention_rates[]
}

###############################################################################
# 7) AGGREGATE OUTPUTS
###############################################################################
aggregate_sodium_outputs <- function(graphs, ref) {
  graphs <- copy(as.data.table(graphs))
  ref    <- copy(as.data.table(ref))
  
  graphs[pop == 0, pop := 1]
  
  # Crude grouped outputs
  all_ages <- graphs[
    ,
    .(Prevalence = sum(sick), Death = sum(dead), Pop = sum(pop)),
    by = .(sex, year, cause, location)
  ]
  all_ages[, age.group := "All ages (20-95)"]
  
  age_20_39 <- graphs[
    age < 40,
    .(Prevalence = sum(sick), Death = sum(dead), Pop = sum(pop)),
    by = .(sex, year, cause, location)
  ]
  age_20_39[, age.group := "20-39"]
  
  age_30_69 <- graphs[
    age >= 30 & age < 70,
    .(Prevalence = sum(sick), Death = sum(dead), Pop = sum(pop)),
    by = .(sex, year, cause, location)
  ]
  age_30_69[, age.group := "30-69"]
  
  age_70_95 <- graphs[
    age >= 70,
    .(Prevalence = sum(sick), Death = sum(dead), Pop = sum(pop)),
    by = .(sex, year, cause, location)
  ]
  age_70_95[, age.group := "70-95"]
  
  age_20_69 <- graphs[
    age < 70,
    .(Prevalence = sum(sick), Death = sum(dead), Pop = sum(pop)),
    by = .(sex, year, cause, location)
  ]
  age_20_69[, age.group := "20-69"]
  
  allage <- rbindlist(
    list(all_ages, age_20_39, age_30_69, age_70_95, age_20_69),
    use.names = TRUE
  )
  
  # Both sexes
  allsex <- allage[
    ,
    .(Prevalence = sum(Prevalence), Death = sum(Death), Pop = sum(Pop)),
    by = .(age.group, year, cause, location)
  ]
  allsex[, sex := "Both"]
  
  out <- rbindlist(list(allage, allsex), use.names = TRUE)
  
  # All causes
  allcause <- out[
    ,
    .(Prevalence = sum(Prevalence), Death = sum(Death), Pop = sum(Pop)),
    by = .(age.group, year, sex, location)
  ]
  allcause[, cause := "All causes"]
  
  out <- rbindlist(list(out, allcause), use.names = TRUE)
  
  # Crude rates
  out[, prevrate  := (Prevalence / Pop) * 100000]
  out[, deathrate := (Death / Pop) * 100000]
  
  # Age-standardized rates
  std_input <- graphs[
    ,
    .(sick = sum(sick), dead = sum(dead), pop = sum(pop)),
    by = .(age, year, cause, sex, location)
  ]
  
  std_merged <- merge(
    std_input,
    ref,
    by = c("sex", "age", "cause", "location"),
    all.x = TRUE
  )
  
  std_merged[, crudeprevrate  := sick / pop]
  std_merged[, crudedeathrate := dead / pop]
  std_merged[, newprev := crudeprevrate * Nx2040]
  std_merged[, newdead := crudedeathrate * Nx2040]
  
  male_total <- ref[sex == "Male",   sum(Nx2040, na.rm = TRUE)]
  fem_total  <- ref[sex == "Female", sum(Nx2040, na.rm = TRUE)]
  
  asr <- std_merged[
    ,
    .(prevrate = sum(newprev, na.rm = TRUE),
      deathrate = sum(newdead, na.rm = TRUE)),
    by = .(sex, location, cause, year)
  ]
  
  asr[sex == "Male",   `:=`(prevrate = (prevrate / male_total) * 100000,
                            deathrate = (deathrate / male_total) * 100000)]
  asr[sex == "Female", `:=`(prevrate = (prevrate / fem_total) * 100000,
                            deathrate = (deathrate / fem_total) * 100000)]
  asr[, age.group := "Age-standardized"]
  
  asr_allcause <- asr[
    ,
    .(prevrate = sum(prevrate), deathrate = sum(deathrate)),
    by = .(age.group, sex, year, location)
  ]
  asr_allcause[, cause := "All causes"]
  
  asr <- rbindlist(list(asr, asr_allcause), use.names = TRUE)
  
  asr_allsex <- asr[
    ,
    .(prevrate = sum(prevrate), deathrate = sum(deathrate)),
    by = .(age.group, year, cause, location)
  ]
  asr_allsex[, sex := "Both"]
  
  asr <- rbindlist(list(asr, asr_allsex), use.names = TRUE)
  
  out <- rbindlist(list(out, asr), use.names = TRUE, fill = TRUE)
  
  # Relabel causes
  out[cause == "ihd",     cause := "Ischemic heart disease"]
  out[cause == "hhd",     cause := "Hypertensive heart disease"]
  out[cause == "istroke", cause := "Ischemic stroke"]
  out[cause == "hstroke", cause := "Hemorrhagic stroke"]
  
  out[]
}

###############################################################################
# 8) MAIN WRAPPER: RUN SODIUM-ONLY MODEL
#
# Inputs:
# - country: scalar character
# - sodium_reduction_g: achieved sodium reduction in grams/day
# - saltyear_start, saltyear_end: linear scale-up period
# - data_in: BP distribution input
# - b_rates: baseline rates
# - baseline: baseline epidemiologic outputs
# - ref: reference population for standardization
#
# Expected columns in data_in:
#   location, age, sex, raisedBP, bp_cat, Mean, stdev
#
# Expected columns in b_rates:
#   location, age, sex, cause, year, IR, CF, BG.mx, BG.mx.all,
#   PREVt0, DIS.mx.t0, Nx, ALL.mx, sick, well, dead, pop, all.mx
#
# Expected columns in baseline:
#   location, age, sex, year, cause, well, sick, dead, pop, all.mx
###############################################################################
run_sodium_model <- function(country,
                             sodium_reduction_g,
                             saltyear_start,
                             saltyear_end,
                             data_in,
                             b_rates,
                             baseline,
                             ref,
                             model_start_year = 2017,
                             model_end_year = 2040) {
  data_in  <- copy(as.data.table(data_in))
  b_rates  <- copy(as.data.table(b_rates))
  baseline <- copy(as.data.table(baseline))
  ref      <- copy(as.data.table(ref))
  
  # Country-specific inputs
  base_rates <- b_rates[location == country]
  base_out   <- baseline[location == country]
  DT0        <- data_in[location == country]
  
  # Reproduce app logic: duplicate baseline BP rows over 24 modeled years
  DT0[, Year := model_start_year]
  DT0 <- DT0[, !c("Low95CI", "High95CI"), with = FALSE]
  
  n_years <- model_end_year - model_start_year + 1
  DT_in <- DT0[rep(seq_len(.N), n_years)]
  DT_in[, Year := rep(model_start_year:model_end_year, each = nrow(DT0))]
  
  # Original app converted sodium(g) to salt(g) via * 2.54
  salt_effect_equiv <- sodium_reduction_g * 2.54
  
  # Intervention and baseline BP probabilities
  bp_prob_salt <- get_bp_prob_salt(
    DT         = DT_in,
    salteff_g  = salt_effect_equiv,
    start_year = saltyear_start,
    end_year   = saltyear_end
  )
  
  bp_prob_base <- get_bp_prob_salt(
    DT         = DT_in,
    salteff_g  = 0,
    start_year = saltyear_start,
    end_year   = saltyear_end
  )
  
  # Expand age groups to single-year ages
  bp_prob_salt <- expand_bp_probs_to_single_age(bp_prob_salt)
  bp_prob_base <- expand_bp_probs_to_single_age(bp_prob_base)
  
  # Build intervention-adjusted rates
  intervention_rates <- build_intervention_rates_sodium(
    bp_prob_int  = bp_prob_salt,
    bp_prob_base = bp_prob_base,
    base_rates   = base_rates
  )
  
  # Run state transitions
  sim_out <- run_state_transitions_sodium(
    intervention_rates = intervention_rates,
    start_year = model_start_year,
    end_year   = model_end_year
  )
  
  sim_graphs <- sim_out[
    ,
    .(age, cause, sex, year, well, sick, dead, pop, all.mx, location)
  ]
  
  # Add baseline
  base_graphs <- base_out[
    ,
    .(age, sex, year, cause, well, sick, dead, pop, all.mx, location)
  ]
  
  sim_graphs[, intervention := "Sodium reduction"]
  base_graphs[, intervention := "Baseline"]
  
  graphs <- rbindlist(list(base_graphs, sim_graphs), use.names = TRUE, fill = TRUE)
  
  # Aggregate outputs
  aggregated <- aggregate_sodium_outputs(
    graphs = graphs,
    ref    = ref
  )
  
  list(
    bp_prob_base        = bp_prob_base,
    bp_prob_salt        = bp_prob_salt,
    intervention_rates  = intervention_rates,
    state_outputs       = sim_out,
    detailed_outputs    = graphs,
    aggregated_outputs  = aggregated
  )
}