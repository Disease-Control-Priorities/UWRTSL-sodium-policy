
# Old modeling code from original app, adapted to run as a script and with some
# optimizations. Preserved original app logic as much as possible, but some
# helper functions and data manipulations have been restructured for clarity and
# efficiency. Key steps include:
# 1) Formatting large numbers for display (so_formatter)
# 2) Calculating BP-category-specific RR multipliers (add_rr)
# 3) Modeling sodium-induced shifts in BP distribution (get_bp_prob_salt)
# 4) Expanding age groups to single-year ages (expand_bp_probs_to_single_age
# 5) Calculating intervention-specific incidence rates (build_intervention_rates_sodium)
# 6) Running multi-year state-transition
#    model (run_state_transitions_sodium)
# 7) Aggregating outputs by age group
  
    
library(data.table)

###############################################################################
# 1) POLICY DEFINITIONS
###############################################################################

default_sodium_policy_table <- function(
    reform = "Mandatory",
    packaged_salt_pct = 30,
    outside_salt_pct = 30,
    reform_cost = 0.05,
    label_cost = 0.05,
    media_cost = 0.05,
    support_cost = 0.05,
    other_cost = 0.00,
    other_eff_pct = 0
) {
  reform_eff_pct <- if (reform == "Mandatory") 20 else 15
  label_eff_pct  <- 10
  media_eff_pct  <- 10
  support_eff_pct <- 20
  
  data.table(
    policy = c(
      "Reformulation",
      "Package labelling",
      "Media campaigns",
      "Supportive environment",
      "Other"
    ),
    efficacy = c(
      (reform_eff_pct / 100) * (packaged_salt_pct / 100),
      (label_eff_pct  / 100) * (packaged_salt_pct / 100),
      (media_eff_pct  / 100),
      (support_eff_pct / 100) * (outside_salt_pct / 100),
      (other_eff_pct / 100)
    ),
    efficacy_pct = c(
      reform_eff_pct,
      label_eff_pct,
      media_eff_pct,
      support_eff_pct,
      other_eff_pct
    ),
    target_share_pct = c(
      packaged_salt_pct,
      packaged_salt_pct,
      100,
      outside_salt_pct,
      100
    ),
    unit_cost = c(
      reform_cost,
      label_cost,
      media_cost,
      support_cost,
      other_cost
    )
  )
}

###############################################################################
# 2) APPLY POLICY SELECTION
###############################################################################

summarize_sodium_policy_package <- function(
    selected_policies = NULL,
    policy_dt
) {
  policy_dt <- copy(as.data.table(policy_dt))
  
  if (is.null(selected_policies) || length(selected_policies) == 0) {
    selected <- policy_dt[0]
    total_efficacy <- 0
    total_unit_cost <- 0
  } else {
    selected <- policy_dt[policy %in% selected_policies]
    total_efficacy <- selected[, sum(efficacy)]
    total_unit_cost <- selected[, sum(unit_cost)]
  }
  
  list(
    selected = selected,
    total_efficacy = total_efficacy,   # fraction of sodium reduced
    total_unit_cost = total_unit_cost  # per-capita annual cost
  )
}

###############################################################################
# 3) BASELINE / TARGET SODIUM TABLE
###############################################################################

build_sodium_intake_table <- function(
    baseline_sodium_g,
    total_efficacy
) {
  reduced <- baseline_sodium_g * total_efficacy
  target  <- baseline_sodium_g - reduced
  
  data.table(
    Baseline = baseline_sodium_g,
    Reduced = reduced,
    Target = target
  )
}

###############################################################################
# 4) YEAR-SPECIFIC SODIUM POLICY COSTS
#    Matches original app logic:
#    - before start year: 0
#    - linearly scale up between start and end
#    - full cost afterward
###############################################################################

calc_sodium_policy_costs <- function(
    pop_dt,                   # must include year, Pop
    per_capita_cost,
    start_year,
    end_year,
    exchange = 1
) {
  pop_dt <- copy(as.data.table(pop_dt))
  
  pop_dt[, saltcosts := 0]
  
  pop_dt[
    year < start_year,
    saltcosts := 0
  ]
  
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
# 5) SODIUM -> BP DISTRIBUTION MODEL
###############################################################################

get_bp_prob_salt <- function(
    DT,
    salteff_g,
    saltyear1,
    saltyear2
) {
  DT <- copy(as.data.table(DT))
  
  if (salteff_g != 0) {
    DT[
      Year >= saltyear1 & Year <= saltyear2,
      Mean := Mean - (
        ((1.23 * raisedBP) + ((1 - raisedBP) * 0.55)) *
          salteff_g *
          (Year - saltyear1 + 1) / (saltyear2 - saltyear1 + 1)
      )
    ]
    
    DT[
      Year > saltyear2,
      Mean := Mean - (
        ((1.23 * raisedBP) + ((1 - raisedBP) * 0.55)) * salteff_g
      )
    ]
  }
  
  DT[, Scale := stdev^2 / Mean]
  DT[, Shape := (Mean / stdev)^2]
  
  DT[bp_cat == "<120",    prob := pgamma(120, shape = Shape, scale = Scale, lower.tail = TRUE)]
  DT[bp_cat == "120-129", prob := pgamma(130, shape = Shape, scale = Scale, lower.tail = TRUE) -
       pgamma(120, shape = Shape, scale = Scale, lower.tail = TRUE)]
  DT[bp_cat == "130-139", prob := pgamma(140, shape = Shape, scale = Scale, lower.tail = TRUE) -
       pgamma(130, shape = Shape, scale = Scale, lower.tail = TRUE)]
  DT[bp_cat == "140-149", prob := pgamma(150, shape = Shape, scale = Scale, lower.tail = TRUE) -
       pgamma(140, shape = Shape, scale = Scale, lower.tail = TRUE)]
  DT[bp_cat == "150-159", prob := pgamma(160, shape = Shape, scale = Scale, lower.tail = TRUE) -
       pgamma(150, shape = Shape, scale = Scale, lower.tail = TRUE)]
  DT[bp_cat == "160-169", prob := pgamma(170, shape = Shape, scale = Scale, lower.tail = TRUE) -
       pgamma(160, shape = Shape, scale = Scale, lower.tail = TRUE)]
  DT[bp_cat == "170-179", prob := pgamma(180, shape = Shape, scale = Scale, lower.tail = TRUE) -
       pgamma(170, shape = Shape, scale = Scale, lower.tail = TRUE)]
  DT[bp_cat == "180+",    prob := 1 - pgamma(180, shape = Shape, scale = Scale, lower.tail = TRUE)]
  
  DT[, .(age, sex, Year, bp_cat, prob, location)]
}

###############################################################################
# 6) RR MAPPER
###############################################################################

add_rr <- function(RR, bp) {
  fcase(
    bp == "<120",    1,
    bp == "120-129", 1 / RR,
    bp == "130-139", 1 / RR^2,
    bp == "140-149", 1 / RR^3,
    bp == "150-159", 1 / RR^4,
    bp == "160-169", 1 / RR^5,
    bp == "170-179", 1 / RR^6,
    default = 1 / RR^7
  )
}

###############################################################################
# 7) EXPAND AGE GROUPS TO SINGLE-YEAR AGES
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
# 8) BUILD SODIUM-ONLY INTERVENTION RATES
###############################################################################

build_intervention_rates_salt <- function(
    bp_prob_int,
    bp_prob_base,
    base_rates
) {
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
  
  bp_probs[, RRi_IHD := add_rr(0.83, bp_cat)]
  bp_probs[, RRi_HHD := add_rr(0.72, bp_cat)]
  bp_probs[, RRi_stroke := add_rr(0.73, bp_cat)]
  
  alphas <- bp_probs[
    ,
    .(
      ihd = sum(prob_0 * RRi_IHD),
      istroke = sum(prob_0 * RRi_stroke),
      hstroke = sum(prob_0 * RRi_stroke),
      hhd = sum(prob_0 * RRi_HHD)
    ),
    by = .(age, sex, location, Year)
  ]
  
  alphas <- melt(
    alphas,
    id.vars = c("age", "sex", "location", "Year"),
    variable.name = "cause",
    value.name = "alpha"
  )
  
  rris <- bp_probs[, .(age, sex, Year, location, bp_cat, prob, RRi_IHD, RRi_HHD, RRi_stroke)]
  rris[, hstroke := RRi_stroke]
  setnames(rris, c("RRi_IHD", "RRi_HHD", "RRi_stroke"), c("ihd", "hhd", "istroke"))
  
  rris <- melt(
    rris,
    id.vars = c("age", "sex", "location", "bp_cat", "prob", "Year"),
    variable.name = "cause",
    value.name = "RRi"
  )
  
  bp_long <- merge(
    rris,
    alphas,
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
  
  intervention_rates[, yixpi := (RRi * IR / alpha) * prob]
  
  intervention_rates[
    ,
    IR := sum(yixpi),
    by = .(
      age, sex, location, cause, CF, BG.mx, BG.mx.all,
      PREVt0, DIS.mx.t0, Nx, year, ALL.mx
    )
  ]
  
  unique(
    intervention_rates[, !c("prob", "bp_cat", "yixpi", "RRi", "alpha", "prob_0")]
  )
}

###############################################################################
# 9) RUN STATE-TRANSITION MODEL FOR SODIUM ONLY
###############################################################################

run_salt_state_model <- function(intervention_rates,
                                 start_year = 2017,
                                 end_year = 2040) {
  intervention_rates <- copy(as.data.table(intervention_rates))
  
  intervention_rates[CF > 0.99, CF := 0.99]
  intervention_rates[IR > 0.99, IR := 0.99]
  
  for (i in seq_len(end_year - start_year)) {
    yr <- start_year + i
    
    b2 <- intervention_rates[year <= yr & year >= (yr - 1)]
    b2[, age2 := age + 1]
    
    b2[, sick2 := shift(sick) * (1 - (CF + BG.mx)) + shift(well) * IR,
       by = .(sex, location, cause, age)]
    
    b2[, dead2 := shift(sick) * CF,
       by = .(sex, location, cause, age)]
    
    b2[, pop2 := shift(pop) - shift(all.mx),
       by = .(sex, location, cause, age)]
    b2[pop2 < 0, pop2 := 0]
    
    b2[, all.mx2 := sum(dead2), by = .(sex, location, year, age)]
    b2[, all.mx2 := all.mx2 + (pop2 * BG.mx.all)]
    
    b2[, well2 := pop2 - all.mx2 - sick2]
    b2[well2 < 0, well2 := 0]
    
    b2 <- b2[
      year == yr & age2 < 96,
      .(age = age2, sick2, dead2, well2, pop2, all.mx2, sex, location, cause)
    ]
    
    intervention_rates[year == yr & age > 20, sick := b2$sick2]
    intervention_rates[year == yr & age > 20, dead := b2$dead2]
    intervention_rates[year == yr & age > 20, well := b2$well2]
    intervention_rates[year == yr & age > 20, pop := b2$pop2]
    intervention_rates[year == yr & age > 20, all.mx := b2$all.mx2]
  }
  
  intervention_rates[]
}

###############################################################################
# 10) MASTER WRAPPER: SODIUM PACKAGE + SODIUM EPIDEMIOLOGY + COSTING
###############################################################################

run_sodium_policy_model <- function(
    country,
    selected_policies,
    baseline_sodium_g,
    data_in,
    b_rates,
    baseline,
    ref = NULL,
    saltyear_start,
    saltyear_end,
    reform = "Mandatory",
    packaged_salt_pct = 30,
    outside_salt_pct = 30,
    reform_cost = 0.05,
    label_cost = 0.05,
    media_cost = 0.05,
    support_cost = 0.05,
    other_cost = 0,
    other_eff_pct = 0,
    start_year = 2017,
    end_year = 2040
) {
  
  policy_dt <- default_sodium_policy_table(
    reform = reform,
    packaged_salt_pct = packaged_salt_pct,
    outside_salt_pct = outside_salt_pct,
    reform_cost = reform_cost,
    label_cost = label_cost,
    media_cost = media_cost,
    support_cost = support_cost,
    other_cost = other_cost,
    other_eff_pct = other_eff_pct
  )
  
  package_summary <- summarize_sodium_policy_package(
    selected_policies = selected_policies,
    policy_dt = policy_dt
  )
  
  sodium_table <- build_sodium_intake_table(
    baseline_sodium_g = baseline_sodium_g,
    total_efficacy = package_summary$total_efficacy
  )
  
  # App logic used reduced sodium * 2.54 before applying BP effect
  sodium_reduction_equiv <- sodium_table$Reduced[1] * 2.54
  
  base_rates <- copy(as.data.table(b_rates))[location == country]
  baseline_country <- copy(as.data.table(baseline))[location == country]
  DT0 <- copy(as.data.table(data_in))[location == country]
  
  DT0[, Year := start_year]
  drop_cols <- intersect(c("Low95CI", "High95CI"), names(DT0))
  if (length(drop_cols) > 0) DT0[, (drop_cols) := NULL]
  
  n_years <- end_year - start_year + 1
  DT_in <- DT0[rep(seq_len(.N), n_years)]
  DT_in[, Year := rep(start_year:end_year, each = nrow(DT0))]
  
  bp_prob_salt <- get_bp_prob_salt(
    DT = DT_in,
    salteff_g = sodium_reduction_equiv,
    saltyear1 = saltyear_start,
    saltyear2 = saltyear_end
  )
  
  bp_prob_base <- get_bp_prob_salt(
    DT = DT_in,
    salteff_g = 0,
    saltyear1 = saltyear_start,
    saltyear2 = saltyear_end
  )
  
  bp_prob_salt <- expand_bp_probs_to_single_age(bp_prob_salt)
  bp_prob_base <- expand_bp_probs_to_single_age(bp_prob_base)
  
  intervention_rates <- build_intervention_rates_salt(
    bp_prob_int = bp_prob_salt,
    bp_prob_base = bp_prob_base,
    base_rates = base_rates
  )
  
  sim_out <- run_salt_state_model(
    intervention_rates = intervention_rates,
    start_year = start_year,
    end_year = end_year
  )
  
  sim_graphs <- sim_out[, .(
    age, cause, sex, year, well, sick, dead, pop, all.mx, location
  )]
  sim_graphs[, intervention := "Sodium reduction"]
  
  baseline_country <- baseline_country[, .(
    age, sex, year, cause, well, sick, dead, pop, all.mx, location
  )]
  baseline_country[, intervention := "Baseline"]
  
  detailed_outputs <- rbindlist(
    list(baseline_country, sim_graphs),
    use.names = TRUE,
    fill = TRUE
  )
  
  pop_for_cost <- detailed_outputs[
    intervention == "Sodium reduction" &
      age == 20 &
      cause == unique(cause)[1],
    .(year, Pop = pop, location)
  ]
  
  sodium_costs <- calc_sodium_policy_costs(
    pop_dt = pop_for_cost,
    per_capita_cost = package_summary$total_unit_cost,
    start_year = saltyear_start,
    end_year = saltyear_end,
    exchange = 1
  )
  
  list(
    policy_table = policy_dt,
    selected_policies = package_summary$selected,
    total_efficacy = package_summary$total_efficacy,
    total_unit_cost = package_summary$total_unit_cost,
    sodium_intake = sodium_table,
    bp_prob_base = bp_prob_base,
    bp_prob_salt = bp_prob_salt,
    intervention_rates = intervention_rates,
    state_outputs = sim_out,
    detailed_outputs = detailed_outputs,
    sodium_policy_costs = sodium_costs
  )
}