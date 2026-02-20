
#...........................................................
# Sodium data ----
#...........................................................


## Salt data
# Save the data
dt_sodium <- readRDS(file = paste0(wd_data,"Sodium/", "sodium_data.rds"))

# Computing mean intake per location

dt_pop <- readRDS(file = paste0(wd_raw,"GBD/","totalpop_ihme.rds"))

# rename locations to match gbd locations (the baseline)
name_map <- c(
  "United States of America"          = "United States",
  "Ukraine (without Crimea & Sevastopol)" = "Turkey")

dt_pop[, location := fcoalesce(name_map[location], location)]

dt_pop <- dt_pop[, .(location, year_id, sex_name, age_group_name, val),with=T]

setnames(dt_pop, c("val", "year_id","sex_name"),
         c("population","year","sex"))

age_match<-data.frame(age=20:95)%>%
  mutate(age.group = ifelse(age<25, "20 to 24", NA),
         age.group = ifelse(age>=25 & age<30, "25 to 29", age.group),
         age.group = ifelse(age>=30 & age<35, "30 to 34", age.group),
         age.group = ifelse(age>=35 & age<40, "35 to 39", age.group),
         age.group = ifelse(age>=40 & age<45, "40 to 44", age.group),
         age.group = ifelse(age>=45 & age<50, "45 to 49", age.group),
         age.group = ifelse(age>=50 & age<55, "50 to 54", age.group),
         age.group = ifelse(age>=55 & age<60, "55 to 59", age.group),
         age.group = ifelse(age>=60 & age<65, "60 to 64", age.group),
         age.group = ifelse(age>=65 & age<70, "65 to 69", age.group),
         age.group = ifelse(age>=70 & age<75, "70 to 74", age.group),
         age.group = ifelse(age>=75 & age<80, "75 to 79", age.group),
         age.group = ifelse(age>=80 & age<85, "80 to 84", age.group),
         age.group = ifelse(age>=85 & age<90, "85 to 89", age.group),
         age.group = ifelse(age>=90 & age<95, "90 to 94", age.group),
         age.group = ifelse(age==95, "95 plus", age.group))

age_match$age <- as.character(age_match$age)

dt_pop[age_group_name == "<1 year", age_group_name := "0"]
dt_pop[age_group_name == "95 plus", age_group_name := "95"]

dt_pop <- merge(dt_pop, age_match, by.x = "age_group_name", by.y = "age", all.x = TRUE)

# Average population by age group (not year because POp comes up to 2019)
dt_pop <- dt_pop[,list(population=mean(population)),by=list(location,sex,age.group)]

dt_sodium[,age.group:=age]

# Merge population data with sodium data
dt_sodium <- merge(dt_sodium,dt_pop,
                by = c("location","sex","age.group"), all.x = TRUE,all.y = TRUE)

# check not missing population . There are only 4 territories not included in our analysis
#dt_check <- unique(dt_sodium[is.na(sodium_current),],by="location")
dt_check <- unique(dt_sodium[is.na(population),],by="location")

# compute mean sodium intake per location
dt_sodium_mean <- dt_sodium[, .(sodium_current = weighted.mean(sodium_current,population, na.rm = TRUE),
                                salt = min(salt,na.rm=T)),
                      by = list(year,location)]

dt_sodium_mean <- dt_sodium_mean[year >= 2019,]

# Create scenarios for 2025-2050

dt_sodium_mean_24 <- dt_sodium_mean[year==2024,]

years <- 2025:2050
dt_sodium_scenarios <- dt_sodium_mean_24[, {
  # create a fresh table of years
  yearly_data <- data.table(year = years)
  # assign the constant values of sodium_current & salt for this 'location' group
  yearly_data[, `:=`(
    sodium_current = sodium_current,
    salt           = salt
  )]
  yearly_data
}, by = location]

dt_sodium_scenarios <- rbind(dt_sodium_mean,dt_sodium_scenarios,fill=T)

# Scenarios: reduce 15% by 2030, 30% by 2030
# interpolate the target values for the scenarios linearly

interpolate_sodium_reduction <- function(dt,
                                         current_col       = "sodium_current",
                                         out_col           = "sodium_target_prog",
                                         transition_start  = 2025L,
                                         transition_end    = 2030L,
                                         target_fraction   = 0.85) {
  stopifnot(is.data.table(dt))
  
  # 1) Before the transition, keep at current
  dt[
    year < transition_start,
    (out_col) := get(current_col)
  ]
  
  # 2) Between transition_start and transition_end, ramp DOWN
  dt[
    year >= transition_start & year <= transition_end,
    (out_col) := get(current_col) +
      (get(current_col) * target_fraction - get(current_col)) *
      (year - transition_start) / (transition_end - transition_start)
  ]
  
  # 3) After transition_end, hold at the reduced level
  dt[
    year > transition_end,
    (out_col) := get(current_col) * target_fraction
  ]
  
  invisible(dt)
}

interpolate_sodium_reduction(
  dt = dt_sodium_scenarios,
  current_col      = "sodium_current",
  out_col          = "sodium_target_prog",
  transition_start = 2025L,
  transition_end   = 2030L,
  target_fraction  = 0.85
)

interpolate_sodium_reduction(
  dt = dt_sodium_scenarios,
  current_col      = "sodium_current",
  out_col          = "sodium_target_asp",
  transition_start = 2025L,
  transition_end   = 2030L,
  target_fraction  = 0.7
)

interpolate_sodium_reduction(
  dt = dt_sodium_scenarios,
  current_col      = "salt",
  out_col          = "salt_target_prog",
  transition_start = 2025L,
  transition_end   = 2030L,
  target_fraction  = 0.85
)

interpolate_sodium_reduction(
  dt = dt_sodium_scenarios,
  current_col      = "salt",
  out_col          = "salt_target_asp",
  transition_start = 2025L,
  transition_end   = 2030L,
  target_fraction  = 0.7
)

setnames(dt_sodium_scenarios,"salt","salt_current")

# remove missing salt current in not included territories
dt_sodium_scenarios <- dt_sodium_scenarios[!is.na(salt_current),]

dt_sodium_scenarios[location=="Bolivia (Plurinational State of)", location := "Bolivia"]
dt_sodium_scenarios[location=="United Republic of Tanzania", location := "Tanzania"]

# Save the data
saveRDS(dt_sodium_scenarios,file = paste0(wd_data,"Sodium/", "sodium_policy_scenarios.rds"))

# remove objects that are no longer needed
rm(dt_sodium, dt_pop, dt_sodium_mean, dt_sodium_mean_24,
   years, dt_sodium_scenarios, interpolate_sodium_reduction,
   name_map, dt_check,age_match)

#...........................................................
# TFA Policy----
#...........................................................

# dt_tfa_scenarios <- dt_tfa[, {
#   yearly_data <- data.table(year = years)
#   yearly_data[, tfa_current := tfa_current]
#   yearly_data[, tfa_target := ifelse(year<2025 | tfa_current< 0.5,tfa_current,0.5)] # Default value
#   yearly_data[year >= tfa_bpp_date_in_effect & year < 2025, tfa_target := tfa_current]
#   yearly_data
# }, by = location]

# TFA data
dt_tfa <- readRDS(file = paste0(wd_data,"TFAPolicy/", "tfa_data.rds"))

name_map <- c(
  "United States of America"          = "United States",
  "Taiwan"                            = "Taiwan (Province of China)",
  "Urkraine (without Crimea & Sevastopol)" = "Ukraine")

# P target
p_tfa_target <- 0.00
dt_tfa[, location := fcoalesce(name_map[location], location)]

# Computing mean intake per location

dt_pop <- readRDS(file = paste0(wd_raw,"GBD/","totalpop_ihme.rds"))

dt_pop <- dt_pop[, .(location, year_id, sex_name, age_group_name, val),with=T]

setnames(dt_pop, c("val", "year_id","sex_name"),
                c("population","year","sex"))

name_map <- c(
  "United States of America"          = "United States",
  "Taiwan"                            = "Taiwan (Province of China)",
  "Urkraine (without Crimea & Sevastopol)" = "Ukraine")

dt_pop[, location := fcoalesce(name_map[location], location)]


age_match<-data.frame(age=20:95)%>%
  mutate(age.group = ifelse(age<25, "20 to 24", NA),
         age.group = ifelse(age>=25 & age<30, "25 to 29", age.group),
         age.group = ifelse(age>=30 & age<35, "30 to 34", age.group),
         age.group = ifelse(age>=35 & age<40, "35 to 39", age.group),
         age.group = ifelse(age>=40 & age<45, "40 to 44", age.group),
         age.group = ifelse(age>=45 & age<50, "45 to 49", age.group),
         age.group = ifelse(age>=50 & age<55, "50 to 54", age.group),
         age.group = ifelse(age>=55 & age<60, "55 to 59", age.group),
         age.group = ifelse(age>=60 & age<65, "60 to 64", age.group),
         age.group = ifelse(age>=65 & age<70, "65 to 69", age.group),
         age.group = ifelse(age>=70 & age<75, "70 to 74", age.group),
         age.group = ifelse(age>=75 & age<80, "75 to 79", age.group),
         age.group = ifelse(age>=80 & age<85, "80 to 84", age.group),
         age.group = ifelse(age>=85 & age<90, "85 to 89", age.group),
         age.group = ifelse(age>=90 & age<95, "90 to 94", age.group),
         age.group = ifelse(age==95, "95 plus", age.group))

age_match$age <- as.character(age_match$age)

dt_pop[age_group_name == "<1 year", age_group_name := "0"]
dt_pop[age_group_name == "95 plus", age_group_name := "95"]

dt_pop <- merge(dt_pop, age_match, by.x = "age_group_name", by.y = "age", all.x = TRUE)

# Average population by age group (not year because POp comes up to 2019)
dt_pop <- dt_pop[,list(population=mean(population)),by=list(location,sex,age.group)]

dt_tfa[,age.group:=age]

# Merge population data with TFA data
dt_tfa <- merge(dt_tfa,dt_pop,
                by = c("location","sex","age.group"), all.x = TRUE,all.y = F)

# check not missing population . There are only 1 territories not included in our analysis
#dt_check <- unique(dt_sodium[is.na(sodium_current),],by="location")
dt_check <- unique(dt_tfa[is.na(population),],by="location")


# compute mean TFA intake per location
dt_tfa_mean <- dt_tfa[, .(tfa_current = weighted.mean(tfa_current,population, na.rm = TRUE),
                          tfa_bpp_date_in_effect = min(tfa_bpp_date_in_effect, na.rm = TRUE),
                          tfa_bpp_date_passed = min(tfa_bpp_date_passed, na.rm = TRUE)),
                        by = list(year,location)]

#dt_tfa_mean_24 <- dt_tfa_mean[year==2024,]

#Most recent data from 2024:2019
dt_tfa_mean_24 <- dt_tfa_mean[year %in% c(2024),]
dt_tfa_mean_24 <- dt_tfa_mean_24[order(dt_tfa_mean_24$tfa_current,decreasing = T),]
dt_tfa_mean_24 <- unique(dt_tfa_mean_24,by="location")

years <- 2025:2050
dt_tfa_scenarios <- dt_tfa_mean_24[, {
  yearly_data <- data.table(year = years)
  yearly_data[, tfa_current := tfa_current]
  yearly_data
}, by = location]

dt_tfa_scenarios <- rbind(dt_tfa_mean,dt_tfa_scenarios,fill=T)

# Add the tfa_target to the main data table

dt_tfa_scenarios[,c("tfa_bpp_date_in_effect","tfa_bpp_date_passed"):=NULL]
dt_tfa_scenarios <- merge(dt_tfa_scenarios, 
                                  dt_tfa_mean_24[, .(location, tfa_bpp_date_in_effect,tfa_bpp_date_passed)],
                                  by = "location", all.x = TRUE)

# Fix bpp date for specific locations
dt_tfa_scenarios[ , tfa_target := tfa_current]
dt_tfa_scenarios[ tfa_bpp_date_passed<=2023 & year>2023 & year<2025 , tfa_target := ifelse(tfa_current<p_tfa_target,tfa_current,p_tfa_target)]
#dt_tfa_scenarios[ year>=2025 , tfa_target := 0.005]
#dt_tfa_scenarios[ year>=2027 , tfa_target := ifelse(tfa_current<p_tfa_target,tfa_current,p_tfa_target)]

# ensure that once a country reaches 0, its tfa_target remain 0 thereafter.

# Set initial targets to current values
dt_tfa_scenarios[, tfa_target := tfa_current]

# If TFA policy is passed before or in 2023, drop to target (0) by 2024
dt_tfa_scenarios[
  tfa_bpp_date_passed <= 2023 & year >= 2024, 
  tfa_target := p_tfa_target
]

# For all countries, once TFA reaches 0, it cannot go up again
dt_tfa_scenarios[, tfa_target := 
                   ifelse(
                     year >= min(year[tfa_target == p_tfa_target], na.rm = TRUE),
                     p_tfa_target,
                     tfa_target
                   ),
                 by = location
]

# Optional: also keep tfa_current at 0 after a country hits 0
dt_tfa_scenarios[, tfa_current := 
                   ifelse(
                     year >= min(year[tfa_target == p_tfa_target], na.rm = TRUE),
                     p_tfa_target,
                     tfa_current
                   ),
                 by = location
]

dt_tfa_scenarios[ year>=2027 , tfa_target := ifelse(tfa_current<p_tfa_target,tfa_current,p_tfa_target)]

dt_tfa_scenarios <- dt_tfa_scenarios[, .(location, year, tfa_current,tfa_target)]

# Taiwan, Ukraine and other territories are NA, so assigning the minimum value
dt_tfa_scenarios[is.na(tfa_target),  tfa_target := p_tfa_target]
dt_tfa_scenarios[is.na(tfa_current), tfa_current := p_tfa_target]

# Check country names
dt_tfa_scenarios[, location := gsub("United States of America", "United States", location)]
dt_tfa_scenarios[location=="Bolivia (Plurinational State of)", location := "Bolivia"]

dt_tfa_scenarios[, location := gsub("Taiwan", "Taiwan (Province of China)", location)]

dt_tfa_scenarios[, location := gsub("United Republic of Tanzania", "Tanzania", location)]

# Fix countries names
dt_tfa_scenarios[location == "Türkiye", location := "Turkey"]
dt_tfa_scenarios[location == "Côte d'Ivoire", location := "Ivory Coast"]

#print length(locs_statins)
locs_tfa <- unique(dt_tfa_scenarios$location)
print(paste0("Number of locations with tfa data: ", length(locs_tfa)))

# Save the data
saveRDS(dt_tfa_scenarios,file = paste0(wd_data,"TFAPolicy/", "tfa_policy_scenarios.rds"))

# clean up
rm(dt_tfa, dt_pop, dt_tfa_mean, dt_tfa_mean_24,
   years, dt_tfa_scenarios, name_map, dt_check, age_match)

#...........................................................
# Statins/Lipid data ----
#...........................................................

# Targets

# US 100% of elegible adults, i.e 32% 40-75
#https://www.acc.org/Latest-in-Cardiology/Articles/2022/10/04/13/38/Comparing-Guideline-Recommendations-of-Statin-Use-For-the-Primary-Prevention-of-ASCVD?utm_source=chatgpt.com
# WHO 50% of elegible adults 40+
#https://cdn.who.int/media/docs/default-source/inaugural-who-partners-forum/gmf_indicator_definitions_version_nov2014438a791b-16d3-46f3-a398-88dee12e796b.pdf?sfvrsn=4b337764_1&utm_source=chatgpt.com

# Input from Polypill paper

# Depreccated, for the file that blended multiple pills to the one with just statins prediction
# dt_sta.1 <- fread(paste0(wd_raw,"Statins/","coverage_statins",".csv"))
# dt_sta$location <- dt_sta$location_name
# data.in.statin <- dt_sta[cause=="ihd",c("location","pp_cov"),with=F]

data.in.statin <- fread(paste0(wd_raw,"Statins/","FDC_coverage_data_statints_pp",".csv"))

data.in.statin[location == "Côte d'Ivoire", location := "Ivory Coast"]

# # Diabetes prevalence
# data.in.statin <- fread(paste0(wd,"bp_data6.csv"))%>%rename(location = location_gbd)%>%select(-Year, -Country)
# 
# data.in.statin <- unique(data.in.statin[,c("location","age","sex","diabetes"),with=F],by=c("location","age","sex"))
# 
# data.in.statin <- merge(data.in.statin, 
#                         dt_sta,
#                         by=c("location"), all.x=TRUE)

# ?? Subset for peoble 40+ 55-80 from SPC paper
#data.in.statin <- data.in.statin[age >= 40]

#setnames(data.in.statin,"age", "age_group")

# Age groups + merge on statin coverage info
#data.in.statin[, age_group := create_age_groups(age_group)]

# Current vs target coverage
data.in.statin[, statins_current := ifelse(is.na(pp_cov),0,pp_cov)]
data.in.statin[, statins_target  := 0.60]  # assuming progress scenario
#data.in.statin[diabetes > statins_target, statins_target  := diabetes]  # assuming diabetes defines the target
data.in.statin[statins_current > statins_target, statins_target  := statins_current]  # assuming diabetes defines the target

data.in.statin[,c("pp_cov","diabetes"):=NULL]

# Scaling up scenarios
# ? Scenario by age

# ── function to expand & interpolate ─────────────────────────────────────
interpolate_statins <- function(dt_raw,
                                start_year = 2025L,
                                end_year   = 2050L,
                                current_col = "statins_current",
                                target_col  = "statins_target",
                                out_col     = "statins_uptake") {

  stopifnot(is.data.table(dt_raw))
  yrs <- seq.int(start_year, end_year)               # 2025:2030
  span <- end_year - start_year                      # = 5

  dt_out <- dt_raw[, {
    delta <- get(target_col) - get(current_col)
    .(year = yrs,
      (get(current_col) + delta * (yrs - start_year) / span))
  }, by = .(location)]

  setnames(dt_out, old = "V2", new = out_col)
  setcolorder(dt_out, c("location", "year", out_col))

  dt_out[]
}

## ── function to expand & interpolate with a logistic curve ───────────────
interpolate_statins_logistic <- function(dt_raw,
                                         start_year   = 2025L,
                                         end_year     = 2050L,
                                         current_col  = "statins_current",
                                         target_col   = "statins_target",
                                         out_col      = "statins_uptake",
                                         k            = 6) {  # k controls steepness (approx 6 gives a classic S-shape)
  
  stopifnot(is.data.table(dt_raw))
  
  yrs  <- seq.int(start_year, end_year)
  span <- end_year - start_year
  
  ## pre-compute the normalising constants so that g(0)=0 and g(1)=1
  c1 <- 1 / (1 + exp(  k * 0.5))       # value of logistic at t = 0
  c2 <- 1 / (1 + exp(- k * 0.5))       # value of logistic at t = 1
  
  dt_out <- dt_raw[, {
    delta   <- get(target_col) - get(current_col)
    t_norm  <- (yrs - start_year) / span                     # 0 … 1
    g       <- (1 / (1 + exp(-k * (t_norm - 0.5))) - c1) /   # rescaled logistic
      (c2 - c1)
    .(year  = yrs,
      uptake = get(current_col) + delta * g)
  }, by = .(location)]
  
  setnames(dt_out, "uptake", out_col)
  setcolorder(dt_out, c("location", "year", out_col))
  dt_out[]
}


## ── scale up 2025-2030 ────────────────────────────────────────────────────────────────
#dt_statins_interp <- interpolate_statins_logistic(data.in.statin)
dt_statins_interp <- interpolate_statins(data.in.statin)

dt_statins_current <- c()

for(ii in 2019:2024){
  dt_temp <- dt_statins_interp[year==2025,]
  dt_temp[, year := ii]
  dt_statins_current <- rbind(dt_statins_current,
                             dt_temp)
}

# dt_statins_target <- c()
# 
# for(ii in 2031:2050){
#   dt_temp <- dt_statins_interp[year==2030,]
#   dt_temp[, year := ii]
#   dt_statins_target <- rbind(dt_statins_target, 
#                              dt_temp)
# }
# 
# dt_statins_scenarios <- rbind(dt_statins_current, dt_statins_interp,
#                                   dt_statins_target)

dt_statins_scenarios <- rbind(dt_statins_current, dt_statins_interp)

dt_statins_scenarios <- merge(dt_statins_scenarios,data.in.statin,all.x = T)

dt_statins_scenarios <- dt_statins_scenarios[order(dt_statins_scenarios$location,dt_statins_scenarios$year), ]
dt_statins_scenarios[, statins_uptake_lag := shift(statins_uptake, n = 1, type = "lag"), by = location]
dt_statins_scenarios[, statins_uptake_delta_lag := statins_uptake-statins_uptake_lag]

dt_statins_scenarios[, statins_uptake_delta := statins_uptake-statins_current]

dt_statins_scenarios[is.na(statins_uptake_lag), statins_uptake_lag := 0]
dt_statins_scenarios[is.na(statins_uptake_delta), statins_uptake_delta_lag := 0]
dt_statins_scenarios[is.na(statins_uptake_delta), statins_uptake_delta := 0]

# Check country names
dt_statins_scenarios[, location := gsub("United States of America", "United States", location)]


dt_statins_scenarios[location=="Bolivia (Plurinational State of)", location := "Bolivia"]
dt_statins_scenarios[location=="United Republic of Tanzania", location := "Tanzania"]

dt_statins_scenarios_samoa <- dt_statins_scenarios[location=="Samoa",]
dt_statins_scenarios_samoa[,location:="American Samoa"]

dt_statins_scenarios <- rbind(dt_statins_scenarios,dt_statins_scenarios_samoa)

# Check locations without statins data

# American Samoa
# Bermuda
# Bolivia
# Greenland
# Ivory Coast
# Tanzania

# Save the data
saveRDS(dt_statins_scenarios, file = paste0(wd_data,"Statins/", "statin_data.rds"))

#print length(locs_statins)
locs_statins <- unique(dt_statins_scenarios$location)
print(paste0("Number of locations with statin data: ", length(locs_statins)))

#clean
rm(data.in.statin,dt_sta,dt_statins_interp,dt_statins_scenarios,dt_temp,
   dt_statins_current,dt_statins_target,interpolate_statins,dt_statins_scenarios_samoa)

#...........................................................
# Bp control----
#...........................................................

#...........................................................
## Coverage----
#...........................................................

# From coverage_newfig

#Using age-standardized rates of htn control from 2000-2019

ncdr <- read.csv(paste0(wd_raw,"NCD-RisC/","NCD-RisC_Lancet_2021_Hypertension_age_standardised_countries.csv"), stringsAsFactors = F)%>%
  select(Country.Region.World, ISO, Year, Proportion.of.controlled.hypertension.among.all.hypertension)%>%
  rename(control = Proportion.of.controlled.hypertension.among.all.hypertension,
         Country = Country.Region.World)%>%
  group_by(Country, ISO, Year)%>%
  summarise(control = mean(control))%>%
  group_by(Country, ISO)%>%
  mutate(change = shift(control, type='lead')- control,
         r_change = 100*change/control)


#https://www.statology.org/quadratic-regression-r/


ncdr$control2<-ncdr$control^2
quadraticModel <- lm(change ~ control + control2, data=ncdr%>%filter(Country=="Canada"))
summary(quadraticModel)

#create sequence of control values
controlValues <- seq(0, 0.60, 0.01)
#create list of predicted change using quadratic model
changePredict <- predict(quadraticModel,list(control=controlValues, control2=controlValues^2))

data<-ncdr%>%filter(Country=="Canada")

fit<-data.frame(controlValues=controlValues, 
                changePredict=changePredict)

ggplot(data, aes(x=control, y=change))+
  geom_point()+
  geom_line(data = fit, aes(x=controlValues, y=changePredict))+
  xlab("Baseline control")+
  ylab("Change in coverage")+
  ggtitle("Canada: 1990-2019")

#Add custom fxn line
quadraticModel2<-quadraticModel
quadraticModel2$coefficients[1]<-0
quadraticModel2$coefficients[2]<-0.43*0.527
quadraticModel2$coefficients[3]<- -0.43
quadraticModel2

#create sequence of control values
controlValues2 <- seq(0, 0.60, 0.01)
#create list of predicted change using quadratic model
changePredict2 <- predict(quadraticModel2,
                          list(control=controlValues2, 
                               control2=controlValues2^2))

fit2<-data.frame(controlValues2=controlValues2, 
                 changePredict2=changePredict2)

ggplot(data, aes(x=control, y=change))+
  geom_point()+
  geom_line(data = fit, 
            aes(x=controlValues, y=changePredict,color="Empirical"),
            size=1)+
  geom_line(data = fit2, 
            aes(x=controlValues2, y=changePredict2,color="Modeled"),
            size=1)+
  xlab("Baseline control")+
  ylab("Change in coverage")+
  ggtitle("Canada: 1990-2019")+
  scale_color_manual(name = "Models", 
                     values = c("Empirical" = "darkblue", "Modeled" = "red"))


#ggsave("Canada.png", height = 6, width =8)

#compare to other countries

more_data<-ncdr%>%filter(Country%in%c("Canada",
                                      "South Korea",
                                      "Germany",
                                      "Finland",
                                      "Iceland",
                                      "China"))

ggplot(more_data, aes(x=control, y=change))+
  geom_point(aes(colour=Country))+
  geom_line(data = fit, 
            aes(x=controlValues, y=changePredict),
            size=1, color="darkblue")+
  geom_line(data = fit2, 
            aes(x=controlValues2, y=changePredict2),
            size=1, color = "red")+
  xlab("Baseline control")+
  ylab("Change in coverage")+
  ylim(0,0.035)

#ggsave("other_countries.png", height = 6, width = 8)



## Ambitious----

quadraticModel3<-quadraticModel
quadraticModel3$coefficients[1]<-0
quadraticModel3$coefficients[2]<-0.285*0.75
quadraticModel3$coefficients[3]<- -0.285
quadraticModel3
#create sequence of control values
controlValues3 <- seq(0, 1, 0.01)
#create list of predicted change using quadratic model
changePredict3 <- predict(quadraticModel3,
                          list(control=controlValues3, 
                               control2=controlValues3^2))

fit3<-data.frame(controlValues3=controlValues3, 
                 changePredict3=changePredict3)


ggplot(data, aes(x=control, y=change))+
  geom_point()+
  geom_line(data = fit, 
            aes(x=controlValues, y=changePredict,color="Empirical"),
            size=1)+
  geom_line(data = fit2, 
            aes(x=controlValues2, y=changePredict2,color="Progress"),
            size=1)+
  geom_line(data = fit3, 
            aes(x=controlValues3, y=changePredict3,color="Aspirational"),
            size=1)+
  xlab("Baseline control")+
  xlim(0,1.1)+
  ylab("Change in coverage")+
  ggtitle("Canada: 1990-2019")+
  scale_color_manual(name = "Models", 
                     values = c("Empirical" = "darkblue", 
                                "Progress" = "red",
                                "Aspirational" = "darkgreen"))

#### Appendix plot####
ggplot(ncdr, aes(x=100*control, y=100*change))+
  geom_point()+
  geom_line(data = fit2, 
            aes(x=100*controlValues2, y=100*changePredict2,color="Progress"),
            size=1)+
  geom_line(data = fit3, 
            aes(x=100*controlValues3, y=100*changePredict3,color="Aspirational"),
            size=1)+
  xlab("Proportion of population with blood pressure controlled in year t (%)")+
  xlim(0,100)+
  ylim(0,5)+
  ylab("Additional proportion of population with blood pressure controlled in year t + 1 (%)")+
  scale_color_manual(name = "Models", 
                     values = c("Progress" = "red",
                                "Aspirational" = "blue"))

#ggsave("figures/scale-up.png",height = 6, width = 10)

ggplot(ncdr, aes(x=Year, y=control))+
  geom_point()

mean(ncdr%>%filter(Year==2019)%>%arrange(control)%>%pull(control))
df<-ncdr%>%filter(Year==2019)%>%arrange(desc(control))
df<-df[1:5,]
mean(df$control)


## add resolve data ----

rtsl<-read.csv(paste0(wd,"add_cov_data.csv"), stringsAsFactors = F)%>%
  filter(location%in%c("Bangladesh", "Colombia", "Ecuador", "Ethiopia", "Peru", "Vietnam", "India"))

rtsl[8,4]<-"Longest running HEARTS \nprogram (year 1)"
rtsl[9,4]<-"Longest running HEARTS \nprogram (year 2)"

rtsl<-rtsl%>%mutate(data = ifelse(data=="RTSL", "HEARTS program", data))

plot<-bind_rows(rtsl, ncdr%>%mutate(data="NCD-RisC"))%>%arrange(desc(data))

cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "grey", "#0072B2", "#D55E00", "#CC79A7")

mytheme <- theme_bw() + theme(legend.title = element_blank())
theme_set(mytheme)

# ggplot(plot%>%filter(change>=0),
#        aes(x=100*control, y=100*change, color=data, fill=data,
#            alpha=data, size=data, shape=data, linetype=data))+
#   geom_point()+
#   scale_fill_manual(values = c("#1E88E5", cbPalette[1],cbPalette[3],cbPalette[7],"black",'#D81B60'),
#                     aesthetics = c("colour", "fill")) +
#   scale_alpha_manual(values = c(1,1, 1,  1, 0.1, 1))+
#   scale_size_manual(values = c(1,2,2,2,1, 1))+
#   scale_shape_manual(values = c(NA, 24,23,15, 19, NA))+
#   scale_linetype_manual(values = c("solid", NA,NA,NA,NA, "solid"))+
#   guides(color = guide_legend(override.aes = list(alpha = 1)))+
#   xlab("Proportion of population with blood pressure controlled in year t (%)")+
#   xlim(0,100)+
#   coord_cartesian(ylim=c(0,9))+
#   ylab("Additional proportion of population with blood pressure controlled in year t+1 (%)")+
#   #theme(legend.title=element_blank())+
#   #theme_bw()+
#   geom_line(data = fit2%>%filter(changePredict2>=0),
#             aes(x=100*controlValues2, y=100*changePredict2,
#                 color=   "Progress scenario \nscale-up function",
#                 fill=    "Progress scenario \nscale-up function",
#                 alpha=   "Progress scenario \nscale-up function",
#                 size=    "Progress scenario \nscale-up function",
#                 shape =  "Progress scenario \nscale-up function",
#                 linetype="Progress scenario \nscale-up function"))+
#   geom_line(data = fit3%>%filter(changePredict3>=0),
#             aes(x=100*controlValues3, y=100*changePredict3,
#                 color=    "Aspirational scenario \nscale-up function",
#                 fill=     "Aspirational scenario \nscale-up function",
#                 alpha=    "Aspirational scenario \nscale-up function",
#                 size=     "Aspirational scenario \nscale-up function",
#                 shape =   "Aspirational scenario \nscale-up function",
#                 linetype= "Aspirational scenario \nscale-up function"))


# ggsave("../../output/fig_A5.pdf", height = 6, width = 8, dpi=600)



#########bau############
#https://www.mathepower.com/en/quadraticfunctions.php

quadraticModel_1b<-quadraticModel
quadraticModel_1b$coefficients[1]<-0
quadraticModel_1b$coefficients[2]<-0.45*0.45
quadraticModel_1b$coefficients[3]<- -0.45

quadraticModel_2b<-quadraticModel
quadraticModel_2b$coefficients[1]<-0
quadraticModel_2b$coefficients[2]<-0.4*0.4
quadraticModel_2b$coefficients[3]<- -0.4

quadraticModel_3b<-quadraticModel
quadraticModel_3b$coefficients[1]<-0
quadraticModel_3b$coefficients[2]<-0.467*0.35
quadraticModel_3b$coefficients[3]<- -0.467

quadraticModel_4b<-quadraticModel
quadraticModel_4b$coefficients[1]<-0
quadraticModel_4b$coefficients[2]<-0.7*0.3
quadraticModel_4b$coefficients[3]<- -0.7

quadraticModel_5b<-quadraticModel
quadraticModel_5b$coefficients[1]<-0
quadraticModel_5b$coefficients[2]<-0.7*0.25
quadraticModel_5b$coefficients[3]<- -0.7

quadraticModel_6b<-quadraticModel
quadraticModel_6b$coefficients[1]<-0
quadraticModel_6b$coefficients[2]<-0.6*0.25
quadraticModel_6b$coefficients[3]<- -0.6

quadraticModel_7b<-quadraticModel
quadraticModel_7b$coefficients[1]<-0
quadraticModel_7b$coefficients[2]<-0.85*0.2
quadraticModel_7b$coefficients[3]<- -0.85

quadraticModel_8b<-quadraticModel
quadraticModel_8b$coefficients[1]<-0
quadraticModel_8b$coefficients[2]<-0.8*0.2
quadraticModel_8b$coefficients[3]<- -0.8

quadraticModel_9b<-quadraticModel
quadraticModel_9b$coefficients[1]<-0
quadraticModel_9b$coefficients[2]<-0.6*0.2
quadraticModel_9b$coefficients[3]<- -0.6


#create sequence of control values
controlValues <- seq(0, 0.6, 0.01)
#create list of predicted change using quadratic model
changePredict_1 <- predict(quadraticModel_1b,
                           list(control=controlValues, 
                                control2=controlValues^2))
changePredict_2 <- predict(quadraticModel_2b,
                           list(control=controlValues, 
                                control2=controlValues^2))
changePredict_3 <- predict(quadraticModel_3b,
                           list(control=controlValues, 
                                control2=controlValues^2))
changePredict_4 <- predict(quadraticModel_4b,
                           list(control=controlValues, 
                                control2=controlValues^2))
changePredict_5 <- predict(quadraticModel_5b,
                           list(control=controlValues, 
                                control2=controlValues^2))
changePredict_6 <- predict(quadraticModel_6b,
                           list(control=controlValues, 
                                control2=controlValues^2))
changePredict_7 <- predict(quadraticModel_7b,
                           list(control=controlValues, 
                                control2=controlValues^2))
changePredict_8 <- predict(quadraticModel_8b,
                           list(control=controlValues, 
                                control2=controlValues^2))
changePredict_9 <- predict(quadraticModel_9b,
                           list(control=controlValues, 
                                control2=controlValues^2))


fit_1<-data.frame(controlValues=controlValues, 
                  changePredict=changePredict_1)
fit_2<-data.frame(controlValues=controlValues, 
                  changePredict=changePredict_2)
fit_3<-data.frame(controlValues=controlValues, 
                  changePredict=changePredict_3)
fit_4<-data.frame(controlValues=controlValues, 
                  changePredict=changePredict_4)
fit_5<-data.frame(controlValues=controlValues, 
                  changePredict=changePredict_5)
fit_6<-data.frame(controlValues=controlValues, 
                  changePredict=changePredict_6)
fit_7<-data.frame(controlValues=controlValues, 
                  changePredict=changePredict_7)
fit_8<-data.frame(controlValues=controlValues, 
                  changePredict=changePredict_8)
fit_9<-data.frame(controlValues=controlValues, 
                  changePredict=changePredict_9)

#....................................................
#fit each country to its BAU fxn - minimize errors
#....................................................

ncdr1<-ncdr%>%mutate(fit1=predict(quadraticModel_1b,
                                  list(control=control, 
                                       control2=control^2)),
                     fit2=predict(quadraticModel_2b,
                                  list(control=control, 
                                       control2=control^2)),
                     fit3=predict(quadraticModel_3b,
                                  list(control=control, 
                                       control2=control^2)),
                     fit4=predict(quadraticModel_4b,
                                  list(control=control, 
                                       control2=control^2)),
                     fit5=predict(quadraticModel_5b,
                                  list(control=control, 
                                       control2=control^2)),
                     fit6=predict(quadraticModel_6b,
                                  list(control=control, 
                                       control2=control^2)),
                     fit7=predict(quadraticModel_7b,
                                  list(control=control, 
                                       control2=control^2)),
                     fit8=predict(quadraticModel_8b,
                                  list(control=control, 
                                       control2=control^2)),
                     fit9=predict(quadraticModel_9b,
                                  list(control=control, 
                                       control2=control^2)),
                     error1 = sqrt((change-fit1)^2),
                     error2 = sqrt((change-fit2)^2),
                     error3 = sqrt((change-fit3)^2),
                     error4 = sqrt((change-fit4)^2),
                     error5 = sqrt((change-fit5)^2),
                     error6 = sqrt((change-fit6)^2),
                     error7 = sqrt((change-fit7)^2),
                     error8 = sqrt((change-fit8)^2),
                     error9 = sqrt((change-fit9)^2)
)


ncdr1<-ncdr1%>%select(Country, ISO, Year, error1, error2, error3, error4, 
                      error5, error6, error7, error8, error9)%>%
  filter(Year>=2010)%>%
  gather(model, error, -ISO, -Year, -Country)%>%
  group_by(ISO,Country, model)%>%
  summarise(error = sum(error, na.rm = T))%>%
  ungroup()%>%
  group_by(ISO, Country)%>%
  filter(error==min(error))%>%
  mutate(model = substr(model, 6,6))


ncdr<-left_join(ncdr, ncdr1%>%select(ISO, model))

ggplot(ncdr, aes(x=control, y=change))+
  geom_point(alpha=0.2)+
  geom_line(data = fit2, 
            aes(x=controlValues2, y=changePredict2,color="Progress"),
            size=1)+
  geom_line(data = fit3, 
            aes(x=controlValues3, y=changePredict3,color="Aspirational"),
            size=1)+
  geom_line(data = fit_1%>%mutate(model = 1), 
            aes(x=controlValues, y=changePredict,color="BAU"),
            size=1)+
  geom_line(data = fit_2%>%mutate(model = 2), 
            aes(x=controlValues, y=changePredict,color="BAU"),
            size=1)+
  geom_line(data = fit_3%>%mutate(model = 3), 
            aes(x=controlValues, y=changePredict,color="BAU"),
            size=1)+
  geom_line(data = fit_4%>%mutate(model = 4), 
            aes(x=controlValues, y=changePredict,color="BAU"),
            size=1)+
  geom_line(data = fit_5%>%mutate(model = 5), 
            aes(x=controlValues, y=changePredict,color="BAU"),
            size=1)+
  geom_line(data = fit_6%>%mutate(model = 6), 
            aes(x=controlValues, y=changePredict,color="BAU"),
            size=1)+
  geom_line(data = fit_7%>%mutate(model = 7), 
            aes(x=controlValues, y=changePredict,color="BAU"),
            size=1)+
  geom_line(data = fit_8%>%mutate(model = 8), 
            aes(x=controlValues, y=changePredict,color="BAU"),
            size=1)+
  geom_line(data = fit_9%>%mutate(model = 9), 
            aes(x=controlValues, y=changePredict,color="BAU"),
            size=1)+
  facet_wrap(~model)+
  xlab("Baseline control rate")+
  xlim(0,1)+
  ylim(0,0.05)+
  ylab("Change in HTN control coverage")+
  ggtitle("1990-2019")+
  scale_color_manual(name = "Models", 
                     values = c("Progress" = "#D81B60",
                                "Aspirational" = "#1E88E5",
                                "BAU" = "#E4AB00"))+
  theme_bw()


#ggsave("baseline_fxns.png", height=8, width=12)
#take out countries already at 0.512 and make a 'progress=bau' bin

#....................................................#
#library(stringr)

ggplot(ncdr%>%filter(model%in%c(1,5,9))%>%mutate(model = factor(model, levels=c(1,5,9),
                                                                labels=c("Best-performing quantile", "Middle-performing quantile", "Worst-performing quantile"))), 
       aes(x=control, y=change))+
  geom_point(alpha=0.2)+
  geom_line(data = fit2, 
            aes(x=controlValues2, y=changePredict2,color="Progress scenario \nscale-up function"),
            size=1)+
  geom_line(data = fit3, 
            aes(x=controlValues3, y=changePredict3,color="Aspirational scenario \nscale-up function"),
            size=1)+
  geom_line(data = fit_1%>%mutate(model = "Best-performing quantile"), 
            aes(x=controlValues, y=changePredict,color="Business as usual scenario \nscale-up function"),
            size=1)+
  geom_line(data = fit_5%>%mutate(model = "Middle-performing quantile"), 
            aes(x=controlValues, y=changePredict,color="Business as usual scenario \nscale-up function"),
            size=1)+
  geom_line(data = fit_9%>%mutate(model = "Worst-performing quantile"), 
            aes(x=controlValues, y=changePredict,color="Business as usual scenario \nscale-up function"),
            size=1)+
  facet_wrap(~model, ncol=1)+
  xlab("Proportion of population with blood pressure controlled in year t (%)")+
  xlim(0,1)+
  ylim(0,0.05)+
  ylab(str_wrap("Additional proportion of population with blood pressure controlled in year t+1 (%)",45))+
  ggtitle("1990-2019")+
  scale_color_manual(name = "Models", 
                     values = c("Progress scenario \nscale-up function" = "#D81B60",
                                "Aspirational scenario \nscale-up function" = "#1E88E5",
                                "Business as usual scenario \nscale-up function" = "#E4AB00"))+
  theme_bw()


#ggsave("../../output/fig_A6_alt.pdf", height=10, width=8, dpi=600)

#....................................................#


ggplot(ncdr, aes(x=control, y=change))+
  geom_point(alpha=0.2)+
  geom_line(data = fit2, 
            aes(x=controlValues2, y=changePredict2,color="Progress"),
            size=1)+
  geom_line(data = fit3, 
            aes(x=controlValues3, y=changePredict3,color="Aspirational"),
            size=1)+
  geom_line(data = fit_1%>%mutate(model = 1), 
            aes(x=controlValues, y=changePredict,color="BAU"),
            size=1)+
  geom_line(data = fit_2%>%mutate(model = 2), 
            aes(x=controlValues, y=changePredict,color="BAU"),
            size=1)+
  geom_line(data = fit_3%>%mutate(model = 3), 
            aes(x=controlValues, y=changePredict,color="BAU"),
            size=1)+
  geom_line(data = fit_4%>%mutate(model = 4), 
            aes(x=controlValues, y=changePredict,color="BAU"),
            size=1)+
  geom_line(data = fit_5%>%mutate(model = 5), 
            aes(x=controlValues, y=changePredict,color="BAU"),
            size=1)+
  geom_line(data = fit_6%>%mutate(model = 6), 
            aes(x=controlValues, y=changePredict,color="BAU"),
            size=1)+
  geom_line(data = fit_7%>%mutate(model = 7), 
            aes(x=controlValues, y=changePredict,color="BAU"),
            size=1)+
  geom_line(data = fit_8%>%mutate(model = 8), 
            aes(x=controlValues, y=changePredict,color="BAU"),
            size=1)+
  geom_line(data = fit_9%>%mutate(model = 9), 
            aes(x=controlValues, y=changePredict,color="BAU"),
            size=1)+
  xlab("Baseline control rate")+
  xlim(0,1)+
  ylim(0,0.05)+
  ylab("Change in HTN control coverage")+
  ggtitle("1990-2019")+
  scale_color_manual(name = "Models", 
                     values = c("Progress" = "#D81B60",
                                "Aspirational" = "#1E88E5",
                                "BAU" = "#E4AB00"))+
  theme_bw()

#ggsave("baseline_fxns_1.png", height=8, width=12)

#new fig


df<- read.csv(paste0(wd_raw,"NCD-RisC/","NCD-RisC_Lancet_2021_Hypertension_age_standardised_countries.csv"), stringsAsFactors = F)%>%
  select(Country.Region.World, ISO, Year, Proportion.of.controlled.hypertension.among.all.hypertension)%>%
  #filter(Year>=2000)%>%
  rename(control = Proportion.of.controlled.hypertension.among.all.hypertension,
         Country = Country.Region.World)%>%
  group_by(Country, ISO, Year)%>%
  summarise(control = mean(control))%>%
  mutate(Progress = control,
         Aspirational = control,
         `Business as usual` = control,
         p_change = NA,
         a_change = NA,
         aroc = NA)

#for those already above say 49%, we assume the progress ~ bau
plocs<-df%>%filter(control>0.49)%>%pull(Country)%>%unique()

df<-left_join(df, ncdr1%>%select(Country, ISO, model))

for(i in 1:31){
  
  if(i>5){
    temp<-df%>%filter(Year==2018+i)%>%
      mutate(Year= Year+1,
             p_change = Progress*0.43*0.527 + (-0.43)*(Progress^2),
             a_change = Aspirational*0.285*0.75 + (-0.285)*(Aspirational^2),
             aroc = ifelse(model==1, `Business as usual`*0.45*0.45 + (-0.45)*(`Business as usual`)^2,
                           ifelse(model==2, `Business as usual`*0.4*0.4 + (-0.4)*(`Business as usual`)^2,
                                  ifelse(model==3, `Business as usual`*0.467*0.35 + (-0.467)*(`Business as usual`)^2,
                                         ifelse(model==4, `Business as usual`*0.7*0.3 + (-0.7)*(`Business as usual`)^2,
                                                ifelse(model==5, `Business as usual`*0.7*0.25 + (-0.7)*(`Business as usual`)^2,
                                                       ifelse(model==6, `Business as usual`*0.6*0.25 + (-0.6)*(`Business as usual`)^2,
                                                              ifelse(model==7, `Business as usual`*0.85*0.2 + (-0.85)*(`Business as usual`)^2,
                                                                     ifelse(model==8, `Business as usual`*0.8*0.2 + (-0.8)*(`Business as usual`)^2,
                                                                            `Business as usual`*0.6*0.2 + (-0.6)*(`Business as usual`)^2)
                                                              ))))))),
             aroc = ifelse(Country%in%plocs, p_change, aroc),
             p_change = ifelse(p_change<0,0,p_change),
             a_change = ifelse(a_change<0,0,a_change),
             aroc     = ifelse(aroc<0,0,aroc),
             Progress = Progress + p_change,
             Aspirational = Aspirational + a_change,
             `Business as usual` = `Business as usual` + aroc)
  }
  else{
    temp<-df%>%filter(Year==2018+i)%>%
      mutate(Year= Year+1,
             Progress = Progress,
             Aspirational = Aspirational,
             `Business as usual` = `Business as usual`,
             p_change = 0,
             a_change = 0,
             aroc     = 0)
  }
  df<-bind_rows(df, temp)
}

regions<-read.csv(paste0(wd,"Country_groupings_extended.csv"), stringsAsFactors = F)%>%
  rename(ISO = iso3)%>%
  select(ISO, wb2021)

df<-left_join(df, regions)%>%
  mutate(`Business as usual` = ifelse(`Business as usual`>Progress, Progress, `Business as usual`))

plot<-df%>%select(Country, ISO, Year, wb2021, Progress, Aspirational, `Business as usual`)%>%
  gather(Scenario, control, - Country, - Year, - wb2021, -ISO)%>%ungroup()

plot$wb2021<-factor(plot$wb2021, levels = c("LIC", "LMIC", "UMIC", "HIC"))
plot$Scenario<-factor(plot$Scenario, levels = c("Business as usual", 
                                                "Progress",
                                                "Aspirational"))

##graph it
ggplot(na.omit(plot), 
       aes(x=Year, y=100*control, group = Country))+
  geom_line(size = 0.5)+
  facet_grid(wb2021~Scenario)+
  ylab("Hypertension control rate (%)")+
  geom_line(y=51.2, color="red", linetype='dotted')

#ggsave("../../output/fig_A7.pdf", height = 8, width = 12, dpi=600)

#write.csv(plot, "../../output/scale_up_data_2022.csv", row.names = F)

covfxn<-df%>%filter(Year>=2017)

covfxn$aroc[covfxn$Year<=2019]<-0
covfxn$p_change[covfxn$Year<=2019]<-0
covfxn$a_change[covfxn$Year<=2019]<-0

#write.csv(covfxn, "model/covfxn.csv", row.names = F)

#plus salt ---

#add salt impacts
#add salt impacts

data.in<-fread(paste0(wd,"bp_data5.csv"))%>%
  select(-Year, -Country)%>%
  rename(location = location_gbd,
         age = Age.group)

#data.in<-fread(paste0(wd,"bp_data6.csv"))

data.in$salt[data.in$location=="China"]<-4.83*2.52

names<-read.csv(paste0(wd,"Country_groupings_extended.csv"), stringsAsFactors = F)%>%
  select(location_gbd, iso3)%>%
  rename(location = location_gbd)

data.in<-left_join(data.in, names)

bpcats<-c("<120", "120-129", "130-139", 
          "140-149", "150-159", "160-169", 
          "170-179", "180+")

data.in<-merge(bpcats, data.in)%>%rename(bp_cat = x)

#b_rates<-fread("../base_rates_2019.csv")%>%
b_rates<-fread(paste0("C:/Users/wrgar/OneDrive - UW/02Work/ResolveToSaveLives/","100MLives/data/","base_rates_2022.csv"))%>%
  filter(cause=="ihd")%>%
  select(year, location, sex, age, Nx)%>%
  rename(Year=year)

source(paste0(wd_code,"functions_review_6_100.R"))

repYear<-function(row){
  2017+floor((row-1)/224)
}

bp_out<-data.frame(Year=numeric(),
                   ref=numeric(),
                   asp=numeric(),
                   location=character()
)

data.in<-data.table(data.in)

for (i in unique(data.in$location)){
  DT<-unique(data.in[location==i][,Year:=2017][,-c("Lower95", "Upper95")])
  DT.in<-DT[rep(seq(1,nrow(DT)), 24)][, Year:=repYear(.I)]
  bp_prob_salt<-get.bp.prob(DT.in, 0.15, 'percent', 2023, 2030, 0, "baseline")
  DT.in<-DT[rep(seq(1,nrow(DT)), 24)][, Year:=repYear(.I)]
  bp_prob_salt2<-get.bp.prob(DT.in, 0.3, 'percent', 2023, 2027, 0, "baseline")
  DT.in<-DT[rep(seq(1,nrow(DT)), 24)][, Year:=repYear(.I)]
  bp_prob_base<-get.bp.prob(DT.in, 0, 'percent', 2023, 2025, 0, "baseline")
  setnames(bp_prob_base, "prob", "prob_0")
  setnames(bp_prob_salt2, "prob", "prob_2")
  
  bp_probs<-merge(bp_prob_salt, bp_prob_base, 
                  by=c("age","sex", "bp_cat", "Year", "location")) 
  bp_probs<-merge(bp_prob_salt2, bp_probs, 
                  by=c("age","sex", "bp_cat", "Year", "location")) 
  
  #duplicating data to be age-specific
  bp_probs[, age:=as.numeric(substr(age, 1,2))]
  bp_probs<-bp_probs[rep(seq_len(nrow(bp_probs)), each=5)]
  bp_probs[,age2:=rep(1:5, nrow(bp_probs)/5)][,age:=age+age2-1]
  
  over90<-bp_probs[age==89]
  
  over90<-over90[rep(seq_len(nrow(over90)), each=6)]
  over90[,age2:=rep(1:6, nrow(over90)/6)][,age:=age+age2]
  
  #bind  
  bp_probs<-rbindlist(list(bp_probs, over90))
  
  bps<-left_join(bp_probs, b_rates%>%filter(location==i), 
                 by=c("age", "sex", "location", "Year"))
  
  normo<-bps%>%filter(bp_cat=="<120" | bp_cat=="120-129" | bp_cat=="130-139")%>%
    group_by(Year, age, sex, location)%>%
    summarise(propbase = sum(prob_0), 
              propref=sum(prob), 
              propasp=sum(prob_2),
              Nx=sum(Nx)/2)%>%
    ungroup()%>%group_by(Year, location)%>%
    summarise(base=weighted.mean(propbase, Nx),
              ref=weighted.mean(propref, Nx),
              asp=weighted.mean(propasp, Nx))%>%
    mutate(ref=ref-base, asp=asp-base)%>%
    select(Year, location, ref, asp)
  
  bp_out<-bind_rows(normo,bp_out)
  
}

bp_out<-left_join(bp_out, names)
#newdf<-left_join(df, bp_out%>%rename(ISO=iso3))

add<-merge(2041:2050, bp_out%>%filter(Year==2040)%>%
             ungroup()%>%select(-Year))%>%rename(Year=x)

newdf<-bind_rows(bp_out, add)%>%filter(Year<=2050)


covfxn<-left_join(covfxn%>%rename(iso3 = ISO), newdf)

covfxn<-covfxn%>%
  mutate(reach_base = ifelse(0.512<=`Business as usual`, Year, NA),
         refwsalt = ifelse(0.512<=Progress+ref, Year, NA),
         aspwsalt = ifelse(0.512<=Aspirational+asp, Year, NA),
         reach_75 = ifelse(0.512<=Progress, Year, NA),
         reach_975 = ifelse(0.512<=Aspirational, Year, NA))%>%
  group_by(iso3)%>%
  mutate(reach_base = min(reach_base, na.rm=T),
         refwsalt = min(refwsalt, na.rm=T),
         aspwsalt = min(aspwsalt, na.rm=T),
         reach_75 = min(reach_75, na.rm=T),
         reach_975 = min(reach_975, na.rm=T))%>%
  ungroup()


# 
# covfxn<-covfxn%>%filter(location!="Global", 
#                         location!="American Samoa",
#                         location!="Andorra",
#                         location!= "Bermuda",
#                         location!= "Dominica",
#                         location!="Greenland",
#                         location!="Marshall Islands",
#                         location!="Northern Mariana Islands",
#                         location!="Palestine",
#                         location!="Taiwan (Province of China)",
#                         location!="Guam",
#                         location!="Puerto Rico",
#                         location!="South Sudan",
#                         location!="Virgin Islands, U.S.")

mean(covfxn$reach_75-covfxn$refwsalt)
mean(covfxn$reach_975-covfxn$aspwsalt)


##cumulative increases
covfxn<-covfxn%>%
  group_by(iso3)%>%
  mutate(p_change = Progress - control,
         p_change = ifelse(p_change<0,0, p_change),
         a_change = Aspirational - control,
         a_change = ifelse(a_change<0,0,a_change),
         aroc     = `Business as usual` - control,
         aroc     = ifelse(aroc<0,0,aroc))

# covfxn$p_change[covfxn$Year<=2022]<-0
# covfxn$a_change[covfxn$Year<=2022]<-0
# covfxn$aroc[covfxn$Year<=2022]<-0

covfxn$p_change[covfxn$Year<=2025]<-0
covfxn$a_change[covfxn$Year<=2025]<-0
covfxn$aroc[covfxn$Year<=2025]<-0

#update Jan 18 - scale-up relative to baseline
covfxn<-covfxn%>%
  group_by(iso3)%>%
  mutate(aroc2 = aroc/(1-control),
         p_change2 = p_change/(1-control),
         a_change2 = a_change/(1-control))


#add ideal - treat all htn in 2023
#covfxn<-covfxn%>%mutate(ideal = ifelse(Year>=2023, (1-control), 0))
covfxn<-covfxn%>%mutate(ideal = ifelse(Year>=2025, (1-control), 0))

# Fix location names
covfxn$location[covfxn$location=="Cote d'Ivoire"]<-"Ivory Coast"

write.csv(covfxn, paste0(wd_data,"covfxn2.csv"), row.names = F)


# Compute linear intervention scale up-----

inc <- fread(paste0(wd_data,"covfxn2.csv"))

# Fixes location names

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


inc <- as.data.table(inc)
inc[, location := fcoalesce(name_map[location], location)]

## keep location, year, control for 2024
inc <- inc[Year == 2024, .(location, iso3, Year, control)]

#rename location, year
setnames(inc, c("location", "Year"), c("location", "year"))

inc[, control_target := 0.5]

interpolate_control <- function(dt_raw,
                                start_year  = 2024L,
                                end_year    = 2050L,
                                current_col = "control",
                                target_col  = "control_target",
                                out_col     = "control_scaled") {
  
  stopifnot(is.data.table(dt_raw))
  
  yrs  <- seq.int(start_year, end_year)
  span <- end_year - start_year
  
  dt_out <- dt_raw[, {
    
    curr <- get(current_col)[1]     # baseline control
    tgt  <- get(target_col)[1]      # baseline target
    
    if (curr > tgt) {
      scaled_vals <- rep(0, length(yrs))
    } else {
      delta <- tgt - curr
      scaled_vals <- curr + delta * (yrs - start_year) / span
    }
    
    .(
      year          = yrs,
      baseline_ctrl = curr,          # preserve baseline value
      scaled        = scaled_vals
    )
    
  }, by = .(location)]
  
  # rename scaled column
  setnames(dt_out, "scaled", out_col)
  
  # reorder columns
  setcolorder(dt_out, c("location", "year", "baseline_ctrl", out_col))
  
  dt_out[]
}


dt_hbp_control <- interpolate_control(
  inc,  
  start_year  = 2026L,
  end_year    = 2040L,
  current_col = "control",
  target_col  = "control_target",
  out_col     = "control_scaled"
)

# take only 2024 rows (baseline year)
dt_hbp_control_2024 <- dt_hbp_control[year == 2026]
dt_hbp_control_2040 <- dt_hbp_control[year == 2040]

# ----- 1. Create 2020–2023 block
dt_hbp_control_2020_2023 <- dt_hbp_control_2024[
  , .(year = 2020:2025,
      baseline_ctrl = baseline_ctrl,
      control_scaled = control_scaled),
  by = .(location)
]

# ----- 2. Create 2041–2050 block
dt_hbp_control_2041_2050 <- dt_hbp_control_2040[
  , .(year = 2041:2050,
      baseline_ctrl = baseline_ctrl,
      control_scaled = control_scaled),
  by = .(location)
]

# ----- 3. Combine all
dt_hbp_control <- rbindlist(
  list(
    dt_hbp_control_2020_2023,
    dt_hbp_control,
    dt_hbp_control_2041_2050
  ),
  use.names = TRUE
)[order(location, year)]



# coverage scale up
dt_hbp_control[, coverage_scaleup := control_scaled - baseline_ctrl]

# Save the data
saveRDS(dt_hbp_control, file = paste0(wd_data,"BloodPressure/", "hbp_control_data.rds"))

#print length(locs_statins)
locs_statins <- unique(dt_hbp_control$location)
print(paste0("Number of locations with data: ", length(locs_statins)))

#clean
rm(dt_hbp_control, dt_hbp_control_2020_2023, inc, interpolate_control, name_map)




# inc <- fread(paste0(wd_data,"covfxn2.csv"))
# 
# # Fixes location names
# 
# name_map <- c(
#   "Brunei"                            = "Brunei Darussalam",
#   "Cape Verde"                        = "Cabo Verde",
#   "Cote d'Ivoire"                     = "Ivory Coast",
#   "Czech Republic"                    = "Czechia",
#   "Federated States of Micronesia"    = "Micronesia (Federated States of)",
#   "Iran"                              = "Iran (Islamic Republic of)",
#   "Laos"                              = "Lao People's Democratic Republic",
#   "Macedonia"                         = "North Macedonia",
#   "Moldova"                           = "Republic of Moldova",
#   "South Korea"                       = "Republic of Korea",
#   "Swaziland"                         = "Eswatini",
#   "Syria"                             = "Syrian Arab Republic",
#   "The Bahamas"                       = "Bahamas",
#   "The Gambia"                        = "Gambia",
#   "Venezuela"                         = "Venezuela (Bolivarian Republic of)",
#   "Vietnam"                           = "Viet Nam",
#   "North Korea"                       = "Democratic People's Republic of Korea"
# )
# 
# 
# inc <- as.data.table(inc)
# inc[, location := fcoalesce(name_map[location], location)]
# 
# ## keep location, year, control for 2024
# inc <- inc[Year == 2024, .(location, iso3, Year, control)]
# 
# #rename location, year
# setnames(inc, c("location", "Year"), c("location", "year"))
# 
# inc[, control_target := 0.5]
# 
# interpolate_control <- function(dt_raw,
#                                 start_year  = 2024L,
#                                 end_year    = 2050L,
#                                 current_col = "control",
#                                 target_col  = "control_target",
#                                 out_col     = "control_scaled") {
#   
#   stopifnot(is.data.table(dt_raw))
#   
#   yrs  <- seq.int(start_year, end_year)
#   span <- end_year - start_year
#   
#   dt_out <- dt_raw[, {
#     
#     curr <- get(current_col)[1]     # baseline control
#     tgt  <- get(target_col)[1]      # baseline target
#     
#     if (curr > tgt) {
#       scaled_vals <- rep(0, length(yrs))
#     } else {
#       delta <- tgt - curr
#       scaled_vals <- curr + delta * (yrs - start_year) / span
#     }
#     
#     .(
#       year          = yrs,
#       baseline_ctrl = curr,          # preserve baseline value
#       scaled        = scaled_vals
#     )
#     
#   }, by = .(location)]
#   
#   # rename scaled column
#   setnames(dt_out, "scaled", out_col)
#   
#   # reorder columns
#   setcolorder(dt_out, c("location", "year", "baseline_ctrl", out_col))
#   
#   dt_out[]
# }
# 
# 
# dt_hbp_control <- interpolate_control(
#   inc,  
#   start_year  = 2024L,
#   end_year    = 2050L,
#   current_col = "control",
#   target_col  = "control_target",
#   out_col     = "control_scaled"
# )
# 
# # take only 2024 rows
# dt_hbp_control_2020_2023 <- dt_hbp_control[year == 2024]
# 
# # expand each row into 4 rows (2020–2023)
# dt_hbp_control_2020_2023 <- dt_hbp_control_2020_2023[
#   , .(year = 2020:2023,
#       baseline_ctrl = baseline_ctrl,
#       control_scaled = baseline_ctrl),
#   by = .(location)
# ]
# 
# # combine
# dt_hbp_control <- rbindlist(
#   list(dt_hbp_control_2020_2023, dt_hbp_control),
#   use.names = TRUE
# )[order(location, year)]
# 
# # coverage scale up
# dt_hbp_control[, coverage_scaleup := control_scaled - baseline_ctrl]
# 
# # Save the data
# saveRDS(dt_hbp_control, file = paste0(wd_data,"BloodPressure/", "hbp_control_data.rds"))
# 
# #print length(locs_statins)
# locs_statins <- unique(dt_hbp_control$location)
# print(paste0("Number of locations with data: ", length(locs_statins)))
# 
# #clean
# rm(dt_hbp_control, dt_hbp_control_2020_2023, inc, interpolate_control, name_map)
