#!/usr/bin/env Rscript
# 
# Mainly from here:
# app.R
# Live version: https://github.com/ocelhay/como/blob/master/inst/comoapp/app.R
# Static version: https://github.com/ocelhay/como/blob/1b61938191d9f63d512a3aaec9f5271a3ca0ed5a/inst/comoapp/app.R

# model_repeat.R
# Live version: https://github.com/ocelhay/como/blob/master/inst/comoapp/www/model/model_repeat.R
# Static version: https://github.com/ocelhay/como/blob/1b61938191d9f63d512a3aaec9f5271a3ca0ed5a/inst/comoapp/www/model/model_repeat.R

source("R/model_once.R")


# Return name of worksheet within which a particular parameter is found
find_parameter_worksheet <- function(parameter_name, template){

    worksheets <- names(template)

    for( w in worksheets ){
	if( "Parameter" %in% names(template[[w]]) ){
        if( parameter_name %in% template[[w]]$Parameter ){

		return( list(worksheet = as.character(w), row = which(parameter_name == template[[w]]$Parameter)) )
        }
	}
    }
}



load_template <- function(file_path, country_name, USE_CPP = FALSE){
    
    raw_template <- parse_excel_template(file_path)
    list_template <- clean_excel_template(raw_template, country_name, USE_CPP)
    
    return( list_template )
}


parse_excel_template <- function(file_path){
    
    template <- list()
    
    # Line 609 (app.R)
    template[["version"]] <- read_excel(file_path, sheet = 1)
    template[["version_template"]] <- names(template[["version"]])[1]
    
    # Line 625 (app.R)
    # Epidemiology Sheet
    df_epidemiology <- read_excel(file_path, sheet = "Epidemiology")
    names(df_epidemiology) <- c("date", "cases", "deaths", "seroprevalence")
    template[["Epidemiology"]] <- df_epidemiology
    
    # Severity/Mortality Sheet
    df_mortality <- read_excel(file_path, sheet = "Severity-Mortality") 
    names(df_mortality) <- c("age_category", "ifr", "ihr")
    template[["Severity-Mortality"]] <- df_mortality
    
    # Population Sheet
    template[["Population"]] <- read_excel(file_path, sheet = "Population") 
    
    template[["Parameters"]] <- read_excel(file_path, sheet = "Parameters")
    template[["Country Area Param"]] <- read_excel(file_path, sheet = "Country Area Param")
    template[["Virus Param"]] <- read_excel(file_path, sheet = "Virus Param")
    template[["Hospitalisation Param"]] <- read_excel(file_path, sheet = "Hospitalisation Param")
    template[["Interventions Param"]] <- read_excel(file_path, sheet = "Interventions Param")
    template[["Interventions"]] <- read_excel(file_path, sheet = "Interventions")
    
    if( "Extra Param" %in% excel_sheets(file_path) ){
      cat("Reading Extra Param sheet\n")
      template[["Extra Param"]] <- read_excel(file_path, sheet = "Extra Param")
    }
    
    return( template )
}

clean_excel_template <- function(raw_template, country_name, USE_CPP = FALSE){

# Line 468 (app.R)
population_rv <- list(data = NULL)
cases_rv <- list(data = NULL)
mort_sever_rv <- list(data = mort_sever_default)
status_app <- list(status = "No Baseline")
simul_baseline <- list(results = NULL, baseline_available = FALSE)
simul_interventions <- list(results = NULL, interventions_available = FALSE)


# Line 475 (app.R)
# Management of interventions ----
interventions <- list(baseline_mat = tibble(NULL), 
                                baseline_age_groups = list(),
                                future_mat = tibble(NULL),
                                future_age_groups = list(),
                                valid_baseline_interventions = TRUE, 
                                message_baseline_interventions = NULL,
                                valid_future_interventions = TRUE, 
                                message_future_interventions = NULL)

# Process the epidemiology sheet
cases_rv$data <- raw_template[["Epidemiology"]] %>%
  mutate(date = as.Date(date), cumulative_death = cumsum(deaths)) %>%
  as.data.frame()

mort_sever_rv$data <- raw_template[["Severity-Mortality"]] %>%
  mutate(ihr = ihr/100) %>%  # starting unit should be % - scaling to a value between 0 and 1
  mutate(ifr = ifr/max(ifr))  # starting unit should be % - scaling to a value between 0 and 1


names(raw_template[["Population"]]) <- c("age_category", "pop", "birth", "death")
population_rv$data <- raw_template[["Population"]] %>%
    transmute(country = NA, age_category, pop, birth, death)

# Parameters Sheets
param <- bind_rows(raw_template[["Parameters"]],
                   raw_template[["Country Area Param"]],
                   raw_template[["Virus Param"]],
                   raw_template[["Hospitalisation Param"]],
                   raw_template[["Interventions Param"]]) %>%
    mutate(Value_Date = as.Date(Value_Date)) %>%
    drop_na(Parameter)

# Extra Parameters sheet (if exists)
if( "Extra Param" %in% names(raw_template) ){
  cat("Reading Extra Param sheet\n")
  extra_param <- template[["Extra Param"]] %>%
      mutate(Value_Date = as.Date(Value_Date)) %>%
      drop_na(Parameter)
  
  startdate <- param$Value_Date[param$Parameter == "date_range_simul_start"]
  extra_param$Value[extra_param$Parameter == "date_range_variant_start"]  <- as.numeric(extra_param$Value_Date[extra_param$Parameter == "date_range_variant_start"] - startdate)
  
  param <- bind_rows(param, extra_param)
}


# Construct "input" list
input <- list(
  country_contact = country_name,
  date_range = c(param$Value_Date[param$Parameter == "date_range_simul_start"], 
                 param$Value_Date[param$Parameter == "date_range_simul_end"])
)

param_list <- param[,"Value", drop = T]
names(param_list) <- param$Parameter

input <- c(input, param_list)


# Line 728 (app.R)
# Update interventions in the UI: read "Interventions" sheet and validate
interventions_excel <- raw_template[["Interventions"]] %>% filter(!is.na(Intervention))
names(interventions_excel) <- c("intervention", "date_start", "date_end", "value", "unit",
    "age_group", "apply_to")


if(all(interventions_excel$intervention %in% valid_interventions_v17)) message("Okay, all interventions are valid.")
if(! all(interventions_excel$intervention %in% valid_interventions_v17)) stop("Stop, some interventions are not valid.")


# Line 738 (app.R)
# Update interventions in the UI: baseline interventions
interventions_excel_baseline <- interventions_excel %>% 
  filter(apply_to == "Baseline (Calibration)")

# inputs() needs to be aware of these interventions ... <------
interventions$baseline_mat <- interventions_excel_baseline

# Fill list of age groups
vec <- interventions$baseline_mat$age_group

if(length(vec) > 0) {
  for (i in 1:length(vec)) {
    if(is.na(vec[i])){
      interventions$baseline_age_groups[[i]] <- parse_age_group("1-21")
    }else{
      interventions$baseline_age_groups[[i]] <- parse_age_group(vec[i])
    }
  }
}


nb_interventions_baseline <- interventions_excel_baseline %>% nrow()
if(nb_interventions_baseline > 0) {
  
  for (i in 1:nb_interventions_baseline) {
    input[[paste0("baseline_intervention_", i)]] = interventions_excel_baseline[[i, "intervention"]]
    
    input[[paste0("baseline_daterange_", i)]] = c(
                         start = interventions_excel_baseline[[i, "date_start"]], 
                         end = interventions_excel_baseline[[i, "date_end"]])
    
    input[[paste0("baseline_coverage_", i)]]  = c(value = interventions_excel_baseline[[i, "value"]])
    
    if( is.na(interventions_excel_baseline$age_group[i]) ){
      input[[paste0("baseline_age_group_", i)]] = parse_age_group("1-21")
    }else{
      input[[paste0("baseline_age_group_", i)]] = vec_age_categories[parse_age_group(interventions_excel_baseline$age_group[i])]
    }
  }
}


# Previous results are no longer valid
simul_interventions$results <- NULL

# Line 789 (app.R): source("R/model_repeat.R")

# ----------------- model_repeat.R ----------

# Definitions of several variables ----
popstruc <- population_rv$data %>% 
  select(age_category, pop) %>% 
  rename(agefloor = age_category) %>% 
  as.data.frame()

popbirth <- population_rv$data %>% 
  select(age_category, birth) %>% 
  as.data.frame() # unit should be per person per day

mort <- population_rv$data %>% 
  pull(death) # unit should be per person per day

ihr <- mort_sever_rv$data %>% 
  select(age_category, ihr) %>% 
  as.data.frame()

ifr <- mort_sever_rv$data %>% 
  select(age_category, ifr) %>% 
  as.data.frame()

# Complete contact Matrices ----
c_home <- contact_home[[input$country_contact]] %>% as.matrix()
c_school <- contact_school[[input$country_contact]] %>% as.matrix()
c_work <- contact_work[[input$country_contact]] %>% as.matrix()
c_other <- contact_other[[input$country_contact]] %>% as.matrix()
nce <- A - length(c_home[1, ])

contact_home <- matrix(0, nrow = A, ncol = A)
contact_school <- matrix(0, nrow = A, ncol = A)
contact_work <- matrix(0, nrow = A, ncol = A)
contact_other <- matrix(0, nrow = A, ncol = A)

for (i in 1:(A - nce)) {
  for (j in 1:(A - nce)) {
    contact_home[i, j] <- c_home[i, j]
    contact_school[i, j] <- c_school[i, j]
    contact_work[i, j] <- c_work[i, j]
    contact_other[i, j] <- c_other[i, j]
  }
}

for (i in (A + 1 - nce):A) {
  for (j in 1:(A - nce)) {
    contact_home[i, j] <- c_home[(A - nce), j]
    contact_school[i, j] <- c_school[(A - nce), j]
    contact_work[i, j] <- c_work[(A - nce), j]
    contact_other[i, j] <- c_other[(A - nce), j]
  }
}
for (i in 1:(A - nce)) {
  for (j in (A + 1 - nce):A) {
    contact_home[i, j] <- c_home[i, (A - nce)]
    contact_school[i, j] <- c_school[i, (A - nce)]
    contact_work[i, j] <- c_work[i, (A - nce)]
    contact_other[i, j] <- c_other[i, (A - nce)]
  }
}
for (i in (A + 1 - nce):A) {
  for (j in (A + 1 - nce):A) {
    contact_home[i, j] <- c_home[(A - nce), (A - nce)]
    contact_school[i, j] <- c_school[(A - nce), (A - nce)]
    contact_work[i, j] <- c_work[(A - nce), (A - nce)]
    contact_other[i, j] <- c_other[(A - nce), (A - nce)]
  }
}

# Define time variables ----
startdate <- input$date_range[1]
stopdate <- input$date_range[2]
times <- seq(0, as.numeric(stopdate - startdate))

# Define parameters vector ----
parameters <- input[
  c("p", "rho", "omega", "gamma", "nui", "report", "reportc", "reporth", 
    "beds_available", "icu_beds_available", "ventilators_available", 
    "pdeath_h", "pdeath_hc", "pdeath_icu", "pdeath_icuc", 
    "pdeath_vent", "pdeath_ventc", "ihr_scaling", "nus", 
    "nu_icu", "nu_vent", "rhos", "amp", 
    "pclin", "prob_icu", "prob_vent", "selfis_eff", "dist_eff", "hand_eff", 
    "work_eff", "w2h", "s2h", "cocoon_eff", "age_cocoon", 
    "vaccine_eff", "vac_campaign", "mean_imports", "screen_test_sens", 
    "screen_overdispersion", "quarantine_days", "quarantine_effort", 
    "quarantine_eff_home", "quarantine_eff_other", "household_size", 
    "noise", "iterations", "confidence",
    # additions in v14.14:
    "mass_test_sens", "isolation_days",
    # additions in v15.1:
    "pdeath_ho", "pdeath_hco", "pdeath_icuo", "pdeath_icuco",
    "propo2", "dexo2", "dexo2c", "dexv", "dexvc", "vent_dex",
    "mask_eff",
    # additions in v16.2:
    "prob_icu_v", "prob_icu_vr", "prob_icu_r", "prob_v_v", "prob_v_vr", "prob_v_r",
    "pclin_v", "pclin_vr", "pclin_r", "sigmaEV", "sigmaEVR", "sigmaER", "sigmaR", "vac_dur",
    "vac_dur_r", "report_natdeathI", "report_natdeathCL", "report_v",
    "report_cv", "report_vr", "report_cvr", "report_r", "report_cr", "reporth_ICU",
    "report_death_HC", "pdeath_vent_hc", "pdeath_icu_hc", "pdeath_icu_hco",
    "reporth_g", "seroneg",
    "vaccine_eff_r", "pre",
    # addition in v17:
    "init", "sample_size", "se", "sp",
    "phi",
    # addition from como_command_line
    "new_variant_p_multiplier", "date_range_variant_start"
  )] %>% 
  unlist()

parameters <- c(
  parameters, 
  give = 95, 
  nusc = input$nus, 
  nu_icuc = input$nu_icu, 
  nu_ventc = input$nu_vent)

ihr[,2]<- parameters["ihr_scaling"]*ihr[,2]   

# Scale parameters to percentages/ rates
parameters["rho"]<-parameters["rho"]/100
parameters["omega"]<-(1/(parameters["omega"]*365))
parameters["gamma"]<-1/parameters["gamma"]
parameters["nui"]<-1/parameters["nui"]
parameters["report"]<-parameters["report"]/100
parameters["reportc"]<-parameters["reportc"]/100
parameters["report_v"]<-parameters["report_v"]/100
parameters["report_cv"]<-parameters["report_cv"]/100
parameters["report_vr"]<-parameters["report_vr"]/100
parameters["report_cvr"]<-parameters["report_cvr"]/100
parameters["report_r"]<-parameters["report_r"]/100
parameters["report_cr"]<-parameters["report_cr"]/100
parameters["reporth"]<-parameters["reporth"]/100
parameters["nus"]<-1/parameters["nus"]
parameters["rhos"]<-parameters["rhos"]/100
parameters["amp"]<-parameters["amp"]/100
parameters["selfis_eff"]<-parameters["selfis_eff"]/100
parameters["dist_eff"]<-parameters["dist_eff"]/100
parameters["hand_eff"]<-parameters["hand_eff"]/100
parameters["mask_eff"]<-parameters["mask_eff"]/100
parameters["work_eff"]<-parameters["work_eff"]/100
parameters["w2h"]<-parameters["w2h"]/100
parameters["s2h"]<-parameters["s2h"]/100
parameters["cocoon_eff"]<-parameters["cocoon_eff"]/100
parameters["age_cocoon"]<-floor((parameters["age_cocoon"]/5)+1)
parameters["vaccine_eff"]<-parameters["vaccine_eff"]/100
parameters["vaccine_eff_r"]<-parameters["vaccine_eff_r"]/100
age_vaccine_min<-(parameters["age_vaccine_min"])
age_vaccine_max<-(parameters["age_vaccine_max"])
# parameters["vaccine_cov"]<-parameters["vaccine_cov"]/100
# parameters["vac_campaign"]<-parameters["vac_campaign"]*7
parameters["screen_test_sens"]<-parameters["screen_test_sens"]/100
parameters["quarantine_days"]<-parameters["quarantine_days"]
parameters["quarantine_effort"]<-1/parameters["quarantine_effort"]
parameters["quarantine_eff_home"]<-parameters["quarantine_eff_home"]/-100
parameters["quarantine_eff_other"]<-parameters["quarantine_eff_other"]/100
parameters["give"]<-parameters["give"]/100
parameters["pdeath_h"]<-parameters["pdeath_h"]/100
parameters["pdeath_ho"]<-parameters["pdeath_ho"]/100
parameters["pdeath_hc"]<-parameters["pdeath_hc"]/100
parameters["pdeath_hco"]<-parameters["pdeath_hco"]/100
parameters["pdeath_icu"]<-parameters["pdeath_icu"]/100
parameters["pdeath_icuo"]<-parameters["pdeath_icuo"]/100
parameters["pdeath_icuc"]<-parameters["pdeath_icuc"]/100
parameters["pdeath_icuco"]<-parameters["pdeath_icuco"]/100
parameters["pdeath_vent"]<-parameters["pdeath_vent"]/100
parameters["pdeath_ventc"]<-parameters["pdeath_ventc"]/100
parameters["nusc"]<-1/parameters["nusc"]
parameters["nu_icu"]<-1/parameters["nu_icu"]
parameters["nu_icuc"]<-1/parameters["nu_icuc"]
parameters["nu_vent"]<-1/parameters["nu_vent"]
parameters["nu_ventc"]<-1/parameters["nu_ventc"]
parameters["pclin"]<-parameters["pclin"]/100
parameters["prob_icu"]<-parameters["prob_icu"]/100
parameters["prob_vent"]<-parameters["prob_vent"]/100
# iterations<-parameters["iterations"]
# noise<-parameters["noise"]
# confidence<-parameters["confidence"]/100
parameters["mass_test_sens"]<-parameters["mass_test_sens"]/100
# age_testing_min<-(parameters["age_testing_min"])
# age_testing_max<-(parameters["age_testing_max"])
parameters["isolation_days"]<-parameters["isolation_days"]
parameters["propo2"]<-parameters["propo2"]/100
parameters["dexo2"]<-parameters["dexo2"]/100
parameters["dexo2c"]<-parameters["dexo2c"]/100
parameters["dexv"]<-parameters["dexv"]/100
parameters["dexvc"]<-parameters["dexvc"]/100
parameters["vent_dex"]<-parameters["vent_dex"]/100
parameters["prob_icu_v"]<-parameters["prob_icu_v"]/100
parameters["prob_icu_vr"]<-parameters["prob_icu_vr"]/100
parameters["prob_icu_r"]<-parameters["prob_icu_r"]/100
parameters["prob_v_v"]<-parameters["prob_v_v"]/100
parameters["prob_v_r"]<-parameters["prob_v_r"]/100
parameters["prob_v_vr"]<-parameters["prob_v_vr"]/100
parameters["pclin_v"]<-parameters["pclin_v"]/100
parameters["pclin_vr"]<-parameters["pclin_vr"]/100
parameters["pclin_r"]<-parameters["pclin_r"]/100
parameters["sigmaEV"]<-parameters["sigmaEV"]/100
parameters["sigmaER"]<-parameters["sigmaER"]/100
parameters["sigmaEVR"]<-parameters["sigmaEVR"]/100
parameters["sigmaR"]<-parameters["sigmaR"]/100
parameters["vac_dur"]<-1/parameters["vac_dur"]/100
parameters["vac_dur_r"]<-1/parameters["vac_dur_r"]/100
parameters["report_natdeathI"]<-parameters["report_natdeathI"]/100
parameters["report_natdeathCL"]<-parameters["report_natdeathCL"]/100
parameters["report_death_HC"]<-parameters["report_death_HC"]/100
parameters["reporth_ICU"]<-parameters["reporth_ICU"]/100
parameters["pre"]<-parameters["pre"]/100
parameters["pdeath_vent_hc"]<-parameters["pdeath_vent_hc"]/100
parameters["pdeath_icu_hc"]<-parameters["pdeath_icu_hc"]/100
parameters["pdeath_icu_hco"]<-parameters["pdeath_icu_hco"]/100
parameters["reporth_g"]<-parameters["reporth_g"]/100
parameters["seroneg"]<-(1/parameters["seroneg"])


# initial conditions for the main solution vector ----
initI<-0*popstruc[,2]  # Infected and symptomatic
initE<-0*popstruc[,2]  # Incubating
# initE[aci]<-1          # place random index case in E compartment
initE[aci]<-parameters["init"]     # place random index case in E compartment
initR<-parameters["pre"]*popstruc[,2]  # Immune
initX<-0*popstruc[,2]  # Isolated 
initV<-0*popstruc[,2]  # Vaccinated 
initQS<-0*popstruc[,2] # quarantined S 
initQE<-0*popstruc[,2] # quarantined E  
initQI<-0*popstruc[,2] # quarantined I  
initQR<-0*popstruc[,2] # quarantined R  
initH<-0*popstruc[,2]  # hospitalised 
initHC<-0*popstruc[,2] # hospital critical 
initC<-0*popstruc[,2]  # Cumulative cases (true)
initCM<-0*popstruc[,2] # Cumulative deaths (true)
initCL<-0*popstruc[,2] # symptomatic cases
initQC<-0*popstruc[,2] # quarantined C 
initICU<-0*popstruc[,2]   # icu
initICUC<-0*popstruc[,2]  # icu critical
initICUCV<-0*popstruc[,2] # icu critical
initVent<-0*popstruc[,2]  # icu vent
initVentC<-0*popstruc[,2] # icu vent crit
initCMC<-0*popstruc[,2]   # Cumulative deaths - overload (true)
initZ<-0*popstruc[,2]     # testing - quarantined (true)
initEV<-0*popstruc[,2]    # vaccinated exposed
initER<-0*popstruc[,2]    # recovered exposed
initEVR<-0*popstruc[,2]   # recovered and vaccinated exposed
initVR<-0*popstruc[,2]    # recovered and vaccinated
initQV<-0*popstruc[,2]    # quarantined and vaccinated
initQEV<-0*popstruc[,2]   # quarantined, exposed and vaccinated
initQEVR<-0*popstruc[,2]  # quarantined, exposed, recovered and vaccinated
initQER<-0*popstruc[,2]   # quarantined, exposed and recovered
initQVR<-0*popstruc[,2]   # quarantined, recovered and vaccinated
initHCICU<-0*popstruc[,2] # icu not seeking
initHCV<-0*popstruc[,2]   # ventilator not seeking
initAb<-0*popstruc[,2]   # ventilator not seeking

initS<-popstruc[,2]-initE-initI-initCL-initR-initX-initZ-initV-initH-initHC-initICU-initICUC-initICUCV-initVent-initVentC-
  initQS-initQE-initQI-initQR-initQC-initEV-initER-initEVR-initVR-initQV-initQEV-initQEVR-initQER-initQVR-
  initHCICU-initHCV # Susceptible (non-immune)


  # Define dataframe of interventions ----
inp <- bind_rows(interventions$baseline_mat %>% mutate(apply_to = "Baseline (Calibration)"),
                 interventions$future_mat %>% mutate(apply_to = "Hypothetical Scenario"))


Y<-c(initS,initE,initI,initR,initX,initH,initHC,initC,initCM,initV, initQS, initQE, initQI, initQR, initCL, initQC, initICU, 
     initICUC, initICUCV, initVent, initVentC, initCMC,initZ, initEV, initER, initEVR, initVR, 
     initQV,initQEV,initQEVR,initQER,initQVR,initHCICU,initHCV,initAb) # initial conditions for the main solution vector

# -------- model_repeat.R ---------

parameters["iterations"] <- 1

inp[["Target"]] <- 1:NROW(inp)

vectors <- inputs(inp, 'Baseline (Calibration)', times, startdate, stopdate)

# Temporary fix the issue where the app crashes if the vaccination efficacy is 100 
# by replacing 100 by 99.
# It should better to fix this in the model.
vectors$vc_vector[which(vectors$vc_vector == 100)] <- 99

check_parameters_list_for_na(parameters_list = parameters)

list_template <- list(
    Y = Y, 
    times = times, 
    startdate = startdate,
    stopdate = stopdate,
    parameters = parameters, 
    input = vectors, 
    A = A,  
    ihr = ihr, 
    ifr = ifr, 
    mort = mort, 
    popstruc = popstruc, 
    popbirth = popbirth, 
    ageing = ageing,
    contact_home = contact_home, 
    contact_school = contact_school, 
    contact_work = contact_work, 
    contact_other = contact_other, 
    age_group_vectors = interventions$baseline_age_groups, 
    cases_rv = cases_rv,
    interventions = interventions,
    use_cpp = USE_CPP
)

return(list_template)
}
