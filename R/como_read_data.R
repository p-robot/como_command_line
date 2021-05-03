#!/usr/bin/env Rscript
# 
# Mainly from here:
# app.R
# Live version: https://github.com/ocelhay/como/blob/master/inst/comoapp/app.R
# Static version: https://github.com/ocelhay/como/blob/1b61938191d9f63d512a3aaec9f5271a3ca0ed5a/inst/comoapp/app.R

# model_repeat.R
# Live version: https://github.com/ocelhay/como/blob/master/inst/comoapp/www/model/model_repeat.R
# Static version: https://github.com/ocelhay/como/blob/1b61938191d9f63d512a3aaec9f5271a3ca0ed5a/inst/comoapp/www/model/model_repeat.R


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


# Line 487 (app.R)

# # Create interventions tibble with input from UI ----
# interventions$baseline_mat <- tibble(
#   intervention = unlist(input[paste0("baseline_intervention_", 1:nb_interventions_max)]),
#   date_start = do.call("c", input[paste0("baseline_daterange_", 1:nb_interventions_max)])[seq(1, (2*nb_interventions_max - 1), by = 2)],
#   date_end = do.call("c", input[paste0("baseline_daterange_", 1:nb_interventions_max)])[seq(2, 2*nb_interventions_max, by = 2)],
#   value = unlist(input[paste0("baseline_coverage_", 1:nb_interventions_max)]),
#   age_group = unlist(map(input[paste0("baseline_age_group_", 1:nb_interventions_max)], 
#                          ~ paste(str_sub(.x, 1, 2), collapse = ","))),
#   Target = 1:nb_interventions_max) %>%
#   mutate(unit = case_when(intervention == "(*Self-isolation) Screening" ~ " contacts",
#                           intervention == "Mass Testing" ~ " thousands tests", 
#                           TRUE ~ "%")) %>%
#   filter(intervention != "_")


# # Fill list of age groups
# vec <- interventions$baseline_mat$age_group
# if(length(vec) > 0) {
#   for (i in 1:length(vec)) {
#     interventions$baseline_age_groups[[i]] <- parse_age_group(vec[i])
#   }
# }

# interventions$future_mat <- tibble(
#   intervention = unlist(input[paste0("future_intervention_", 1:nb_interventions_max)]),
#   date_start = do.call("c", input[paste0("future_daterange_", 1:nb_interventions_max)])[seq(1, (2*nb_interventions_max - 1), by = 2)],
#   date_end = do.call("c", input[paste0("future_daterange_", 1:nb_interventions_max)])[seq(2, 2*nb_interventions_max, by = 2)],
#   value = unlist(input[paste0("future_coverage_", 1:nb_interventions_max)]),
#   age_group = unlist(map(input[paste0("future_age_group_", 1:nb_interventions_max)], 
#                          ~ paste(str_sub(.x, 1, 2), collapse = ","))),
#   Target = 1:nb_interventions_max) %>%
#   mutate(unit = case_when(intervention == "(*Self-isolation) Screening" ~ " contacts",
#                           intervention == "Mass Testing" ~ " thousands tests", 
#                           TRUE ~ "%")) %>%
#   filter(intervention != "_")

# # Fill list of age groups
# vec <- interventions$future_mat$age_group
# if(length(vec) > 0) {
#   for (i in 1:length(vec)) {
#     interventions$future_age_groups[[i]] <- parse_age_group(vec[i])
#   }
# }

# # Validation of interventions ----
# validation_baseline <- fun_validation_interventions(dta = interventions$baseline_mat, 
#                                                     simul_start_date = input$date_range[1], 
#                                                     simul_end_date= input$date_range[2])
# interventions$valid_baseline_interventions <- validation_baseline$validation_interventions
# interventions$message_baseline_interventions <- validation_baseline$message_interventions

# validation_future <- fun_validation_interventions(dta = interventions$future_mat, 
#                                                   simul_start_date = input$date_range[1], 
#                                                   simul_end_date= input$date_range[2])
# interventions$valid_future_interventions <- validation_future$validation_interventions
# interventions$message_future_interventions <- validation_future$message_interventions


# Line 609 (app.R)
version <- read_excel(file_path, sheet = 1)
version_template <- names(version)[1]

# Line 625 (app.R)
# Epidemiology Sheet
dta <- read_excel(file_path, sheet = "Epidemiology")
names(dta) <- c("date", "cases", "deaths", "seroprevalence")

cases_rv$data <- dta %>%
  mutate(date = as.Date(date), cumulative_death = cumsum(deaths)) %>%
  as.data.frame()

# Severity/Mortality Sheet
dta <- read_excel(file_path, sheet = "Severity-Mortality") 
names(dta) <- c("age_category",	"ifr",	"ihr")

mort_sever_rv$data <- dta %>%
  mutate(ihr = ihr/100) %>%  # starting unit should be % - scaling to a value between 0 and 1
  mutate(ifr = ifr/max(ifr))  # starting unit should be % - scaling to a value between 0 and 1


# Population Sheet
dta <- read_excel(file_path, sheet = "Population")
names(dta) <- c("age_category",	"pop",	"birth",	"death")

population_rv$data <- dta %>%
    transmute(country = NA, age_category, pop, birth, death)


# Parameters Sheets
param <- bind_rows(read_excel(file_path, sheet = "Parameters"),
                   read_excel(file_path, sheet = "Country Area Param"),
                   read_excel(file_path, sheet = "Virus Param"),
                   read_excel(file_path, sheet = "Hospitalisation Param"),
                   read_excel(file_path, sheet = "Interventions Param")) %>%
  mutate(Value_Date = as.Date(Value_Date)) %>%
  drop_na(Parameter)


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
interventions_excel <- read_excel(file_path, sheet = "Interventions") %>%
  filter(!is.na(Intervention))
names(interventions_excel) <- c("intervention", "date_start", "date_end", "value", "unit", "age_group", "apply_to")


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
      interventions$baseline_age_groups[[i]] <- "1-21"
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
      input[[paste0("baseline_age_group_", i)]] = "1-21"
    }else{
      input[[paste0("baseline_age_group_", i)]] = vec_age_categories[parse_age_group(interventions_excel_baseline$age_group[i])]
    }
  }
}


# # Update interventions in the UI: future interventions
# interventions_excel_future <- interventions_excel %>% 
#   filter(apply_to == "Hypothetical Scenario")
# nb_interventions_future <- interventions_excel_future %>% nrow()
# if(nb_interventions_future > 0) {
  
#   for (i in 1:nb_interventions_future) {
#     input[[paste0("future_intervention_", i)]] = interventions_excel_future[[i, "intervention"]]
#     input[[paste0("future_daterange_", i)]] = c(
#                          start = interventions_excel_future[[i, "date_start"]], 
#                          end = interventions_excel_future[[i, "date_end"]])
#     input[[paste0("future_coverage_", i)]] = c(value = interventions_excel_future[[i, "value"]])
#     input[[paste0("future_age_group_", i)]] = vec_age_categories[parse_age_group(interventions_excel_future$age_group[i])]
#   }
# }


# Previous results are no longer valid
simul_interventions$results <- NULL


# Line 789 (app.R)
source("R/model_repeat.R")
parameters["iterations"] <- 1

inp[["Target"]] <- 1:NROW(inp)

vectors <- inputs(inp, 'Baseline (Calibration)', times, startdate, stopdate)

# Temporary fix the issue where the app crashes if the vaccination efficacy is 100 
# by replacing 100 by 99.
# It should better to fix this in the model.
vectors$vc_vector[which(vectors$vc_vector == 100)] <- 99

check_parameters_list_for_na(parameters_list = parameters)


# # Line 1 (model_repeat.R)
# # Definitions of several variables ----
# popstruc <- population_rv$data %>% 
#   select(age_category, pop) %>% 
#   rename(agefloor = age_category) %>% 
#   as.data.frame()

# popbirth <- population_rv$data %>% 
#   select(age_category, birth) %>% 
#   as.data.frame() # unit should be per person per day

# mort <- population_rv$data %>% 
#   pull(death) # unit should be per person per day

# ihr <- mort_sever_rv$data %>% 
#   select(age_category, ihr) %>% 
#   as.data.frame()

# ifr <- mort_sever_rv$data %>% 
#   select(age_category, ifr) %>% 
#   as.data.frame()


# #########    POP AGEING
# # per year ageing matrix
# A<-length(popstruc[,2])
# dd<-seq(1:A)/seq(1:A)
# ageing <- t(diff(diag(dd),lag=1)/(5*365.25))
# ageing<-cbind(ageing,0*seq(1:A)) # no ageing from last compartment

# #
# # pop<-population$country==country_name
# # pp<-population$pop[pop]
# ###  CONTACT MATRICES
# c_home <- contact_home[[country_name]] %>% as.matrix()
# c_school <- contact_school[[country_name]] %>% as.matrix()
# c_work <- contact_work[[country_name]] %>% as.matrix()
# c_other <- contact_other[[country_name]] %>% as.matrix()
# nce <-A-length(c_home[1,])

# contact_home<-matrix(0,nrow=A,ncol=A)
# contact_school<-matrix(0,nrow=A,ncol=A)
# contact_work<-matrix(0,nrow=A,ncol=A)
# contact_other<-matrix(0,nrow=A,ncol=A)

# for (i in 1:(A-nce)){
#   for (j in 1:(A-nce)){
#     contact_home[i,j]<-c_home[i,j]
#     contact_school[i,j]<-c_school[i,j]
#     contact_work[i,j]<-c_work[i,j]
#     contact_other[i,j]<-c_other[i,j]
#   }
# }

# for (i in (A+1-nce):A){
#   for (j in 1:(A-nce)){
#     contact_home[i,j]<-c_home[(A-nce),j]
#     contact_school[i,j]<-c_school[(A-nce),j]
#     contact_work[i,j]<-c_work[(A-nce),j]
#     contact_other[i,j]<-c_other[(A-nce),j]
#   }
# }
# for (i in 1:(A-nce)){
#   for (j in (A+1-nce):A){
#     contact_home[i,j]<-c_home[i,(A-nce)]
#     contact_school[i,j]<-c_school[i,(A-nce)]
#     contact_work[i,j]<-c_work[i,(A-nce)]
#     contact_other[i,j]<-c_other[i,(A-nce)]
#   }
# }
# for (i in (A+1-nce):A){
#   for (j in (A+1-nce):A){
#     contact_home[i,j]<-c_home[(A-nce),(A-nce)]
#     contact_school[i,j]<-c_school[(A-nce),(A-nce)]
#     contact_work[i,j]<-c_work[(A-nce),(A-nce)]
#     contact_other[i,j]<-c_other[(A-nce),(A-nce)]
#   }
# }



# #########   INITIALISE SIMULATION/INTERVENTION START TIMES
# startdate <- param$Value_Date[param$Parameter == "date_range_simul_start"]
# stopdate <- param$Value_Date[param$Parameter == "date_range_simul_end"]
# startdate <- startdate[1]
# stopdate <- stopdate[1]


# day_start <- as.numeric(startdate-startdate)
# day_stop <- as.numeric(stopdate-startdate)
# times <- seq(day_start, day_stop)

# tin<-as.numeric(startdate-as.Date("2020-01-01"))/365.25
# initP<-sum(popstruc[,2])       # population size 
# ageindcase<-20                 # age of index case (years)
# aci <- floor((ageindcase/5)+1) # age class of index case


# #############   DEFINE PARAMETERS
# parameters <- c(
  
#   ###  Transmission instrinsic
#   p = param$Value[param$Parameter=="p"][1],
#   rho = param$Value[param$Parameter=="rho"][1],
#   omega = param$Value[param$Parameter=="omega"][1],
#   gamma = param$Value[param$Parameter=="gamma"][1],
#   nui = param$Value[param$Parameter=="nui"][1],
#   report = param$Value[param$Parameter=="report"][1],
#   reportc = param$Value[param$Parameter=="reportc"][1],
#   reporth = param$Value[param$Parameter=="reporth"][1],
#   beds_available = param$Value[param$Parameter=="beds_available"][1],
#   icu_beds_available = param$Value[param$Parameter=="icu_beds_available"][1],
#   ventilators_available = param$Value[param$Parameter=="ventilators_available"][1],
#   give = 95,
#   pdeath_h = mean( param$Value[param$Parameter=="pdeath_h"],na.rm=T),
#   pdeath_ho = mean( param$Value[param$Parameter=="pdeath_ho"],na.rm=T),
#   pdeath_hc = mean( param$Value[param$Parameter=="pdeath_hc"],na.rm=T),
#   pdeath_hco = mean( param$Value[param$Parameter=="pdeath_hco"],na.rm=T),
#   pdeath_icu = mean( param$Value[param$Parameter=="pdeath_icu"],na.rm=T),
#   pdeath_icuo = mean( param$Value[param$Parameter=="pdeath_icuo"],na.rm=T),
#   pdeath_icuc = mean( param$Value[param$Parameter=="pdeath_icuc"],na.rm=T),
#   pdeath_icuco = mean( param$Value[param$Parameter=="pdeath_icuco"],na.rm=T),
#   pdeath_vent = mean( param$Value[param$Parameter=="pdeath_vent"],na.rm=T),
#   pdeath_ventc = mean( param$Value[param$Parameter=="pdeath_ventc"],na.rm=T),
#   ihr_scaling = param$Value[param$Parameter=="ihr_scaling"][1],
#   nus = param$Value[param$Parameter=="nus"][1],
#   nusc = param$Value[param$Parameter=="nus"][1], # nusc = nus
#   nu_icu = param$Value[param$Parameter=="nu_icu"][1],
#   nu_icuc = param$Value[param$Parameter=="nu_icu"][1],  # nu_icuc = nu_icu
#   nu_vent = param$Value[param$Parameter=="nu_vent"][1],
#   nu_ventc = param$Value[param$Parameter=="nu_vent"][1], # nu_ventc = nu_vent
#   rhos = param$Value[param$Parameter=="rhos"][1],
#   amp = param$Value[param$Parameter=="amp"][1],
#   phi = param$Value[param$Parameter=="phi"][1],
#   pclin = param$Value[param$Parameter=="pclin"][1],
#   prob_icu = param$Value[param$Parameter=="prob_icu"][1],
#   prob_vent = param$Value[param$Parameter=="prob_vent"][1],
#   propo2 = param$Value[param$Parameter=="propo2"][1],
#   dexo2 = mean( param$Value[param$Parameter=="dexo2"],na.rm=T),
#   dexo2c = mean( param$Value[param$Parameter=="dexo2c"],na.rm=T),
#   dexv = mean( param$Value[param$Parameter=="dexvc"],na.rm=T),
#   dexvc = mean( param$Value[param$Parameter=="dexvc"],na.rm=T),
#   vent_dex = mean(param$Value[param$Parameter=="vent_dex"],na.rm=T),
#   prob_icu_v = mean(param$Value[param$Parameter=="prob_icu_v"],na.rm=T),
#   prob_icu_vr = mean(param$Value[param$Parameter=="prob_icu_vr"],na.rm=T),
#   prob_icu_r = mean(param$Value[param$Parameter=="prob_icu_r"],na.rm=T),
#   prob_v_v = mean(param$Value[param$Parameter=="prob_v_v"],na.rm=T),
#   prob_v_vr = mean(param$Value[param$Parameter=="prob_v_vr"],na.rm=T),
#   prob_v_r = mean(param$Value[param$Parameter=="prob_v_r"],na.rm=T),
#   pclin_v = mean(param$Value[param$Parameter=="pclin_v"],na.rm=T),
#   pclin_vr = mean(param$Value[param$Parameter=="pclin_vr"],na.rm=T),
#   pclin_r = mean(param$Value[param$Parameter=="pclin_r"],na.rm=T),
#   sigmaEV = mean(param$Value[param$Parameter=="sigmaEV"],na.rm=T),
#   sigmaEVR = mean(param$Value[param$Parameter=="sigmaEVR"],na.rm=T),
#   sigmaER = mean(param$Value[param$Parameter=="sigmaER"],na.rm=T),
#   sigmaR = mean(param$Value[param$Parameter=="sigmaR"],na.rm=T),
#   vac_dur = mean(param$Value[param$Parameter=="vac_dur"],na.rm=T),
#   vac_dur_r = mean(param$Value[param$Parameter=="vac_dur_r"],na.rm=T),
#   report_natdeathI = mean(param$Value[param$Parameter=="report_natdeathI"],na.rm=T),
#   report_natdeathCL = mean(param$Value[param$Parameter=="report_natdeathCL"],na.rm=T),
#   pre = mean(param$Value[param$Parameter=="pre"],na.rm=T),
#   report_v = param$Value[param$Parameter=="report_v"][1],
#   report_cv = param$Value[param$Parameter=="report_cv"][1],
#   report_vr = param$Value[param$Parameter=="report_vr"][1],
#   report_cvr = param$Value[param$Parameter=="report_cvr"][1],
#   report_r = param$Value[param$Parameter=="report_r"][1],
#   report_cr = param$Value[param$Parameter=="report_cr"][1],
#   reporth_ICU = param$Value[param$Parameter=="reporth_ICU"][1],
#   report_death_HC = param$Value[param$Parameter=="report_death_HC"][1],
#   pdeath_vent_hc = mean( param$Value[param$Parameter=="pdeath_vent_hc"],na.rm=T),
#   pdeath_icu_hc = mean( param$Value[param$Parameter=="pdeath_icu_hc"],na.rm=T),
#   pdeath_icu_hco = mean( param$Value[param$Parameter=="pdeath_icu_hco"],na.rm=T),
#   reporth_g = param$Value[param$Parameter=="reporth_g"][1],
#   seroneg = param$Value[param$Parameter=="seroneg"][1],
#   sample_size = param$Value[param$Parameter=="sample_size"][1],
  
#   ###  INTERVENTIONS
#   # self isolation
#   selfis_eff = mean(param$Value[param$Parameter=="selfis_eff"],na.rm=T),
#   # social distancing
#   dist_eff = mean(param$Value[param$Parameter=="dist_eff"],na.rm=T),
#   # hand washing
#   hand_eff = mean(param$Value[param$Parameter=="hand_eff"],na.rm=T),
#   # mask wearing
#   mask_eff = mean(param$Value[param$Parameter=="mask_eff"],na.rm=T),
#   # working at home
#   work_eff = mean(param$Value[param$Parameter=="work_eff"],na.rm=T),
#   w2h = mean(param$Value[param$Parameter=="w2h"],na.rm=T),
#   # school closures
#   # school_eff = mean(param$Value[param$Parameter=="school_eff"],na.rm=T),
#   s2h = mean(param$Value[param$Parameter=="s2h"],na.rm=T),
#   # cocooning the elderly
#   cocoon_eff = mean(param$Value[param$Parameter=="cocoon_eff"],na.rm=T),
#   age_cocoon = mean(param$Value[param$Parameter=="age_cocoon"],na.rm=T),
#   # vaccination campaign
#   # vaccine_on = as.numeric(param$Value_Date[param$Parameter=="date_vaccine_on"] - startdate),
#   vaccine_eff = mean(param$Value[param$Parameter=="vaccine_eff"],na.rm=T),
#   vaccine_eff_r = mean(param$Value[param$Parameter=="vaccine_eff_r"],na.rm=T),
#   # age_vaccine_min = mean(param$Value[param$Parameter=="age_vaccine_min"],na.rm=T),
#   # age_vaccine_max = mean(param$Value[param$Parameter=="age_vaccine_max"],na.rm=T),
#   # vaccine_cov = param$Value[param$Parameter=="vaccine_cov"],
#   vac_campaign = mean(param$Value[param$Parameter=="vac_campaign"],na.rm=T),
#   # travel ban
#   mean_imports = mean(param$Value[param$Parameter=="mean_imports"],na.rm=T),
#   # screening
#   screen_test_sens = mean(param$Value[param$Parameter=="screen_test_sens"],na.rm=T),
#   # screen_contacts = mean(param$Value[param$Parameter=="screen_contacts"],na.rm=T),
#   screen_overdispersion = mean(param$Value[param$Parameter=="screen_overdispersion"],na.rm=T),
#   # voluntary home quarantine
#   quarantine_days = mean(param$Value[param$Parameter=="quarantine_days"],na.rm=T),
#   quarantine_effort = mean(param$Value[param$Parameter=="quarantine_effort"],na.rm=T),
#   quarantine_eff_home = mean(param$Value[param$Parameter=="quarantine_eff_home"],na.rm=T),
#   quarantine_eff_other = mean(param$Value[param$Parameter=="quarantine_eff_other"],na.rm=T),
#   # mass testing
#   # age_testing_min = mean(param$Value[param$Parameter=="age_testing_min"],na.rm=T),
#   # age_testing_max = mean(param$Value[param$Parameter=="age_testing_max"],na.rm=T),
#   mass_test_sens = mean(param$Value[param$Parameter=="mass_test_sens"],na.rm=T),
#   isolation_days = mean(param$Value[param$Parameter=="isolation_days"],na.rm=T),
  
#   ###  Initialisation
#   init = param$Value[param$Parameter=="init"][1],
  
#   ### Others
#   household_size = param$Value[param$Parameter=="household_size"][1],
#   noise = param$Value[param$Parameter=="noise"][1],
#   iterations = param$Value[param$Parameter=="iterations"][1],
#   confidence = param$Value[param$Parameter=="confidence"][1]
# )
# ihr[,2]<- parameters["ihr_scaling"]*ihr[,2]   
# parameters["ifr_correction_young"]<-1
# parameters["ifr_correction_old"]<-1
# # ifr[1:12,2]<-ifr[1:12,2]/ifr_correction_young
# # ihr$ihr[15:21]<-ihr$ihr[15:21]*ifr_correction_old

# # Scale parameters to percentages/ rates
# parameters["rho"]<-parameters["rho"]/100
# parameters["omega"]<-(1/(parameters["omega"]*365))
# parameters["gamma"]<-1/parameters["gamma"]
# parameters["nui"]<-1/parameters["nui"]
# parameters["report"]<-parameters["report"]/100
# parameters["reportc"]<-parameters["reportc"]/100
# parameters["report_v"]<-parameters["report_v"]/100
# parameters["report_cv"]<-parameters["report_cv"]/100
# parameters["report_vr"]<-parameters["report_vr"]/100
# parameters["report_cvr"]<-parameters["report_cvr"]/100
# parameters["report_r"]<-parameters["report_r"]/100
# parameters["report_cr"]<-parameters["report_cr"]/100
# parameters["reporth"]<-parameters["reporth"]/100
# parameters["nus"]<-1/parameters["nus"]
# parameters["rhos"]<-parameters["rhos"]/100
# parameters["amp"]<-parameters["amp"]/100
# parameters["selfis_eff"]<-parameters["selfis_eff"]/100
# parameters["dist_eff"]<-parameters["dist_eff"]/100
# parameters["hand_eff"]<-parameters["hand_eff"]/100
# parameters["mask_eff"]<-parameters["mask_eff"]/100
# parameters["work_eff"]<-parameters["work_eff"]/100
# parameters["w2h"]<-parameters["w2h"]/100
# # parameters["school_eff"]<-parameters["school_eff"]/100
# parameters["s2h"]<-parameters["s2h"]/100
# parameters["cocoon_eff"]<-parameters["cocoon_eff"]/100
# parameters["age_cocoon"]<-floor((parameters["age_cocoon"]/5)+1)
# parameters["vaccine_eff"]<-parameters["vaccine_eff"]/100
# parameters["vaccine_eff_r"]<-parameters["vaccine_eff_r"]/100
# # age_vaccine_min<-(parameters["age_vaccine_min"])
# # age_vaccine_max<-(parameters["age_vaccine_max"])
# # parameters["vaccine_cov"]<-parameters["vaccine_cov"]/100
# # parameters["vac_campaign"]<-parameters["vac_campaign"]*7
# parameters["screen_test_sens"]<-parameters["screen_test_sens"]/100
# parameters["quarantine_days"]<-parameters["quarantine_days"]
# parameters["quarantine_effort"]<-1/parameters["quarantine_effort"]
# parameters["quarantine_eff_home"]<-parameters["quarantine_eff_home"]/-100
# parameters["quarantine_eff_other"]<-parameters["quarantine_eff_other"]/100
# parameters["give"]<-parameters["give"]/100
# parameters["pdeath_h"]<-parameters["pdeath_h"]/100
# parameters["pdeath_ho"]<-parameters["pdeath_ho"]/100
# parameters["pdeath_hc"]<-parameters["pdeath_hc"]/100
# parameters["pdeath_hco"]<-parameters["pdeath_hco"]/100
# parameters["pdeath_icu"]<-parameters["pdeath_icu"]/100
# parameters["pdeath_icuo"]<-parameters["pdeath_icuo"]/100
# parameters["pdeath_icuc"]<-parameters["pdeath_icuc"]/100
# parameters["pdeath_icuco"]<-parameters["pdeath_icuco"]/100
# parameters["pdeath_vent"]<-parameters["pdeath_vent"]/100
# parameters["pdeath_ventc"]<-parameters["pdeath_ventc"]/100
# parameters["nusc"]<-1/parameters["nusc"]
# parameters["nu_icu"]<-1/parameters["nu_icu"]
# parameters["nu_icuc"]<-1/parameters["nu_icuc"]
# parameters["nu_vent"]<-1/parameters["nu_vent"]
# parameters["nu_ventc"]<-1/parameters["nu_ventc"]
# parameters["pclin"]<-parameters["pclin"]/100
# parameters["prob_icu"]<-parameters["prob_icu"]/100
# parameters["prob_vent"]<-parameters["prob_vent"]/100
# iterations<-parameters["iterations"]
# noise<-parameters["noise"]
# confidence<-parameters["confidence"]/100
# parameters["mass_test_sens"]<-parameters["mass_test_sens"]/100
# # age_testing_min<-(parameters["age_testing_min"])
# # age_testing_max<-(parameters["age_testing_max"])
# parameters["isolation_days"]<-parameters["isolation_days"]
# parameters["propo2"]<-parameters["propo2"]/100
# parameters["dexo2"]<-parameters["dexo2"]/100
# parameters["dexo2c"]<-parameters["dexo2c"]/100
# parameters["dexv"]<-parameters["dexv"]/100
# parameters["dexvc"]<-parameters["dexvc"]/100
# parameters["vent_dex"]<-parameters["vent_dex"]/100
# parameters["prob_icu_v"]<-parameters["prob_icu_v"]/100
# parameters["prob_icu_vr"]<-parameters["prob_icu_vr"]/100
# parameters["prob_icu_r"]<-parameters["prob_icu_r"]/100
# parameters["prob_v_v"]<-parameters["prob_v_v"]/100
# parameters["prob_v_r"]<-parameters["prob_v_r"]/100
# parameters["prob_v_vr"]<-parameters["prob_v_vr"]/100
# parameters["pclin_v"]<-parameters["pclin_v"]/100
# parameters["pclin_vr"]<-parameters["pclin_vr"]/100
# parameters["pclin_r"]<-parameters["pclin_r"]/100
# parameters["sigmaEV"]<-parameters["sigmaEV"]/100
# parameters["sigmaER"]<-parameters["sigmaER"]/100
# parameters["sigmaEVR"]<-parameters["sigmaEVR"]/100
# parameters["sigmaR"]<-parameters["sigmaR"]/100
# parameters["vac_dur"]<-1/parameters["vac_dur"]/100
# parameters["vac_dur_r"]<-1/parameters["vac_dur_r"]/100
# parameters["report_natdeathI"]<-parameters["report_natdeathI"]/100
# parameters["report_natdeathCL"]<-parameters["report_natdeathCL"]/100
# parameters["report_death_HC"]<-parameters["report_death_HC"]/100
# parameters["reporth_ICU"]<-parameters["reporth_ICU"]/100
# parameters["pre"]<-parameters["pre"]/100
# parameters["pdeath_vent_hc"]<-parameters["pdeath_vent_hc"]/100
# parameters["pdeath_icu_hc"]<-parameters["pdeath_icu_hc"]/100
# parameters["pdeath_icu_hco"]<-parameters["pdeath_icu_hco"]/100
# parameters["reporth_g"]<-parameters["reporth_g"]/100
# parameters["seroneg"]<-(1/parameters["seroneg"])


# parameters_noise <- c("p", "rho", "omega", "gamma", "nui", "ihr_scaling","nus", "nu_icu","nu_vent",
#                       "rhos", "selfis_eff", "dist_eff", "hand_eff", "mask_eff", "work_eff", 
#                       "w2h", "s2h", "cocoon_eff", "mean_imports", "screen_overdispersion", 
#                       "quarantine_effort", "quarantine_eff_home", "quarantine_eff_other")

# # parameters_fit <- c("p", "ihr_scaling","ifr_correction_young","ifr_correction_old","init")
# # parameters_fit <- rownames(fit_mat)
# ###########################################################################
# # Define the indices for each variable
# Sindex<-1:A
# Eindex<-(A+1):(2*A)
# Iindex<-(2*A+1):(3*A)
# Rindex<-(3*A+1):(4*A)
# Xindex<-(4*A+1):(5*A)
# Hindex<-(5*A+1):(6*A)
# HCindex<-(6*A+1):(7*A)
# Cindex<-(7*A+1):(8*A)
# CMindex<-(8*A+1):(9*A)
# Vindex<-(9*A+1):(10*A)
# QSindex<-(10*A+1):(11*A)
# QEindex<-(11*A+1):(12*A)
# QIindex<-(12*A+1):(13*A)
# QRindex<-(13*A+1):(14*A)
# CLindex<-(14*A+1):(15*A)
# QCindex<-(15*A+1):(16*A)
# ICUindex<-(16*A+1):(17*A)
# ICUCindex<-(17*A+1):(18*A)
# ICUCVindex<-(18*A+1):(19*A)
# Ventindex<-(19*A+1):(20*A)
# VentCindex<-(20*A+1):(21*A)
# CMCindex<-(21*A+1):(22*A)
# Zindex<-(22*A+1):(23*A)
# EVindex<-(23*A+1):(24*A)
# ERindex<-(24*A+1):(25*A)
# EVRindex<-(25*A+1):(26*A)
# VRindex<-(26*A+1):(27*A)
# QVindex<-(27*A+1):(28*A)
# QEVindex<-(28*A+1):(29*A)
# QEVRindex<-(29*A+1):(30*A)
# QERindex<-(30*A+1):(31*A)
# QVRindex<-(31*A+1):(32*A)
# HCICUindex<-(32*A+1):(33*A)
# HCVindex<-(33*A+1):(34*A)
# Abindex<-(34*A+1):(35*A)

# ###########################################################################
# # MODEL INITIAL CONDITIONS
# initI<-0*popstruc[,2]  # Infected and symptomatic
# initE<-0*popstruc[,2]  # Incubating
# # initE[aci]<-1          # place random index case in E compartment
# initE[aci]<-parameters["init"]     # place random index case in E compartment
# initR<-parameters["pre"]*popstruc[,2]  # Immune
# initX<-0*popstruc[,2]  # Isolated 
# initV<-0*popstruc[,2]  # Vaccinated 
# initQS<-0*popstruc[,2] # quarantined S 
# initQE<-0*popstruc[,2] # quarantined E  
# initQI<-0*popstruc[,2] # quarantined I  
# initQR<-0*popstruc[,2] # quarantined R  
# initH<-0*popstruc[,2]  # hospitalised 
# initHC<-0*popstruc[,2] # hospital critical 
# initC<-0*popstruc[,2]  # Cumulative cases (true)
# initCM<-0*popstruc[,2] # Cumulative deaths (true)
# initCL<-0*popstruc[,2] # symptomatic cases
# initQC<-0*popstruc[,2] # quarantined C 
# initICU<-0*popstruc[,2]   # icu
# initICUC<-0*popstruc[,2]  # icu critical
# initICUCV<-0*popstruc[,2] # icu critical
# initVent<-0*popstruc[,2]  # icu vent
# initVentC<-0*popstruc[,2] # icu vent crit
# initCMC<-0*popstruc[,2]   # Cumulative deaths - overload (true)
# initZ<-0*popstruc[,2]     # testing - quarantined (true)
# initEV<-0*popstruc[,2]    # vaccinated exposed
# initER<-0*popstruc[,2]    # recovered exposed
# initEVR<-0*popstruc[,2]   # recovered and vaccinated exposed
# initVR<-0*popstruc[,2]    # recovered and vaccinated
# initQV<-0*popstruc[,2]    # quarantined and vaccinated
# initQEV<-0*popstruc[,2]   # quarantined, exposed and vaccinated
# initQEVR<-0*popstruc[,2]  # quarantined, exposed, recovered and vaccinated
# initQER<-0*popstruc[,2]   # quarantined, exposed and recovered
# initQVR<-0*popstruc[,2]   # quarantined, recovered and vaccinated
# initHCICU<-0*popstruc[,2] # icu not seeking
# initHCV<-0*popstruc[,2]   # ventilator not seeking
# initAb<-0*popstruc[,2]   # ventilator not seeking

# initS<-popstruc[,2]-initE-initI-initCL-initR-initX-initZ-initV-initH-initHC-initICU-initICUC-initICUCV-initVent-initVentC-
#   initQS-initQE-initQI-initQR-initQC-initEV-initER-initEVR-initVR-initQV-initQEV-initQEVR-initQER-initQVR-
#   initHCICU-initHCV # Susceptible (non-immune)


# inp <- read_excel(file_path, sheet = "Interventions") %>%
#   filter(! is.na(Intervention))
# # Test if listed interventions are valid
# valid_interventions_v17 <- c("Dexamethasone", "Handwashing", "International Travel Ban",
#                              "Mask Wearing", "Mass Testing", "School Closures",
#                              "Partial School Closures",
#                              "Self-isolation if Symptomatic",
#                              "(*Self-isolation) Household Isolation", "(*Self-isolation) Screening", "Shielding the Elderly",
#                              "Social Distancing", "Vaccination", "Working at Home")
# if(all(inp$Intervention %in% valid_interventions_v17)) message("Okay, all interventions are valid.")
# if(!all(inp$Intervention %in% valid_interventions_v17)) stop("Stop, some interventions are not valid.")
# # complte the age_groups column
# inp$`Age Groups`[is.na(inp$`Age Groups`)] <- "1-21"
# vec<- inp$`Age Groups`



# inp$Target<-rep(0,length(inp$Value))
# age_group_vectors <- list()
# for (i in 1:length(vec)){
#   pp<-parse_age_group(vec[i])
#   age_group_vectors[[i]] <- pp
#   inp$Target[i]<-i
# }


# vectors0<-inputs(inp,'Baseline (Calibration)')
# vectors<-inputs(inp,'Hypothetical Scenario')


# f <- c(1,(1+parameters["give"])/2,(1-parameters["give"])/2,0)
# KH<-parameters["beds_available"]
# KICU<- parameters["icu_beds_available"]+parameters["ventilators_available"]
# Kvent<- parameters["ventilators_available"]
# x.H <- c(0,(1+parameters["give"])*KH/2,(3-parameters["give"])*KH/2,2*KH)
# x.ICU <- c(0,(1+parameters["give"])*KICU/2,(3-parameters["give"])*KICU/2,2*KICU)
# x.Vent <- c(0,(1+parameters["give"])*Kvent/2,(3-parameters["give"])*Kvent/2,2*Kvent)
# fH <- splinefun(x.H, f, method = "hyman")
# fICU <- splinefun(x.ICU, f, method = "hyman")
# fVent<- splinefun(x.Vent, f, method = "hyman")


# Y<-c(initS,initE,initI,initR,initX,initH,initHC,initC,initCM,initV, initQS, initQE, initQI, initQR, initCL, initQC, initICU, 
#      initICUC, initICUCV, initVent, initVentC, initCMC,initZ, initEV, initER, initEVR, initVR, 
#      initQV,initQEV,initQEVR,initQER,initQVR,initHCICU,initHCV,initAb) # initial conditions for the main solution vector

