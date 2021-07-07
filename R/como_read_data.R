#!/usr/bin/env Rscript
# 
# Mainly from here:
# app.R
# Live version: https://github.com/ocelhay/como/blob/master/inst/comoapp/app.R
# Static version: https://github.com/ocelhay/como/blob/1b61938191d9f63d512a3aaec9f5271a3ca0ed5a/inst/comoapp/app.R

# model_repeat.R
# Live version: https://github.com/ocelhay/como/blob/master/inst/comoapp/www/model/model_repeat.R
# Static version: https://github.com/ocelhay/como/blob/1b61938191d9f63d512a3aaec9f5271a3ca0ed5a/inst/comoapp/www/model/model_repeat.R

read_data <- function(file_path, country_name){

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


# Extra Parameters sheet (if exists)
if( "Extra Param" %in% excel_sheets(file_path) ){
  cat("Reading Extra Param sheet\n")
  extra_param <- read_excel(file_path, sheet = "Extra Param") %>%
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

}
