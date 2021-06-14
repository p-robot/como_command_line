#!/usr/bin/env Rscript
# 
# Script to test that the command-line version of the model can read
# over 100 interventions in the template (the Shiny version is capped
# at 100 interventions).
# 
# Test file "tests/data/Template_CoMoCOVID-19App_v17_100interventions.xlsx"
# has 100 1-day handwashing interventions included with 0% uptake and the 
# usual interventions for this template are included after 100 so this 
# should give the same output as the app without these interventions added.

##########
# PREAMBLE
# --------
require(testthat)
source("R/como_preamble.R")
source("R/model_once.R")
source("R/como_functions.R")
source("R/fun_covid.R") # solution function for deSolve (no CPP)

###################
# ARGUMENTS
# -----------------

# Parse command-line arguments
country_name <- "United Kingdom of Great Britain"
file_path <- "tests/data/Template_CoMoCOVID-19App_v17_100interventions.xlsx"
app_file <- "tests/data/COVID19_App_Data_Template_CoMoCOVID-19App_v17.csv"

df_app <- read.csv(app_file)

# List of columns to check
cols2check <- names(df_app)[grepl("baseline", names(df_app))]

###########################
# RUN THE MODEL WITH CPP
# -------------------------
source("R/como_preamble.R")
source("R/model_once.R")
source("R/como_functions.R")
source("R/fun_covid.R") # solution function for deSolve (no CPP)

print("Checking output of 100 interventions")
USE_CPP <- TRUE
list_template <- load_template(file_path, country_name, USE_CPP)
list_output <- run_model(list_template)
df_cli <- process_outputs(list_output, list_template)

for(col in cols2check){
  expect_equal(df_app[[col]], df_cli[[col]])
}
print("All baseline output passed")
