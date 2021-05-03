#!/usr/bin/env Rscript
# 
# Script to test the output (all columns containing "baseline")
# of the CoMo app against reproduced outputs without using the Shiny 
# interface.  

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
file_path <- "tests/data/Template_CoMoCOVID-19App_v17.xlsx"
app_file <- "tests/data/COVID19_App_Data_Template_CoMoCOVID-19App_v17.csv"

df_app <- read.csv(app_file)

# List of columns to check
cols2check <- names(df_app)[grepl("baseline", names(df_app))]

#########################
# RUN THE MODEL USING CPP
# -----------------------
print("Checking CPP output")
USE_CPP <- TRUE
list_template <- load_template(file_path, country_name, USE_CPP)
list_output <- run_model(list_template)
df_cli_cpp <- process_outputs(list_output, list_template)

for(col in cols2check){
  expect_equal(df_app[[col]], df_cli_cpp[[col]])
}
print("All baseline CPP output passed")

###########################
# RUN THE MODEL WITHOUT CPP
# -------------------------
source("R/como_preamble.R")
source("R/model_once.R")
source("R/como_functions.R")
source("R/fun_covid.R") # solution function for deSolve (no CPP)

print("Checking non-CPP output")
USE_CPP <- FALSE
list_template <- load_template(file_path, country_name, USE_CPP)
list_output <- run_model(list_template)
df_cli_nocpp <- process_outputs(list_output, list_template)

for(col in cols2check){
  expect_equal(df_app[[col]], df_cli_nocpp[[col]])
}
print("All baseline non-CPP output passed")
