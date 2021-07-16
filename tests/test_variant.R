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
source("R/como_read_data.R")
source("R/como_functions.R")
source("R/fun_covid.R") # solution function for deSolve (no CPP)

###################
# ARGUMENTS
# -----------------

# Parse command-line arguments
country_name <- "United Kingdom of Great Britain"
file_path <- "tests/data/Template_CoMoCOVID-19App_v17_p0.0245_variant.xlsx"

#########################
# RUN THE MODEL USING CPP
# -----------------------
cat("Checking CPP and non-CPP output from the template:", file_path, "\n")
list_template_cpp <- load_template(file_path, country_name, USE_CPP = TRUE)
list_output_cpp <- run_model(list_template_cpp)
df_cli_cpp <- process_outputs(list_output_cpp, list_template_cpp)
#write.csv(df_cli_cpp, "cpp.csv")

############################
# RUN THE MODEL WITHOUT  CPP
# --------------------------
list_template_noncpp <- load_template(file_path, country_name, USE_CPP = FALSE)
list_output_noncpp <- run_model(list_template_noncpp)
df_cli_noncpp <- process_outputs(list_output_noncpp, list_template_noncpp)
#write.csv(df_cli_noncpp, "noncpp.csv")


# List of columns to check
cols2check <- names(df_cli_cpp)[grepl("baseline", names(df_cli_cpp))]

for(col in cols2check){
  expect_equal(df_cli_cpp[[col]], df_cli_noncpp[[col]])
}
print("All baseline CPP and non-CPP output passed")

