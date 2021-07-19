#!/usr/bin/env Rscript

# Print warnings
options(warn = 1)

VERBOSE <- TRUE
USE_CPP <- TRUE

##########
# PREAMBLE
# --------
if(VERBOSE){cat("Loading packages and functions\n")}
source("R/como_preamble.R")
source("R/model_once.R")
source("R/como_functions.R")
source("R/como_read_data.R")
source("R/fun_covid.R") # solution function for deSolve (no CPP)

###################
# COMMAND-LINE ARGS
# -----------------

if(VERBOSE){cat("Read command-line args\n")}
args <- commandArgs(trailingOnly = TRUE)

# Parse command-line arguments
country_name <- args[1]
file_path <- args[2]
output_file <- args[3]

###################
# RUN THE MODEL
# -----------------

start_time <- Sys.time()
if(VERBOSE){cat("Running the model\n")}

# Load CoMo template
list_template <- load_template(file_path, country_name, USE_CPP)

# Run the model
list_output <- run_model(list_template)

# Process model outputs
dta <- process_outputs(list_output, list_template)

end_time <- Sys.time()
print(paste0("Runtime: ", round(end_time - start_time, 2)))

if(VERBOSE){cat("Processing model outputs\n")}

# Write model outputs to file
write.csv(dta, output_file, row.names = FALSE)
