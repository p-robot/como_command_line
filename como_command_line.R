#!/usr/bin/env Rscript

VERBOSE <- TRUE
USE_CPP <- FALSE

##########
# PREAMBLE
# --------
if(VERBOSE){cat("Loading packages and functions\n")}
source("como_preamble.R")
source("model_once.R")
source("como_functions.R")

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
# LOAD non-CPP function
# -----------------

if(!USE_CPP){
  if(VERBOSE){cat("Loading functions for deSolve\n")}
  source("como_read_data.R")
  source("fun_covid.R")
}

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
print(paste0("Runtime: ", end_time - start_time))

if(VERBOSE){cat("Process model outputs\n")}

# Write model outputs to file
write.csv(dta, output_file, row.names = FALSE)
