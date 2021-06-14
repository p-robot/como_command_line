#!/usr/bin/env Rscript

VERBOSE <- TRUE
USE_CPP <- FALSE

###################
# COMMAND-LINE ARGS
# -----------------

if(VERBOSE){cat("Read command-line args\n")}
args <- commandArgs(trailingOnly = TRUE)

# Parse command-line arguments
country_name <- args[1]
file_path <- args[2]
output_file_stub <- args[3]

###################
# RUN THE MODEL
# -----------------

if(VERBOSE){cat("Running the model\n")}

for(ihr_scaling in seq(2.2, 2.8, 0.1)){
for(reportc in seq(0.3, 0.4, 0.05) ){
for(p in seq(0.0279, 0.0279, 0.0001) ){
    
    if(VERBOSE){cat("Loading packages and functions\n")}
    source("R/como_preamble.R")
    source("R/model_once.R")
    source("R/como_functions.R")
    source("R/fun_covid.R") # solution function for deSolve (no CPP)
    
    print("---------------")
    print(paste0("ihr_scaling: ", ihr_scaling))
    print(paste0("reportc: ", reportc))
    print(paste0("p: ", p))
    
    # Load CoMo template
    list_template <- load_template(file_path, country_name, USE_CPP)
    list_template$parameters["p"] <- p
    list_template$parameters["reportc"] <- reportc
    list_template$parameters["ihr_scaling"] <- ihr_scaling
    
    # Run the model
    list_output <- run_model(list_template)

    # Process model outputs
    dta <- process_outputs(list_output, list_template)

    if(VERBOSE){cat("Processing model outputs\n")}
    dta$p <- p
    dta$reportc <- reportc
    dta$ihr_scaling <- ihr_scaling
    
    # Write model outputs to file
    write.csv(dta, paste0(output_file_stub, p, "_reportc", reportc, "_ihr_scaling", ihr_scaling, ".csv"), row.names = FALSE)
}
}
}
