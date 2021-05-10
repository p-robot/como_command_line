#!/usr/bin/env Rscript

VERBOSE <- TRUE
USE_CPP <- FALSE

###################
# RUN THE MODEL
# -----------------

if(VERBOSE){cat("Running the model\n")}

output_file_stub <- "output-JOR_p"

for(reportc in seq(0.3, 0.4, 0.01) ){
for(p in seq(0.03, 0.05, 0.001) ){
    
    if(VERBOSE){cat("Loading packages and functions\n")}
    source("R/como_preamble.R")
    source("R/model_once.R")
    source("R/como_functions.R")
    source("R/fun_covid.R") # solution function for deSolve (no CPP)
    
    print("---------------")
    print(paste0("reportc: ", reportc))
    print(paste0("p: ", p))
    
    # Load CoMo template
    list_template <- load_template(file_path, country_name, USE_CPP)
    list_template$parameters["p"] <- p
    list_template$parameters["reportc"] <- reportc
    
    # Run the model
    list_output <- run_model(list_template)

    # Process model outputs
    dta <- process_outputs(list_output, list_template)

    if(VERBOSE){cat("Processing model outputs\n")}
    dta$p <- p
    dta$reportc <- reportc
    
    # Write model outputs to file
    write.csv(dta, paste0(output_file_stub, p, "_reportc", reportc, ".csv"), row.names = FALSE)
}
}
