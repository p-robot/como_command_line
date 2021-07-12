#!/usr/bin/env Rscript
# 
# R script to generate a random uniform sample from parameter space
# 
# Author: W. Probert
# Created: June 2021

if( !interactive() ){
	args <- commandArgs(trailingOnly = TRUE)
	seed <- args[1]
	parameter_file <- args[2]
	n_samples <- as.integer(args[3])
	output_csv <- args[4]
	
	# Set the seed for the random number generator
	set.seed(seed)

	# Read the CSV file of parameters to adjust
	df <- read.csv(parameter_file, stringsAsFactors = FALSE)

date_params <- df[df$class == "date",]
numeric_params <- df[df$class == "numeric",]
integer_params <- df[df$class == "integer",]

# Overarching list to store results
output <- list()

    # Create a random sample of date variables (if any)
    if( NROW(date_params) > 0 ){
    
        date_output <- list()
    
        for( d in 1:NROW(date_params) ){
        
            date_lower <- as.Date(date_params[d,"lower_limit"])
            date_upper <- as.Date(date_params[d,"upper_limit"])
            sampled_dates <- sample(0:as.integer(date_upper - date_lower), size = n_samples)
        
            date_output[[d]] <- data.frame("dates" = sampled_dates + date_lower)
        }
    
        date_outputs <- do.call(cbind, date_output)
        names(date_outputs) <- date_params$parameter_name
        
        output[["date"]] <- date_outputs
    }

    # Create a random sample of numeric variables
    n_numeric_params <- NROW(numeric_params)
    if( n_numeric_params > 0 ){
        
        # Random sample of numeric variables
        sample_vec <- runif(
            n = n_samples*n_numeric_params, 
            min = as.numeric(numeric_params$lower_limit), 
            max = as.numeric(numeric_params$upper_limit))
        
        numeric_outputs <- as.data.frame(matrix(sample_vec, ncol = n_numeric_params, byrow = TRUE))
        names(numeric_outputs) <- numeric_params$parameter_name
        
        output[["numeric"]] <- numeric_outputs
    }
    
    # Concatenate the samples from different types of variables
    names(output) <- NULL
    df_sample <- do.call(cbind, output)
    
    # Reorder to original order
    df_sample <- df_sample[,df$parameter_name]
    
    # Add a parameter ID to the sample
    df_sample$param_id <- 1:NROW(df_sample)

	# Write the sample parameter set to a CSV file
	write.csv(df_sample, output_csv, row.names = FALSE, quote = FALSE)
}

