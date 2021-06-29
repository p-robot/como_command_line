#!/usr/bin/env Rscript
# 
# R script to generate a random uniform sample from parameter space
# 
# Author: W. Probert
# Created: June 2021

if( !interactive() ){
	args <- commandArgs(trailingOnly = TRUE)
	parameter_file <- args[1]
	n_axis_samples <- as.integer(args[2])
	output_csv <- args[3]
	
	df_params <- read.csv(parameter_file, stringsAsFactors = FALSE)
	n_params <- NROW(df_params)

	vectors_list <- list()
	for(i in 1:n_params){
		vectors_list[[i]] <- seq(df_params[i, "lower_limit"], df_params[i, "upper_limit"], length.out = n_axis_samples)

	}

	df_sample <- expand.grid(vectors_list)
	names(df_sample) <- df_params$parameter_name
	write.csv(df_sample, output_csv, row.names = FALSE, quote = FALSE)
}

