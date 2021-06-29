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
	
	set.seed(seed)

	df_params <- read.csv(parameter_file, stringsAsFactors = FALSE)
	n_params <- NROW(df_params)

	sample_vec <- runif(n_samples*n_params, df_params$lower_limit, df_params$upper_limit)
	df_sample <- as.data.frame(matrix(sample_vec, ncol = n_params, byrow = TRUE))
	names(df_sample) <- df_params$parameter_name
	write.csv(df_sample, output_csv, row.names = FALSE, quote = FALSE)
}

