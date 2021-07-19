#!/usr/bin/env Rscript
# 
# Script to plot a single timeseries output from the CoMo model
# 
# Usage
# -----
# 
# Rscript plot_como_output.R <INPUT_FILE> <PLOTTING_VAR> <OUTPUT_FILE>
# 
# Arguments
# ---------
# INPUT_FILE: Path to CSV of CoMo output
# PLOTTING_VAR: Column name of the CoMo output file to plot against 'date'
# OUTPUT_FILE: File of output figure to save
# 
# W. Probert, 2021

library(ggplot2)

args <- commandArgs(trailingOnly = TRUE)

input_file <- args[1]
plotting_var <- args[2]
observed_file <- args[3]
observed_var <- args[4]
output_file <- args[5]
xlab <- args[6]
ylab <- args[7]

df <- read.csv(input_file)

# Convert date to date class
df$date <- as.Date(df$date)

df_obs <- read.csv(observed_file)
df_obs$date <- as.Date(df_obs$date)


p <- ggplot(df, aes_string(x = "date", y = plotting_var)) + 
	geom_line() + 
	geom_line(data = df_obs, aes_string(x = "date", y = observed_var), color = "red") + 
	theme_bw() + xlab(xlab) + ylab(ylab)


ggsave(output_file, p, height = 6, width = 8, dpi = 300)

