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
library(data.table)

args <- commandArgs(trailingOnly = TRUE)

glob_input_files <- args[1]
plotting_var <- args[2]
output_file <- args[3]

group_var <- "param_id"

df <- fread(cmd = paste0("awk 'NR==1||FNR!=1' ", glob_input_files), header = TRUE)

# Convert date to date class
df$date <- as.Date(df$date)

p <- ggplot(df, aes_string(x = "date", y = plotting_var, group = group_var)) + 
	geom_line(size = 0.8, alpha = 0.6) +
	xlab("Date") + 
	theme_bw()

ggsave(output_file, p, height = 6, width = 8, dpi = 300)

