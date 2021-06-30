#!/usr/bin/env Rscript
# 
# Script to solve the CoMo model using non-linear least-squares 
# - Levenberg-Marquardt algorithm (lsqnonlin from pracma package)
# - Nelder-Mead algorithm (default for optim() function from stats package)
# stats::constrOptim()
# 
# W. Probert

############
# PREAMBLE
# ---------

library(pracma) # for lsqnonlin()

# Define sample data
output_dir <- "tests/data/"

################
# OBSERVED DATA
# --------------
# Read observed data (p = 0.0245; rho = 50%)
df_obs <- read.csv("tests/data/COVID19_App_Data_Template_CoMoCOVID-19App_v17_p0.0245.csv")

sum_stat_obs <- df_obs$baseline_predicted_reported_and_unreported_med

###################
# MODEL DEFINITION
# -----------------

como_model <- function(params){
	source("params_easyabc.R")
	source("R/como_preamble.R")
	source("R/model_once.R")
	source("R/como_functions.R")

	df_params <- read.csv("parameters/calibrated_parameters.csv", stringsAsFactors = FALSE)
	n_params <- NROW(df_params)

	list_template <- load_template(file_path, country_name, USE_CPP)
	print(params)
	
	# Adjust parameters
	for(i in 1:n_params){
		list_template$parameters[df_params$parameter_name[i]] <- params[i]
	}

	list_output <- run_model(list_template)
	df_sim <- process_outputs(list_output, list_template)

	sum_stat_sim <- df_sim$baseline_predicted_reported_and_unreported_med
	return( sum_stat_sim - sum_stat_obs)
}

set.seed(2021)
pars_init <- c(runif(1, 0, 0.1), runif(1, 0, 1.5))
print(pars_init)
#lsqnonlin(como_model, pars_init)

nlminb(pars_init, function(x) sum(como_model(x)**2), lower = c(0.001, 0.01), upper = c(0.1, 1.5))

#optim(pars_init, function(x) sum(como_model(x)**2), method = "L-BFGS-B", lower = c(0, 0), upper = c(0.1, 1.5))

#optim(pars_init, function(x) sum(como_model(x)**2), method = "Nelder-Mead")

#optim(pars_init, function(x) sum(lv_solve(x)**2), method = "BFGS")

# Very slow
# optim(pars_init, function(x) sum(lv_solve(x)**2), method = "SANN", control = list(verbose = 1))


