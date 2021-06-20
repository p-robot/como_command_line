# Script to run an ABC calibration using the CoMo model
# and the EasyABC package

library(EasyABC)
set.seed(2021)

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
file_path <- "tests/data/Template_CoMoCOVID-19App_v17.xlsx"
country_name <- "United Kingdom of Great Britain"
USE_CPP <- TRUE

como_model <- function(params){
	source("R/como_preamble.R")
	source("R/model_once.R")
	source("R/como_functions.R")

        list_template <- load_template(file_path, country_name, USE_CPP)
	    
	# Adjust parameters
	list_template$parameters["p"] <- params[1]
	list_template$parameters["rho"] <- params[2]

	list_output <- run_model(list_template)
	df_sim <- process_outputs(list_output, list_template)
	return( df_sim$baseline_predicted_reported_and_unreported_med)
}

#################
# MODEL PARAMETERS
# -----------------

priors <- list(c("unif", 0, 0.2), c("unif", 0.0, 1.5))

#################
# ABC DEFINITION
# ---------------

pacc <- 0.05
nb_simul <- 500
ABC_Lenormand <- ABC_sequential(method = "Lenormand", model = como_model,
				prior = priors, nb_simul = nb_simul, 
				summary_stat_target = sum_stat_obs,
				p_acc_min = pacc, verbose = TRUE)

save(ABC_Lenormand, file = "ABC_output.Rdata")

