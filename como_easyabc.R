# Script to run an ABC calibration using the CoMo model
# and the EasyABC package

library(EasyABC)
library(parallel)
library(benchmarkme)

set.seed(2022)

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

        list_template <- load_template(file_path, country_name, USE_CPP)
	    
	# Adjust parameters
	list_template$parameters["p"] <- params[2]
	list_template$parameters["rho"] <- params[3]

	list_output <- run_model(list_template)
	df_sim <- process_outputs(list_output, list_template)
	return( df_sim$baseline_predicted_reported_and_unreported_med)
}

#################
# MODEL PARAMETERS
# -----------------

priors <- list(c("unif", 0, 0.1), c("unif", 0.0, 1.5))

#################
# ABC DEFINITION
# ---------------

pacc <- 0.25
nbsimul <- 1000
Ncores <- 7
abc_method <- "Lenormand"

ABC_Lenormand <- ABC_sequential(method = abc_method, model = como_model,
				prior = priors, nb_simul = nbsimul, use_seed = TRUE, inside_prior = TRUE,
				summary_stat_target = sum_stat_obs, n_cluster = Ncores, max_pick = 100000,
				p_acc_min = pacc, verbose = TRUE)

# Add ABC method used in EasyABC package
ABC_Lenormand$abc_method <- abc_method

# Today's date
today <- format(Sys.time(), "%Y_%m_%d_%H%M")


# Save repo-, hardware-, and OS-specific information
current_ram <- benchmarkme::get_ram()
current_cores <- parallel::detectCores()

system_info <- list(
	Sys.time = Sys.time(),
	ram = current_ram, 
	cores = current_cores, 
	Sys.info = Sys.info(),
	R.Version = R.Version(),
	R.home = R.home(), 
	wd = getwd(), 
	commit = system("git rev-parse HEAD", intern = TRUE), 
	repo_remote = system("git remote get-url origin", intern = TRUE))

save(ABC_Lenormand, system_info, file = paste0(today, "_abc_output.Rdata"))


