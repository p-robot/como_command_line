#!/usr/bin/env Rscript
# 
# Script to run an ABC calibration using the CoMo model
# and the EasyABC package

library(parallel)
library(foreach)
library(doParallel)
library(benchmarkme)

output_dir <- "output/"
df_params <- read.csv("parameters/lebanon_parameters.csv", stringsAsFactors = FALSE)
n_params <- NCOL(df_params)

n_cores <- 3
clust <- parallel::makeCluster(n_cores, type = "PSOCK")
doParallel::registerDoParallel(cl = clust)

foreach(i = 1:NROW(df_params)) %dopar% {
	cat("Run ", i, "\n")
	source("params_easyabc.R")
	source("R/como_preamble.R")
	source("R/model_once.R")
	source("R/como_functions.R")

        list_template <- load_template(file_path, country_name, USE_CPP)
	    
	# Adjust parameters
	for(j in 1:n_params){
		param_name <- names(df_params)[j]
		list_template$parameters[param_name] <- df_params[i, param_name]
		cat("adjusting param ", param_name, " to value ", df_params[i, param_name])
	}
	cat("\n")

	list_output <- run_model(list_template)
	df_sim <- process_outputs(list_output, list_template)

	write.csv(df_sim, file.path(output_dir, paste0("output_row", i, ".csv")), row.names = FALSE)
}

# Stop the cluster
parallel::stopCluster(cl = clust)

# Add additional information to the ABC object that was run
#ABC_Lenormand$abc_method <- abc_method
#ABC_Lenormand$calibrated_parameters <- df_params
#ABC_Lenormand$n_cluster <- Ncores
#ABC_Lenormand$nb_simul <- nbsimul
#ABC_Lenormand$p_acc_min <- pacc
#ABC_Lenormand$prior <- priors

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

save(system_info, file = file.path(output_dir, paste0(today, "_abc_output.Rdata")))


