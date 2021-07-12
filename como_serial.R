#!/usr/bin/env Rscript
# 
# Script to run the CoMo model in serial across a grid of parameter values

library(benchmarkme)
library(parallel)

args <- commandArgs(trailingOnly = TRUE)

# Parse command-line arguments
country_name <- args[1]
file_path <- args[2]
parameters_csv <- args[3]
output_dir <- args[4]

USE_CPP <- TRUE

df_params <- read.csv(parameters_csv, stringsAsFactors = FALSE)
n_params <- NCOL(df_params) - 1


for(i in 1:NROW(df_params)){
	cat("Run ", i, "\n")
	source("R/como_preamble.R")
	source("R/model_once.R")
	source("R/como_functions.R")
	source("R/como_read_data.R")

	# Read Excel template
	raw_template <- parse_excel_template(file_path)
	
	# Adjust parameters
	for(j in 1:n_params){
		
		# Get parameter name
		param_name <- names(df_params)[j]
		cat(param_name, "\n")
		
		# Find the worksheet in the template where this parameter is housed
		coords <- find_parameter_worksheet(param_name, raw_template)
		worksheet <- coords$worksheet
		row <- coords$row
		cat(worksheet, "\n")

		# Adjust parameter in the raw excel template.  
		if( grepl("date", param_name) ){
			raw_template[[worksheet]][row, "Value_Date"] <- as.Date(df_params[i, param_name])
			cat("adjusting param ", param_name, " to value ", format(as.Date(df_params[i, param_name]), format = "%d/%m/%Y"), "\n")
		}else{
			raw_template[[worksheet]][row, "Value"] <- df_params[i, param_name]
			cat("adjusting param ", param_name, " to value ", df_params[i, param_name], "\n")
		}

        	list_template <- clean_excel_template(raw_template, country_name, USE_CPP)
	}
	cat("\n")
	list_output <- run_model(list_template)
	df_sim <- process_outputs(list_output, list_template)
	
	# Add a parameter ID (if that column name exists in the parameter df, else use row number
	if( "param_id" %in% names(df_params) ){
		df_sim$param_id <- df_params[i, "param_id"]
	}else{
		df_sim$param_id <- i
	}
	
	write.csv(df_sim, file.path(output_dir, paste0("como_output_row", i, ".csv")), 
        row.names = FALSE, quote = FALSE)
}

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

