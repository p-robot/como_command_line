# Script to run a sequential ABC algorithm

# Read observed data

# ABC parameters

n_params <- 2	# Number of parameters
N <- 100	# Number of particles to accept
T <- 5 		# Number of populations
q <- 0.9	# Quantile defining epsilon

# Priors
# Starting date

# p

USE_CPP <- TRUE
source("R/como_preamble.R")
source("R/model_once.R")
source("R/como_functions.R")


# Allocate memory to store parameters and weights
params_current <- matrix(NA, nrow = N, ncol = n_params)
params_prev <- matrix(NA, nrow = N, ncol = n_params)
weights_current <- rep(NA, times = N)
weights_prev <- rep(NA, times = N)

# Define the location of the base template
file_path <- ""
country_name <- "United Kingdom of Great Britain"

for( t in 1:T ){
	n_accepted <- 0
	while( n_accepted <= N ){

		# Load the template file
		list_template <- load_template(file_path, country_name, USE_CPP)
		
		# Check if this is the first population
		# if so, sample from priors, else sample from 
		# previous population
		if( t == 1 ){
			
		}else{
			
		}

		# Simulate using the sampled parameters
		list_output <- run_model(list_template)
		df_output <- process_outputs(list_output, list_template)
		
		# Calculate distance
		dist <- 10

		# Check distance
		if( dist < epsilon[t] ){
			# Save parameter values

			# Calculate weights

		}

	}
	# Save population t
	# Save weights
	
}
