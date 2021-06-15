# Script to run a sequential ABC algorithm

library(truncnorm)

# Read observed data (p = 0.0245)
df_obs <- read.csv("tests/data/COVID19_App_Data_Template_CoMoCOVID-19App_v17_p0.0245.csv")

output_dir <- "tests/data/"

# ABC parameters

n_params <- 1		# Number of parameters
N <- 500		# Number of particles to accept
T <- 10			# Number of populations
epsilon_quantile <- 0.3	# Quantile defining epsilon

USE_CPP <- TRUE

# Allocate memory to store parameters and weights
params_current <- matrix(NA, nrow = N, ncol = n_params)
params_prev <- matrix(NA, nrow = N, ncol = n_params)
weights_current <- rep(NA, times = N)
weights_prev <- rep(NA, times = N)

dist_current <- rep(NA, times = N)
epsilons <- c(Inf, rep(NA, times = T-1))

# Define the location of the base template
file_path <- "tests/data/Template_CoMoCOVID-19App_v17.xlsx"
country_name <- "United Kingdom of Great Britain"

sse <- function(obs, sim){
	return(sum((obs - sim)**2))
}

start_time <- Sys.time()

for( t in 1:T ){
	n_accepted <- 0
	
	while( n_accepted < N ){
		source("R/como_preamble.R")
		source("R/model_once.R")
		source("R/como_functions.R")
		#source("R/fun_covid.R")
		
		# Load the template file
		list_template <- load_template(file_path, country_name, USE_CPP)
		
		# Check if this is the first population
		# if so, sample from priors, else sample from 
		# previous population
		if( t == 1 ){
			
			params_star_star <- runif(1, min = 0, max = 0.2)	# Prob. of infection given contact
			
		}else{
			params_star_star <- c(-1)
			while(params_star_star < 0 | params_star_star > 0.2){
				param_idx <- sample(seq(1, N), 1 , prob = weights_prev)
				params <- params_prev[param_idx]
				params_star_star <- params + rnorm(1, 0, sigma) # FIXME
			}
		}
		list_template$parameters["p"] <- params_star_star

		# Simulate using the sampled parameters
		list_output <- run_model(list_template)
		df_sim <- process_outputs(list_output, list_template)
		
		# Calculate distance
		dist <- sse(df_sim$baseline_predicted_reported_and_unreported_med, 
					df_obs$baseline_predicted_reported_and_unreported_med)
		
		# Check distance
		if( dist < epsilons[t] ){
			# Updated counter of accepted particles
			n_accepted <- n_accepted + 1
			
			# Save parameter values
			params_current[n_accepted] <- params_star_star
			
			# Calculate weights
			if( t == 1 ){
				weights_current[n_accepted] <- 1
			}else{
				numerator <- dunif(params_star_star, min = 0, max = 0.2)
				denominator <- sum(weights_prev * dtruncnorm(param_star_star, mean = params_prev, sigma = sigma, a = 0, b = 0.2))
				if(denominator == 0){ cat("Denominator zero!\n")}
				weights_current[n_accepted] <- numerator/denominator
			}
			
			# Save distance
			dist_current[n_accepted] <- dist
			
			cat("Population", t, " | ")
			cat("Accepted particles: ", n_accepted, "/", N, " | ")
			cat("Dist: ", dist, " | ")
			cat("p: ", params_star_star, "\n")
		}
	}
	sigma <- sqrt(var(params_current))
	params_prev <- params_current
	
	weights_prev <- weights_current / sum(weights_current)
	epsilons[t + 1] <- quantile(dist_current, probs = epsilon_quantile)
	
	# Save population t
	write.csv(params_current, file.path(output_dir, paste0("params_t", t, ".csv")), row.names = F)
	
	# Save weights
	write.csv(weights_current, file.path(output_dir, paste0("weights_t", t, ".csv")), row.names = F)

	# Print timing
	cat("TIME: ", Sys.time() - start_time)
}
