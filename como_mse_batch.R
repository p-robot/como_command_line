
library(tidyverse)
library(data.table)

args <- commandArgs(trailingOnly = TRUE)

glob_input_files <- args[1]
obs_file <- args[2]
sim_var <- args[3]
obs_var <- args[4]
output_file <- args[5]


# Calculate mean squared errors
# (since timeseries could be different lengths, we need to take mean)
mse <- function(obs, sim){
    return( mean( (obs - sim)**2 ) )
}


# Calculate MSE from CoMo data
# include aligning on dates
mse_como <- function(df_obs, df_sim, var_obs, var_sim){
    
    # Subset simulated data to the dates of the observed data.  
    min_final <- as.Date("2020-07-01")#max(c(min(df_obs$date), min(df_sim$date)))
    max_final <- as.Date("2021-07-01")# min(c(max(df_obs$date), max(df_sim$date)))
    
    vec_obs <- df_obs[[var_obs]][df_obs$date < max_final & df_obs$date > min_final]
    vec_sim <- df_sim[[var_sim]][df_sim$date < max_final & df_sim$date > min_final]
    
    return( mse(vec_obs, vec_sim) )
}

# Read the observed data
df_o <- read.csv(obs_file)
df_o$date <- as.Date(df_o$date)

# Load all CoMo output data
df <- fread(cmd = paste0("awk 'NR==1||FNR!=1' ", glob_input_files), header = TRUE)
df$date <- as.Date(df$date)

# Calculate mean squared error 
df_mse <- df %>% 
    group_by(param_id) %>% 
        summarize(err = mse_como(.data, df_o, sim_var, obs_var))

write.csv(df_mse, output_file, row.names = FALSE, quote = FALSE)

