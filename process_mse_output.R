# Script to process MSE calculations from output of the CoMo model

library(data.table)
library(ggplot2)
library(gridExtra)

args <- commandArgs(trailingOnly = TRUE)

param_file <- args[1]# 
simul_file_glob <- args[2] # output/21_07_16/lebanon_mse_row*.csv
input_dir <- args[3]
file_stub <- args[4]

# Avoid grid.arrange from opening devices unnecessarily
options(device = pdf)

# Load all MSE dataframes, load the parameter dataframe
df_mse <- fread(cmd = paste0("awk 'NR==1||FNR!=1' ", simul_file_glob), header = TRUE)
df_params <- read.csv(param_file)

# Merge and find those with MSE of top 1% MSE
df_mse <- merge(df_mse, df_params)
df_select_params <- df_mse[df_mse$err < quantile(df_mse$err, probs = 0.01), ]

# Order best-fitting parameters on 'err' column
df_select_params <- df_select_params[order(df_select_params$err),]

# Save best fitting parameters to file
write.csv(df_select_params, paste0(file_stub, "_best_fits.csv"), row.names = FALSE, quote = FALSE)

df_select_long <- melt(df_select_params, 
    id.vars = c("param_id", "date_range_simul_start", "date_range_variant_start", "init"))

# Marginal posteriors of parameters
p1 <- ggplot(subset(df_select_long, variable %in% c("new_variant_p_multiplier")), aes(value)) +
    geom_density(fill = "#009E73") + 
    theme_bw() + xlab("Parameter: new_variant_p_multiplier") + 
    xlim(c(1.43, 1.9)) + ylab("Density")
    
p2 <- ggplot(subset(df_select_long, variable %in% c("p")), aes(value)) +
    geom_density(fill = "#009E73") + 
    theme_bw() + xlab("Parameter: p") + 
    xlim(c(0.01, 0.05)) + ylab("Density")

p <- grid.arrange(p1, p2, nrow = 2, ncol = 1)

ggsave(paste0(file_stub, "param_hist_numeric.png"), p, width = 6, height = 6, units = "in")

# Plot hist of dates
# Plot hist of integer

ids <- df_select_params$param_id

# Pull the simulation output for these parameter IDs into a single dataframe
outputs <- list()
j <- 1
for(i in 1:length(ids)){
    outputs[[j]] <- read.csv(file.path(input_dir, paste0("como_output_row", ids[i], ".csv")))
    j <- j + 1
}

df_all <- do.call(rbind, outputs)
df_all$date <- as.Date(df_all$date)

# Read observed data
df_obs <- read.csv("data/lebanon_observed_21_07_14.csv")
df_obs$date <- as.Date(df_obs$date)


p <- ggplot(df_all, aes(x = date, y = baseline_death_deaths_from_covid_med, group = param_id)) +
    geom_line(alpha = 0.2, color = "#0072B2") + 
    geom_line(data = df_obs, aes(x = date, y = total_deaths), inherit.aes = FALSE, color = "red") +
    theme_bw() + xlab("Date") + ylab("Cumulative COVID19-related deaths")

ggsave(paste0(file_stub, "fitting_deaths.png"), p, width = 8, height = 6)

p <- ggplot(df_all, aes(x = date, y = baseline_predicted_reported_and_unreported_med, 
    group = param_id)) +
    geom_line(alpha = 0.2, color = "#0072B2") + 
    geom_line(data = df_obs, aes(x = date, y = new_cases), inherit.aes = FALSE, color = "red") +
    theme_bw() + xlab("Date") + ylab("Daily COVID19 infections")

ggsave(paste0(file_stub, "fitting_cases.png"), p, width = 8, height = 6)

library(tidyverse)
df_all <- df_all %>% 
    group_by(param_id) %>% 
    arrange(date) %>% 
    mutate(cum_cases = cumsum(baseline_predicted_reported_and_unreported_med))

df_all$seroprevalence <- 100*(df_all$cum_cases/6800000)
df_obs <- data.frame("date" = as.Date("2021-03-01"), "seroprevalence" = 40)

p <- ggplot(df_all, aes(x = date, y = seroprevalence, 
    group = param_id)) +
    geom_line(alpha = 0.2, color = "#0072B2") + 
    geom_point(data = df_obs, aes(x = date, y = seroprevalence), inherit.aes = FALSE, color = "red") +
    theme_bw() + xlab("Date") + ylab("Seroprevalence (%)")

ggsave(paste0(file_stub, "fitting_seroprevalence.png"), p, width = 8, height = 6)

