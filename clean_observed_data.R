#!/usr/bin/env Rscript
# 
# Script to clean data from Our World In Data (global) to only be 
# data for Lebanon (so file is much smaller)
# 
# W. Probert, 2021

# Read full dataset
df_obs <- read.csv("data/owid-covid-data.csv")

# Subset columns and subset to Lebanon
df_obs <- subset(df_obs, location == "Lebanon", select = c("date", "total_cases", "new_cases", "total_deaths", "new_deaths"))

# Subset dates to only those of interest
df_obs$date <- as.Date(df_obs$date)
df_obs <- subset(df_obs, date <= as.Date("2021-07-14"))

# Convert NA values to zero
df_obs[is.na(df_obs)] <- 0

# Write observed data for Lebanon to file
write.csv(df_obs, "data/lebanon_observed_21_07_14.csv", row.names = FALSE, quote = FALSE)

