#!/usr/bin/env Rscript

require(ggplot2)

###################
# COMMAND-LINE ARGS
# -----------------

args <- commandArgs(trailingOnly = TRUE)
file_path <- args[1]
output_file <- args[2]
var <- args[3]

# Read input data
df_all <- read.csv(file_path)

df_all$date <- as.Date(df_all$date)

p <- ggplot(df_all, aes_string(x = "date", y = var)) + 
    geom_line(size = 0.8, alpha = 0.6) + 
    theme_bw() + xlab("Date")

ggsave(output_file, p, width = 8, height = 8)

#df_sub <- unique(subset(df_all, select = c("date", "baseline_death_deaths_from_covid_med", "baseline_death_deaths_with_covid_med", "p") ))

#p <- ggplot(df_sub, aes(x = date, y = baseline_death_deaths_from_covid_med + baseline_death_deaths_with_covid_med, color = p, group = p)) + 
#    geom_line() + 
#    xlab("Prob. of infection given contact") + 
#    ylab("Covid-19 deaths from 2019-12-02 to 2020-12-31") +
#    geom_hline(yintercept = 23749, linetype = "dashed", color = "grey") + 
#    geom_vline(xintercept = 0.02, linetype = "dashed", color = "grey") + 
#    theme_bw()
#
#ggsave(paste0(output_file, "_test_output_deaths_vs_p.pdf"), p)
