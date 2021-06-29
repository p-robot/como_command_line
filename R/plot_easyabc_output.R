#!/usr/bin/env Rscript

require(ggplot2)
require(reshape2)

###################
# COMMAND-LINE ARGS
# -----------------

args <- commandArgs(trailingOnly = TRUE)
abc_file <- args[1]
abc_object_name <- args[2]
observed_file <- args[3]
output_figure <- args[4]

# Read saved data
load(abc_file)
df_obs <- read.csv(observed_file)
df_obs$time <- 1:NROW(df_obs)

abc_object <- get(abc_object_name)

if( !exists("system_info") ){
	system_info <- list()
}

caption_metadata <- paste0(
		"ABC algorithm: ", abc_object$abc_method, "; ",
		"p_acc_min: ", abc_object$p_acc_min, "; ",
		"nb_simul: ", abc_object$nb_simul, ";", 
		"runtime: ", round(abc_object$computime/60/60, 2), " hours;\n",
		"commit: ", system_info$commit, "; ", 
		"remote: ", system_info$repo_remote, "; "
		)

# Pull metadata from ABC output object
n_param <- NCOL(abc_object$param)
n_popn <- length(abc_object$intermediary)
df <- abc_object$intermediary[[n_popn]]$posterior

# Reshape ABC output
df <- df[, (n_param+2):NCOL(df)]
df <- as.data.frame(t(df)); df$time <- 1:NROW(df)
df_long <- melt(df, id.vars = "time")

# Generate line graph of output
p <- ggplot(df_long, aes(x = time, y = value/1000, group = variable)) + 
    geom_line(aes(color = "#D55E00"), alpha = 0.05) + 
    theme_classic() + 
    geom_line(data = df_obs, aes(x = time, y = baseline_predicted_reported_and_unreported_med/1000, color = "black"), inherit.aes = F) + 
    xlim(0, 300) +
    theme(legend.position = "top") + 
    scale_colour_manual(
        name = "", 
        values =c('#D55E00' = '#D55E00','black' = 'black'), 
        labels = c('Simulated', 'Observed'), guide = 'legend') + 
    xlab("Time") + ylab("Total infections (000's)") + 
    labs(
        title = "CoMo model fit using ABC",
        subtitle = "Fit of parameter sets of p & rho", 
    	caption = caption_metadata)

ggsave(output_figure, p, width = 8, height = 6)

