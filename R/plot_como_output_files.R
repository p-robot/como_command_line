#!/usr/bin/env Rscript
# 
# Script to plot timeseries of a single variable from 
# several output files from the CoMo model

require(ggplot2)
require(argparse)

###################
# COMMAND-LINE ARGS
# -----------------
parser <- ArgumentParser()

parser$add_argument("-v", "--variable", 
    type="character", help="Variable name to plot against 'date'")

parser$add_argument("-f", "--files", nargs = "+",
    type="character", help="Input files of CoMo output to plot against each other")

parser$add_argument("-o", "--output_file", 
    type="character", help="Output file name of generated figure")

args <- parser$parse_args()

# Read input data
output <- list(); i <- 1
for(f in args$files){
  df <- read.csv(f)
  df$basename <- basename(f)
  output[[i]] <- df
  i <- i + 1
}

df_all <- do.call(rbind, output)

df_all$date <- as.Date(df_all$date)

p <- ggplot(df_all, aes_string(x = "date", y = args$variable, color = "basename")) + 
    geom_line(size = 0.8, alpha = 0.6) + 
    theme_bw() + xlab("Date") + 
    theme(legend.position = "top") + 
    guides(color = guide_legend(title = "", ncol = 1))

ggsave(args$output_file, p, width = 8, height = 8)
