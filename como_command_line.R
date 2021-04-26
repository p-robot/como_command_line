#!/usr/bin/env Rscript


##########
# PREAMBLE
# --------

source("como_preamble.R")
source("como_functions.R")

###################
# COMMAND-LINE ARGS
# -----------------

args <- commandArgs(trailingOnly=TRUE)

# Parse command-line arguments
country_name <- args[1]
file_path <- args[2]
output_file <- args[3]

source("como_read_data.R")

out0 <- ode(y = Y, times = times, method = "euler", hini = 0.05, func = covid, parms = parameters, input=vectors0)

simul_interventions <- process_ode_outcome(out_mean = out0, intv_vector = vectors0, param_used = parameters, iterations = 1)
write.csv(simul_interventions, output_file)

simul_interventions$time <- as.Date(simul_interventions$time)

p <- ggplot(as.data.frame(simul_interventions), aes(x = time, y = N)) + 
    geom_line()

ggsave("test_output.pdf", p)

