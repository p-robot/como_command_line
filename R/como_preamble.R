#!/usr/bin/env Rscript

# On some systems it's not possible to access timedatectl command (permissions)
# work around is to set the timezone in R environment
Sys.setenv(TZ="Europe/London")

suppressWarnings(suppressMessages(library(ggplot2)))
suppressWarnings(suppressMessages(library(reshape2)))
suppressWarnings(suppressMessages(library(deSolve)))
suppressWarnings(suppressMessages(library(readxl)))
suppressWarnings(suppressMessages(library(tidyverse)))
suppressWarnings(suppressMessages(library(comoOdeCpp)))
cat("Loaded packages\n")
