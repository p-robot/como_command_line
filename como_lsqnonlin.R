#!/usr/bin/env Rscript
# 
# Script to solve the CoMo model using non-linear least-squares 
# - Levenberg-Marquardt algorithm (lsqnonlin from pracma package)
# - Nelder-Mead algorithm (default for optim() function from stats package)

############
# PREAMBLE
# ---------

library(deSolve)
library(pracma) # for lsqnonlin()

# Define sample data

output_obs <- c(sample_data$x, sample_data$y)

LV <- function(Time, State, Pars){
    
    with(as.list(c(State, Pars)), {
    
    a <- Pars[1]
    b <- Pars[2]
    
    dx <- a*x - x*y
    dy <- b*x*y - y
    
    return(list(c(dx, dy)))
    }
    )
}


# Initial conditions (from eyeing off the graph)
init <- c(x = 1.0, y = 0.5)

# Time steps to simulate
all_times <- seq(0, 15, length.out = 151)


# Function definition
lv_solve <- function(pars){

    lv_soln <- lsoda(func = LV, y = init, parms = pars, times = all_times)
    lv.df <- as.data.frame(lv_soln)
    
    lv.df$time <- round(lv.df$time, 2)
    
    soln <- lv.df[lv.df$time %in% sample_data$time,]
    output <- c(soln$x, soln$y)
    
    return( output - output_obs )
}


pars_init <- c(0.5, 0.5)
lsqnonlin(lv_solve, pars_init)

optim(pars_init, function(x) sum(lv_solve(x)**2), method = "Nelder-Mead")

optim(pars_init, function(x) sum(lv_solve(x)**2), method = "BFGS")

# Very slow
# optim(pars_init, function(x) sum(lv_solve(x)**2), method = "SANN", control = list(verbose = 1))


