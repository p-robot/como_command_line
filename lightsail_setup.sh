#!/bin/sh
# 
# Set-up script for running the command-line version of CoMo model on Amazon's AWS Lightsail.  

apt-get update -y

apt-get install r-base -y
apt-get install r-cran-ggplot2 r-cran-tidyverse r-cran-desolve -y
apt-get install libcurl4-openssl-dev libssl-dev libcurl4-gnutls-dev libxml2-dev -y

# Install XQuartz on client side.  
# Make sure X11 forwarding is on in ssh config file.  
apt-get install x11-apps -y

R -e 'dir.create(Sys.getenv("R_LIBS_USER"), recursive = TRUE); .libPaths(Sys.getenv("R_LIBS_USER"))'
R -e 'install.packages("devtools")'
R -e 'install.packages("foreach")'
R -e 'install.packages("parallel")'
R -e 'install.packages("EasyABC")'
R -e 'install.packages("benchmarkme")'
R -e 'install.packages("doParallel")'

# If errors with testthat occur, this might resolve the issue: 
#R -e 'install.packages("rlang", dependencies = c("Depends", "Imports", "LinkingTo", "Suggests"))'
