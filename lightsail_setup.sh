pt-get update -y

apt-get install r-base -y
apt-get install r-cran-ggplot2 r-cran-tidyverse r-cran-desolve -y
apt-get install libcurl4-openssl-dev libssl-dev libcurl4-gnutls-dev libxml2-dev -y

# Install XQuartz on client side.  
# Make sure X11 forwarding is on in ssh config file.  
apt-get install x11-apps -y

# For running OpenABM-COVID19
apt-get install make -y # For GNU make
apt-get install gcc -y # For gcc C compiler
apt-get install libgsl-dev -y # for GSL libraries
apt-get install swig -y # for SWIG
apt-get install python3-dev -y # for Python.h headers


R -e 'dir.create(Sys.getenv("R_LIBS_USER"), recursive = TRUE); .libPaths(Sys.getenv("R_LIBS_USER"))'
#R -e 'install.packages("rlang", dependencies = c("Depends", "Imports", "LinkingTo", "Suggests"))'
R -e 'install.packages("devtools")'
R -e 'install.packages("foreach")'
R -e 'install.packages("parallel")'
R -e 'install.packages("EasyABC")'
R -e 'install.packages("benchmarkme")'

