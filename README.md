# `como_command_line`


Command-line version of the CoMo model.  


Requirements
------------

Tests were run on R version 4.1.0.  For the CPP version of the solver (much faster), one needs to install the comoOdeCpp package, version 16.6.0.  From within R, this can be installed like so: 

```bash
R -e 'library(devtools); install_github("p-robot/comoOdeCpp@new_variant", subdir="comoOdeCpp")'

# The original CPP folder is found here (but does not include the ability to adjust transmission from introduction of a new variant): 
R -e 'library(devtools); install_github("bogaotory/comoOdeCpp@v16.6.0", subdir="comoOdeCpp")'
```

The CoMo model uses the following R packages: `tidyverse`, `deSolve`.  


If the model is to be run on an AWS Lightsail, then the script [`lightsail_setup.sh`](lightsail_setup.sh) includes all commands needed to prepare the lightsail node for deploying the command-line version of CoMo.  It may be required to define a library for R packages: 

```bash
# Make personal library for R packages if necessary
mkdir "~/R/x86_64-pc-linux-gnu-library/4.1"
```

Basic usage
-----------

The main script `como_command_line.R` can be called from a terminal like so which will run the CoMo model for a particular country, using the specified template, and saving output to the desired CSV file: 
```bash
Rscript como_command_line.R <country_name> <template_path> <output_file>
```

where the input arguments are the following: 

* `country_name`: country name of interest
* `template_path`: file path to the 'baseline' CoMo template
* `output_file`: file path to the output CSV file


**Options**

* The CoMo model is solved using the CPP solver `comoOdeCpp` by default.  Should one wish to use the R version of the solver (i.e. via the deSolve package - a slower solver), then adjust the option `USE_CPP <- FALSE` in the script `como_command_line.R.  

* This version of the CoMo model also includes the ability to include the introduction of a new variant at a particular point in time, with a multiplier on the variable `p` (the probability of transmission given contact).  These parameters are included in a new worksheet in the template called `Extra Param`.  See the example in `tests/data/Template_CoMoCOVID19-App_v17_variant.xlsx`.  

Parameter grid search
---------------------

*Generate parameter sample*


A grid search across parameter space can be performed.  A grid of parameters first needs to be generated which can be performed using the following call:

```bash
Rscript como_random_sample.R <seed> <calibrated_parameters_csv> <n_samples> <output_csv>
```
where the input arguments are the following: 

* `seed`: Integer used to see the random number generator in R
* `calibrated_parameters_csv`: CSV file of parameters to generate a random grid search across (see example in `parameters/calibrated_parameters.csv`.  
* `n_samples`: Number of samples to draw from a random grid.  
* `output_csv`: CSV file of sample from parameter space (number of columns is number of parameters and `n_samples` rows).  


*Simulate CoMo model across parameter sample*

Evaluating the model for each parameter value, adjusted from a baseline template, can be run in the following manner, which will save a new file in the `output_dir`, entitled `como_output_row<i>.csv`, for each row `i` of the `parameter_csv` file: 

```bash
Rscript como_parallel.R <country_name> <template_path> <parameter_csv> <output_dir>
```

where the input arguments are the following: 

* `country_name`: country name of interest
* `template_path`: file path to the 'baseline' CoMo template
* `parameter_csv`: path to a CSV file of parameter values over which to evaluate the como model (adjusted from the baseline template).  
* `output_dir`: directory of where to save CoMo output files (this directory should exist already)


On AWS Lightsail, running the model across a grid search can be set running using the following (which will run in the background even if the user logs off): 

```bash
nohup Rscript como_parallel.R > log/como_parallel.log 2> log/como_parallel.err &
```
Sending output to file `log/como_parallel.log` and error to file `log/como_parallel.err`.  


Tests
-----

Templates for v17 of the CoMo model are taken from the [CoMo github release v17.2.0](https://github.com/ocelhay/como/tree/1b61938191d9f63d512a3aaec9f5271a3ca0ed5a) (or [here](https://github.com/ocelhay/como/releases/tag/v17.2.0)).  The file `Template_CoMoCOVID-19App_v17.xlsx` is available for download from that link.  This was process with V17.2 of the Shiny App to generate the output file `COVID19_App_Data_Template_CoMoCOVID-19App_v17.csv` against which the command-line invocation is compared.  


All tests can be run using the script `run_all_tests.sh`.  

