# `como_command_line`


Command-line version of the CoMo model.  


Requirements
------------

Tests were run on R version 4.1.0.  For the CPP version of the solver (much faster), one needs to install the comoOdeCpp package, version 16.6.0.  From within R, this can be installed like so: 

```bash
# Make personal library for R packages if necessary
mkdir "~/R/x86_64-pc-linux-gnu-library/4.1"

R -e 'library(devtools); install_github("bogaotory/comoOdeCpp@v16.6.0", subdir="comoOdeCpp")'
```


Usage
-----

The main script `como_command_line.R` can be called from a terminal like so: 
```bash
Rscript como_command_line.R <country_name> <template_path> <output_file>
```

On cluster systems, such as AWS Lightsail, the script can be set running using: 

```bash
nohup Rscript como_abc_smc.R > abc.log 2> abc.err &
```
Sending output to `abc.log` and error to `abc.err`.  


Tests
-----

Templates for v17 of the CoMo model are taken from the [CoMo github release v17.2.0](https://github.com/ocelhay/como/tree/1b61938191d9f63d512a3aaec9f5271a3ca0ed5a) (or [here](https://github.com/ocelhay/como/releases/tag/v17.2.0)).  The file `Template_CoMoCOVID-19App_v17.xlsx` is available for download from that link.  This was process with V17.2 of the Shiny App to generate the output file `COVID19_App_Data_Template_CoMoCOVID-19App_v17.csv` against which the command-line invocation is compared.  


All tests can be run using the script `run_all_tests.sh`.  


