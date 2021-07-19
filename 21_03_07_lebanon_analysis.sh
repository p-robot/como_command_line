
# Script to run a grid search across parameter space for fitting to the COVID19 outbreak in Lebanon
# EMRO Modelling Support Team, 2021

seed=2021
n_samples=10000
template="data/Template_CoMoCOVID-19App_v17_variant_vaccination.xlsx"


parameter_sample="parameters/21_07_16_lebanon_parameters.csv"
output_dir="output/21_07_16/"

Rscript como_random_sample.R $seed parameters/calibrated_parameters.csv $n_samples $parameter_sample

# Simulate each parameter set in parallel
mkdir -p $output_dir

# Run the following from the command-line
#nohup Rscript como_parallel.R "Lebanon" "$template" $parameter_sample $output_dir > log/21_07_16_parallel.log 2> log/21_07_16_parallel.err &

# Using an additional parameter rho and gamma, setting init
Rscript como_random_sample.R \
	2021 \
	parameters/calibrated_parameters_rhogammavariable.csv \
	10000 \
	"parameters/21_07_16_lebanon_parameters_rhogammavariable.csv"

nohup Rscript como_parallel.R \
	"Lebanon" \
	"data/Template_CoMoCOVID-19App_v17_variant_vaccination.xlsx" \
	"parameters/21_07_16_lebanon_parameters_rhogammavariable.csv" \
	output/21_07_16_rhogammavariable > log/21_07_16_parallel_rhogammavariable.log 2> log/21_07_16_parallel_rhogammavariable.err &

