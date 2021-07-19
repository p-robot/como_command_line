

for i in 1 2 3 4 5 6 7 8 9;
do
    Rscript como_mse_batch.R \
        "output/21_07_16/como_output_row${i}*.csv" \
        "data/lebanon_observed_21_07_14.csv" \
        baseline_death_deaths_from_covid_med \
        total_deaths \
        output/21_07_16/lebanon_mse_row${i}_s2021_n10000.csv
done

# Process MSE calculations into 1 file and make figures
Rscript process_mse_output.R parameters/21_07_16_lebanon_parameters.csv "output/21_07_16/lebanon_mse_row*.csv"


# FIND BEST FITTING PARAMS, PUT THEM IN A TEMPLATE, RUN ANALYSIS

# Plot best fitting parameter set, deaths
Rscript R/plot_como_output_observed.R \
	results/OUTPUT_Template_CoMoCOVID-19App_v17_variant_vaccination_best_fit.csv \
	baseline_death_deaths_from_covid_med \
	data/lebanon_observed_21_07_14.csv \
	"total_deaths" \
	results/best_fit_deaths_obs.png \
	"Date" \
	"Cumulative COVID19-related deaths"

# Plot best fitting parameter set, cases
Rscript R/plot_como_output_observed.R 
	results/OUTPUT_Template_CoMoCOVID-19App_v17_variant_vaccination_best_fit.csv \
	baseline_predicted_reported_and_unreported_med \
	data/lebanon_observed_21_07_14.csv \
	"new_cases" \
	results/best_fit_cases_obs.png \
	"Date" \
	"Daily COVID19 infections (observed is cases)"

# PARAMETER RHO as 50%
for i in 1 2 3 4 5 6 7 8 9
do
    Rscript como_mse_batch.R \
        "output/21_07_16_rho50/como_output_row${i}*.csv" \
        "data/lebanon_observed_21_07_14.csv" \
        baseline_death_deaths_from_covid_med \
        total_deaths \
        "output/21_07_16_rho50/lebanon_mse_row${i}_s2021_n10000.csv"
done

# Process MSE calculations into 1 file and make figures
Rscript process_mse_output.R \
    parameters/21_07_16_lebanon_parameters.csv \
    "output/21_07_16_rho50/lebanon_mse_row*.csv" \
    "output/21_07_16_rho50" \
    "results/21_07_16_rho50_"


# PARAMETER RHO and GAMMA as calibrated
for i in 1 2 3 4 5 6 7 8 9
do
    Rscript como_mse_batch.R \
        "output/21_07_16_rhogammavariable/como_output_row${i}*.csv" \
        "data/lebanon_observed_21_07_14.csv" \
        baseline_death_deaths_from_covid_med \
        total_deaths \
        "output/21_07_16_rhogammavariable/lebanon_mse_row${i}_s2021_n10000.csv"
done

# Process MSE calculations into 1 file and make figures
Rscript process_mse_output.R \
    parameters/21_07_16_lebanon_parameters_rhogammavariable.csv \
    "output/21_07_16_rhogammavariable/lebanon_mse_row*.csv" \
    "output/21_07_16_rhogammavariable" \
    "results/21_07_16_rhogammavariable_"


