

test_run:
	Rscript como_command_line.R \
		"United Kingdom of Great Britain" \
		"tests/data/Template_CoMoCOVID-19App_v17.xlsx" \
		"tests/COVID19_CLI_Data_Template_CoMoCOVID-19App_v17.csv"

test_run_variant:
	Rscript como_command_line.R \
		"United Kingdom of Great Britain" \
		"tests/data/Template_CoMoCOVID-19App_v17_variant.xlsx" \
		"tests/COVID19_CLI_Data_Template_CoMoCOVID-19App_v17_variant.csv"

test_plot:
	Rscript como_plot_output.R \
		"output/test_output.csv" \
		"output/figures/test_figure.png"


###############################
# ABC using the EasyABC package
# -----------------------------

plot_easyabc:
	Rscript R/plot_easyabc_params.R \
		ABC_output.Rdata \
		"ABC_Lenormand" \
		tests/data/COVID19_App_Data_Template_CoMoCOVID-19App_v17_p0.0245.csv \
		results/figures/test_abc.png

