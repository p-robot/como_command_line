

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
