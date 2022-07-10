# Project name
NAME ?= "Variable rebombination rates ABC-RF analysis"

# Run all steps at once for model dianostic
all:
	Rscript rf_analysis_recombpods.R

# Make plots
fig:
	Rscript rf_oobplots_recombpods.R

# clean files
clean:
	rm -r posteriors/*/
	rm -r posteriors/

# clean only final plot
cleanfig:
	rm posteriors/posterior_plots.pdf
