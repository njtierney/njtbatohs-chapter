# Makefile for Bayesian Applications to Occupational Health Surveillance (batohs)
# Nicholas Tierney

# set parameters ---------------------------------------------------------------
nia = 1000 # number of iterations adapt
nib = 10000 # number of iterations for burnin
nim = 20000 # number of iterations for model
nthin = 20 # how much thinning
nchains = 3 # how much thinning
data_year = 2015

# tidy -------------------------------------------------------------------------
01_tidy/01_tidy_fev1.html: 01_tidy/01_tidy.Rmd
	Rscript -e 'rmarkdown::render("$<", \
	params = list(outcome = "fev1_perc", data = $(data_year)), \
	output_file = "01_tidy_fev1.html")'

01_tidy/01_tidy_bmi.html: 01_tidy/01_tidy.Rmd
	Rscript -e 'rmarkdown::render("$<", \
	params = list(outcome = "bmi", data = $(data_year) ), \
	output_file = "01_tidy_bmi.html")'

01_tidy/01_tidy_sys.html: 01_tidy/01_tidy.Rmd
	Rscript -e 'rmarkdown::render("$<", \
	params = list(outcome = "sys", data = $(data_year)), \
	output_file = "01_tidy_sys.html")'
	
01_tidy/01_tidy_dias.html: 01_tidy/01_tidy.Rmd
	Rscript -e 'rmarkdown::render("$<", \
	params = list(outcome = "dias", data = $(data_year)), \
	output_file = "01_tidy_dias.html")'

01_tidy_all: 01_tidy/01_tidy_dias.html \
01_tidy/01_tidy_sys.html \
01_tidy/01_tidy_bmi.html \
01_tidy/01_tidy_fev1.html

# fit --------------------------------------------------------------------------
02_fit/02_fit_fev1.html: 02_fit/02_fit.Rmd 01_tidy/01_tidy_fev1.html
	Rscript -e 'rmarkdown::render("$<", \
	params = list(n_iterations_adapt = $(nia), \
	n_iterations_burnin = $(nib), \
	n_iterations_model = $(nim), \
	n_thin = $(nthin), \
	n_chains = $(nchains), \
	outcome = "fev1_perc"), \
	output_file = "02_fit_fev1.html")'

#### bmi
02_fit/02_fit_bmi.html: 02_fit/02_fit.Rmd 01_tidy/01_tidy_bmi.html
	Rscript -e 'rmarkdown::render("$<", \
	params = list(n_iterations_adapt = $(nia), \
	n_iterations_burnin = $(nib), \
	n_iterations_model = $(nim), \
	n_thin = $(nthin), \
	n_chains = $(nchains), \
	outcome = "bmi"), \
	output_file = "02_fit_bmi.html")'

#### systolic blood pressure
02_fit/02_fit_sys.html: 02_fit/02_fit.Rmd 01_tidy/01_tidy_sys.html
	Rscript -e 'rmarkdown::render("$<", \
	params = list(n_iterations_adapt = $(nia), \
	n_iterations_burnin = $(nib), \
	n_iterations_model = $(nim), \
	n_thin = $(nthin), \
	n_chains = $(nchains), \
	outcome = "sys"), \
	output_file = "02_fit_sys.html")'

#### Diastolic blood pressure
02_fit/02_fit_dias.html: 02_fit/02_fit.Rmd 01_tidy/01_tidy_dias.html
	Rscript -e 'rmarkdown::render("$<", \
	params = list(n_iterations_adapt = $(nia), \
	n_iterations_burnin = $(nib), \
	n_iterations_model = $(nim), \
	n_thin = $(nthin), \
	n_chains = $(nchains), \
	outcome = "dias"), \
	output_file = "02_fit_dias.html")'

#### Fit all
02_fit_all: 02_fit/02_fit_dias.html \
02_fit/02_fit_sys.html \
02_fit/02_fit_bmi.html \
02_fit/02_fit_fev1.html

# post-process -----------------------------------------------------------------

03_post_process/03_post_process_fev1.html: 03_post_process/03_post_process.Rmd 02_fit/02_fit_fev1.html
	Rscript -e 'rmarkdown::render("$<", \
	params = list(outcome = "fev1_perc"), \
	output_file = "03_post_process_fev1.html")'

03_post_process/03_post_process_bmi.html: 03_post_process/03_post_process.Rmd 02_fit/02_fit_bmi.html
	Rscript -e 'rmarkdown::render("$<", \
	params = list(outcome = "bmi"), \
	output_file = "03_post_process_bmi.html")'

03_post_process/03_post_process_sys.html: 03_post_process/03_post_process.Rmd 02_fit/02_fit_sys.html
	Rscript -e 'rmarkdown::render("$<", \
	params = list(outcome = "sys"), \
	output_file = "03_post_process_sys.html")'

03_post_process/03_post_process_dias.html: 03_post_process/03_post_process.Rmd 02_fit/02_fit_dias.html
	Rscript -e 'rmarkdown::render("$<", \
	params = list(outcome = "dias"), \
	output_file = "03_post_process_dias.html")'
	
#### post-process-all
03_post_process_all: 03_post_process/03_post_process_dias.html \
03_post_process/03_post_process_sys.html \
03_post_process/03_post_process_bmi.html \
03_post_process/03_post_process_fev1.html

# generate paper ---------------------------------------------------------------

# make it as PDF
04_paper/04_paper_all_outcomes.pdf: 04_paper/04_paper.Rmd \
03_post_process/03_post_process_dias.html \
03_post_process/03_post_process_sys.html \
03_post_process/03_post_process_bmi.html \
03_post_process/03_post_process_fev1.html
	Rscript -e 'rmarkdown::render("$<", \
	"pdf_document", \
	params = list(n_iterations_adapt = $(nia), \
	n_iterations_burnin = $(nib), \
	n_iterations_model = $(nim), \
	n_thin = $(nthin), data = $(data_year)), \
	output_file = "04_paper_all_outcomes.pdf")'

# make it as HTML
04_paper/04_paper_all_outcomes.html: 04_paper/04_paper.Rmd \
04_paper/04_paper.Rmd 03_post_process/03_post_process_dias.html \
03_post_process/03_post_process_sys.html \
03_post_process/03_post_process_bmi.html \
03_post_process/03_post_process_fev1.html
	Rscript -e 'rmarkdown::render("$<", \
	"html_document", \
	params = list(n_iterations_adapt = $(nia), \
	n_iterations_burnin = $(nib), \
	n_iterations_model = $(nim), \
	n_thin = $(nthin), data = $(data_year)), \
	output_file = "04_paper_all_outcomes.html")'

paper: 04_paper/04_paper_all_outcomes.html \
04_paper/04_paper_all_outcomes.pdf \

# diagnostics ------------------------------------------------------------------

#### fev1
05_diagnostics/05_diagnostics_fev1.html: 05_diagnostics/05_diagnostics.Rmd 03_post_process/03_post_process_fev1.html
	Rscript -e 'rmarkdown::render("$<", \
	params = list(n_iterations_adapt = $(nia), \
	n_iterations_burnin = $(nib), \
	n_iterations_model = $(nim), \
	n_thin = $(nthin), \
	outcome = "fev1_perc"), \
	output_file = "05_diagnostics_fev1.html")'

#### bmi
05_diagnostics/05_diagnostics_bmi.html: 05_diagnostics/05_diagnostics.Rmd 03_post_process/03_post_process_bmi.html
	Rscript -e 'rmarkdown::render("$<", \
	params = list(n_iterations_adapt = $(nia), \
	n_iterations_burnin = $(nib), \
	n_iterations_model = $(nim), \
	n_thin = $(nthin), \
	outcome = "bmi"), \
	output_file = "05_diagnostics_bmi.html")'

#### sys
05_diagnostics/05_diagnostics_sys.html: 05_diagnostics/05_diagnostics.Rmd 03_post_process/03_post_process_sys.html
	Rscript -e 'rmarkdown::render("$<", \
	params = list(n_iterations_adapt = $(nia), \
	n_iterations_burnin = $(nib), \
	n_iterations_model = $(nim), \
	n_thin = $(nthin), \
	outcome = "sys"), \
	output_file = "05_diagnostics_sys.html")'

#### dias
05_diagnostics/05_diagnostics_dias.html: 05_diagnostics/05_diagnostics.Rmd 03_post_process/03_post_process_dias.html
	Rscript -e 'rmarkdown::render("$<", \
	params = list(n_iterations_adapt = $(nia), \
	n_iterations_burnin = $(nib), \
	n_iterations_model = $(nim), \
	n_thin = $(nthin), \
	outcome = "dias"), \
	output_file = "05_diagnostics_dias.html")'
	
#### all diagnostics
05_diagnostics_all: 05_diagnostics/05_diagnostics_dias.html \
05_diagnostics/05_diagnostics_sys.html \
05_diagnostics/05_diagnostics_bmi.html \
05_diagnostics/05_diagnostics_fev1.html 

# make the whole paper ---------------------------------------------------------
all: 04_paper/04_paper_all_outcomes.html \
05_diagnostics/05_diagnostics_dias.html \
05_diagnostics/05_diagnostics_sys.html \
05_diagnostics/05_diagnostics_bmi.html \
05_diagnostics/05_diagnostics_fev1.html 

# make just fev1, (for testing purposes) ---------------------------------------
all_fev1: 05_diagnostics/05_diagnostics_fev1.html \
03_post_process/03_post_process_fev1.html \
02_fit/02_fit_fev1.html \
01_tidy/01_tidy_fev1.html

# clean ------------------------------------------------------------------------
clean:
	rm -rf outputs/*
	rm -rf */*_cache
	rm -rf */*_files
	rm -f */*.md
	rm -f */*.tex

# clobber ----------------------------------------------------------------------
clobber: 
	rm -rf */*.html
	rm -f 04_paper/*.pdf

# Extras -----------------------------------------------------------------------

# which files are not files in the literal sense?	
.PHONY: all clean clobber



