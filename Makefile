.PHONY: all features fit evaluate

all: features fit evaluate

# Response features
features: scripts/01_create_response_features.R
	Rscript scripts/01_create_response_features.R

# Model fit
fit: scripts/02_fit_model.Rmd data/response_features.fst
	Rscript -e "rmarkdown::render('scripts/02_fit_model.Rmd', output_format = 'all', output_dir = 'output')"

	
# Model evaluation
evaluate: scripts/03_evaluate_model.Rmd data/model_fits.csv
	Rscript -e "rmarkdown::render('scripts/03_evaluate_model.Rmd', output_format = 'all', output_dir = 'output')"