# pfas_nh_model
Predictive model for PFAS levels in private wells in NH

Order and purpose of scripts (output given in parentheses), in the /scripts folder
Data cleaning and processing
- read_csv.R   : read raw datasets with slight formatting/removal of extreme values
								 (modeling_data/PFASwells1.rds, modeling_data/pfoapfhxa.rds)
- recode.R     : recode detects/results, add binary variable, merge two main data files
						 		 (modeling_data/PFASwells.rds)
- soildata.R   : process soil data 
							 	 (modeling_data/final_soildata.rds)
- business.R   : business extraction 
							 	 (modeling_data/new_finalind.rds)
- industries.R : quantify and aggregate industries 
								 (modeling_data/final_industries.rds)
(- precip.R    : use precip_PFAS.csv)
- finalize_iv.R: merge all independent variables into single dataset
                 (modeling_data/merged_variables.rds, modeling_data/unique_ivs.rds)  
- finalize.R   : finalize data for 5 compounds as separate dataframes, contained in 1 list
                 (modeling_data/compounds_data.rds)

Modeling
- logreg.R     : logistic regression
                 (models/compounds_logreg.rds)
- eval_logreg.R: evaluate logistic regression models
                 (models_coef/, models_eval/)
- trees.R      : random forest
                 (models/compounds_forest.rds)
- eval_forest.R: evaluate random forests
                 (models_forest_eval)

Supplemental scripts
- test_temporal_variability.R
- corrplot.R (not finalized yet)
- Makefile: be sure to run "xcode-select --install" in the Terminal tab first


Necessary packages
- tidyverse
- raster
- rgdal
- maptools
- proj4
- rgeos
- corrplot
- InformationValue
- caret
- MASS
- tree
- randomForest
- reshape2
