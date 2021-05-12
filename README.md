# Modeling PFAS contamination in private wells
## Introduction
Source code for the paper:
A Statistical Approach for Identifying Private Wells Susceptible to Perfluoroalkyl Substances (PFAS)Contamination.
Environmental Science & Technology Letters Article ASAP
[DOI: 10.1021/acs.estlett.1c00264](https://pubs.acs.org/doi/10.1021/acs.estlett.1c00264)

## Authors
* Xindi C. Hu
* Beverly Ge
* Bridger Ruyle
* Jennifer Sun
* Elsie M. Sunderland

##  Order and purpose of scripts (output given in parentheses)
All can be found in the /scripts folder
### Data cleaning and processing
- read_csv.R   : read raw datasets with slight formatting/removal of extreme values
								 (modeling_data/PFASwells1.rds)
- recode.R     : recode detects/results, add binary variable, merge two main data files
						 		 (modeling_data/PFASwells.rds)
- soildata.R   : process soil data 
							 	 (modeling_data/final_soildata.rds)
- point_sources_ucmr.R   : business extraction 
							 	 (modeling_data/final_industries.rds)
- impact.py    : quantify and aggregate impact from industries
                                (modeling_data/potential_impact_huc12.csv)
- finalize_iv.R: merge all independent variables into single dataset
                 (modeling_data/merged_variables.rds, modeling_data/unique_ivs.rds)  
- finalize.R   : finalize data for 5 compounds and sumPFAS as separate dataframes, contained in a list
                 (modeling_data/compounds_data.rds)

### Modeling
- logreg.R     : logistic regression
                 (models/compounds_logreg.rds, models/compounds_glm.rds)
- eval_logreg.R: evaluate logistic regression models
- trees.R      : random forest
                 (models/compounds_forest.rds)
- eval_forest.R: evaluate random forests

### Necessary packages
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
- ggthemes
- tmap
- sf
- viridis
- broom
- stargazer
- rlang
- docxtools
- flextable
- kabelExtra
- geojsonR
- geojsonsf
- pROC





