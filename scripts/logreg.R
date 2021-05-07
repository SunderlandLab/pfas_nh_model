# Logistic regression
# Library -----------------------------------------------------------------

library(tidyverse)
library(caret)
library(MASS)
library(rlang)

# Load --------------------------------------------------------------------

compounds_data <- readRDS('../../modeling_data/compounds_data.rds')
compounds <- c("PFPEA", "PFHXA", "PFHPA", "PFOA", "PFOS",  "PFAS") 
level_key <- c("final" = "final",
               "Industry: Plastics and rubber" = "ImpactPlastics",
               "Hydro: Groundwater recharge" = "recharge",
               "Hydro: Monthly precipitation" = "precip",
               "Industry: Textiles manufacturing" = "ImpactTextile",
               "Soil: Percent total silt" = "silttotal_r",
               "Soil: Cation exchange capacity" = "cec7_r",
               "Soil: Percent total clay" = "claytotal_r",
               "Hydro: Slope gradient" = "slopegradwta",
               "Soil: Organic carbon" ='soc0_999',
               "Soil: Bulk density" = "dbthirdbar_r",
               "Soil: Available water capacity" = "awc_r",
               "Industry: Other" = "ImpactOI",
               "Industry: Airports" = "ImpactAirports",
               "Industry: Wastewater treatment plant" = "ImpactWWTP",
               "Industry: Military AFFF" = "ImpactMilitary",
               "Soil: Thickness of soil horizon" = "hzdep",
               "Geo: Bedrock type" = "bedrock_M",
               "Hydro: Low runoff potential" = "hydgrpdcdA",
               "Hydro: Depth to water table" = "wtdepannmin",
               "Geo: Depth to bedrock" = "brockdepmin",
               "Soil: Percent total sand" = "sandtotal_r",
               "Soil: Saturated hydraulic conductivity" = "ksat_r")

# use full dataset to fit model
## set up empty list
compounds_logreg <- list()

for(comp in compounds) {
  # remove stationID and continuous outcome
  data <- compounds_data[[comp]] %>%
    dplyr::select(-c(reg, StationID))
  # rename variables for better output
  
  data <- data %>% 
    rename(!!level_key) %>%
    mutate(final = as.factor(final),
           `Geo: Bedrock type` = as.factor(`Geo: Bedrock type`),
           `Hydro: Low runoff potential` = as.factor(`Hydro: Low runoff potential`)) 
  # Logistic regression
  set.seed(123)
  fit <- glm(final~., data = data, 
             family = binomial) %>% 
    stepAIC(trace = FALSE, direction = "both")
  # standardized logistic regression
  std_fit <- arm::standardize(fit)
  # make sure the coefficients have the same name in both models
  names(std_fit$coefficients) <- names(fit$coefficients)
  compounds_logreg[[comp]] <- 
    list(model = fit,
         model_std = std_fit)
}

saveRDS(compounds_logreg, '../../models/compounds_logreg.rds')
 
# use 10-fold cross validation to evaluate model performance
## set up empty list
compounds_glm <- list()
for (comp in compounds) {
  # remove stationID and continous outcome
  data <- compounds_data[[comp]] %>%
         dplyr::select(-c(StationID, reg))
  # Logistic regression
  set.seed(123)
  # 10 fold validation
  # Helper functions -----------------------------------------------------------
  .cvFolds <- function(Y, V){  #Create CV folds (stratify by outcome)
    Y0 <- split(sample(which(Y==0)), rep(1:V, length=length(which(Y==0))))
    Y1 <- split(sample(which(Y==1)), rep(1:V, length=length(which(Y==1))))
    folds <- vector("list", length=V)
    for (v in seq(V)) {folds[[v]] <- c(Y0[[v]], Y1[[v]])}		
    return(folds)
  }
  
  .doFit <- function(v, folds, data){  #Train/test RF for each fold
    fit <- glm(final~., data=data[-folds[[v]],], 
               family = binomial) %>% 
      stepAIC(trace = FALSE, direction = "both")
    pred <- predict(fit, newdata=data[folds[[v]],], type="response") 
    return(pred = pred)
  }
  # create folds
  folds <- .cvFolds(Y=data$final, V=10)
  #CV train/predict
  glm.predictions <- sapply(seq(10), .doFit, folds=folds, data=data) 
  glm.labels <- sapply(folds, function(x){data$final[x]})
  # apply prediction function from RORC package
  pred.glm <- prediction(glm.predictions, glm.labels)
  perf.glm <- performance(pred.glm, 'tpr', 'fpr')
  # calculate the AUC for the 10 ROC curves
  auc <- performance(pred.glm, "auc")
  mean_auc <- auc@y.values %>% unlist() %>% mean()
  se_auc <- auc@y.values %>% unlist() %>% sd() / sqrt(10)
  # 95% CI
  auc_lb <- mean_auc - 1.96 * se_auc
  auc_ub <- mean_auc + 1.96 * se_auc
  compounds_glm[[comp]] <- 
    list(perf.glm = perf.glm,
         mean_auc = mean_auc,
         auc_lb = auc_lb,
         auc_ub = auc_ub)
}

# Save --------------------------------------------------------------------
saveRDS(compounds_glm, '../../models/compounds_glm.rds')
