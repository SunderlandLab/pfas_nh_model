# Logistic regression
# Library -----------------------------------------------------------------

library(tidyverse)
library(caret)
library(MASS)

# Load --------------------------------------------------------------------

compounds_data <- readRDS('../../modeling_data/compounds_data01232021.rds')
compounds <- names(compounds_data)
level_key <- c("final" = "final",
              "ImpactPlastics" = "Industry: Plastics and rubber", 
               "recharge" = "Hydro: Groundwater recharge",
               "precip" = "Hydro: Monthly precipitation",
               "ImpactTextile" = "Industry: Textiles manufacturing",
               "silttotal_r" = "Soil: Percent total silt",
               "cec7_r" = "Soil: Cation exchange capacity",
               "claytotal_r" = "Soil: Percent total clay",
               "slopegradwta" = "Hydro: Slope gradient",
               #"ImpactPr" = "Industry: Printing industry",
               "soc0_999" = "Soil: Organic carbon",
               "dbthirdbar_r" = "Soil: Bulk density",
               "awc_r" = "Soil: Available water capacity",
               "ImpactOI" = "Industry: Other",
               "ImpactAirports" = "Industry: Airports",
               "ImpactWWTP" = "Industry: Wastewater treatment plant",
               "ImpactMilitary" = "Industry: Military AFFF",
               #"ImpactM" = "Industry: Metal plating",
               "hzdep" = "Soil: Thickness of soil horizon",
               "bedrock_M" = "Geo: Bedrock type",
               "hydgrpdcdA" = "Hydro: Low runoff potential",
               #"ImpactS" = "Industry: Semiconductor manufacturing",
               "wtdepannmin" = "Hydro: Depth to water table",
               "brockdepmin" = "Geo: Depth to bedrock",
               "sandtotal_r" = "Soil: Percent total sand",
               "ksat_r" = "Soil: Saturated hydraulic conductivity")
# use full dataset to fit model
## set up empty list
compounds_logreg <- list()

for(comp in compounds) {
  # remove stationID and continuous outcome
  data <- compounds_data[[comp]] %>%
    dplyr::select(-c(reg))
  # rename variables for better output
  
  data <- data %>% 
    mutate_if(is.factor, as.numeric) %>%
    pivot_longer(-StationID, names_to = "variable", values_to = "value") %>%
    mutate(variable = recode(variable, !!!level_key)) %>%
    pivot_wider(names_from = "variable", values_from = "value") %>%
    mutate(final = as.factor(final),
           `Geo: Bedrock type` = as.factor(`Geo: Bedrock type`),
           `Hydro: Low runoff potential` = as.factor(`Hydro: Low runoff potential`)) %>%
    dplyr::select(-StationID)
  # Logistic regression
  set.seed(123)
  fit <- glm(final~., data = data, 
             family = binomial) %>% 
    stepAIC(trace = FALSE, direction = "both")
  compounds_logreg[[comp]] <- 
    list(model = fit)
}

saveRDS(compounds_logreg, '../../models/compounds_logreg_02052021.rds')

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

# compounds_logreg_alt <- map(compounds_data, function(data) {
#   set.seed(123)
#   #set.seed(123)
#   # drop station ID, reg
#   data<-data %>%
#     dplyr::select(-c(StationID, reg))
#   # Divide into training and test sets, 70-30%
#   p <- sample(nrow(data),floor(0.7*nrow(data)))
#   train_data <- data[p, ]
#   test_data <- data[-p, ]
#   # Save model after stepwise regression
#   model <- glm(final ~ ., data = train_data, family = binomial(link = "logit")) %>% 
#     stepAIC(trace = FALSE, direction = "both")
#   # Predict classes
#   probabilities <- model %>% predict(test_data, type = "response")
#   #plotROC(test_data$final, probabilities)
#   optCutOff <- optimalCutoff(test_data$final, probabilities)[1] 
#   #print(optCutOff)
#   #sensitivity(test_data$final, probabilities, threshold = optCutOff)
#   #specificity(test_data$final, probabilities, threshold = optCutOff)
#   #print(DescTools::VIF(model))
#   predicted_classes <- ifelse(probabilities > optCutOff, 1, 0)
#   
#   return(list(model, train_data, test_data, predicted_classes) %>% 
#            set_names('model', 'train_data', 'test_data', 'predicted_classes'))
# })

# Save --------------------------------------------------------------------
saveRDS(compounds_glm, '../../models/compounds_glm_02022021.rds')

# colocation of industries

# M<-compounds_data[[1]] %>%
#   dplyr::select(starts_with("Impact")) %>%
#   cor()
# corrplot::corrplot(M, method = "number", type = "lower", diag = F)
