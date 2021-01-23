# Logistic regression
# Library -----------------------------------------------------------------

library(tidyverse)
library(caret)
library(MASS)
library(InformationValue)
# Load --------------------------------------------------------------------

compounds_data <- readRDS('../../modeling_data/compounds_data01192021.rds')


# Model -------------------------------------------------------------------
# (Note: will have to revisit code if cross validation implemented)

# A list of 5 lists (1 per compound)
# - Each list contains 1) model object; 2) training data; 3) test data; 4) predicted classes

compounds_logreg_alt <- map(compounds_data, function(data) {
  set.seed(123)
  #set.seed(123)
  # drop station ID, reg
  data<-data %>%
    dplyr::select(-c(StationID, reg)) #%>%
    #mutate(ImpactOI = ImpactOI + ImpactS + ImpactPr) %>%
    #dplyr::select(-c(ImpactS, ImpactPr))
    #add in risk score from Guelfo et al EHP 2018 https://ehp.niehs.nih.gov/doi/full/10.1289/EHP2727
    # mutate(ImpactS = 50*ImpactS,
    #        ImpactPr = 50*ImpactPr,
    #        ImpactOI = 25*ImpactOI,
    #        ImpactM = 25*ImpactM,
    #        ImpactW = 100*ImpactW,
    #        ImpactA = 75*ImpactA,
    #        ImpactAFFF = 100*ImpactAFFF,
    #        ImpactPl = 50*ImpactPl,
    #        ImpactT = 50*ImpactT)
  # Divide into training and test sets, 70-30%
  p <- sample(nrow(data),floor(0.7*nrow(data)))
  train_data <- data[p, ]
  test_data <- data[-p, ]
  # Save model after stepwise regression
  model <- glm(final ~ ., data = train_data, family = binomial(link = "logit")) %>% 
    stepAIC(trace = FALSE, direction = "both")
  # Predict classes
  probabilities <- model %>% predict(test_data, type = "response")
  #plotROC(test_data$final, probabilities)
  optCutOff <- optimalCutoff(test_data$final, probabilities)[1] 
  #print(optCutOff)
  #sensitivity(test_data$final, probabilities, threshold = optCutOff)
  #specificity(test_data$final, probabilities, threshold = optCutOff)
  #print(DescTools::VIF(model))
  predicted_classes <- ifelse(probabilities > optCutOff, 1, 0)
  
  return(list(model, train_data, test_data, predicted_classes) %>% 
           set_names('model', 'train_data', 'test_data', 'predicted_classes'))
})

# Save --------------------------------------------------------------------
saveRDS(compounds_logreg_alt, '../../models/compounds_logreg_01192021.rds')

# colocation of industries

M<-compounds_data[[1]] %>%
  dplyr::select(starts_with("Impact")) %>%
  cor()
corrplot::corrplot(M, method = "number", type = "lower", diag = F)
