# Logistic regression
# Library -----------------------------------------------------------------

library(tidyverse)
library(caret)
library(MASS)
library(InformationValue)
# Load --------------------------------------------------------------------

compounds_data <- readRDS('../../modeling_data/compounds_data11162020.rds')


# Model -------------------------------------------------------------------
# (Note: will have to revisit code if cross validation implemented)

# A list of 5 lists (1 per compound)
# - Each list contains 1) model object; 2) training data; 3) test data; 4) predicted classes

compounds_logreg_alt <- map(compounds_data, function(data) {
  set.seed(99)
  #set.seed(123)
  # drop station ID, reg, and impact3 (counts)
  data<-data %>%
    dplyr::select(-c(StationID, reg, matches("2$")))
  # Divide into training and test sets, 70-30%
  p <- sample(nrow(data),floor(0.7*nrow(data)))
  #p <- data$final %>% createDataPartition(p = 0.7, list = F)
  train_data <- data[p, ]
  test_data <- data[-p, ]
  #train_data <- data[p, -c(6:11)] for env factors only
  #test_data <- data[-p,-c(6:11) ]
  # Save model after stepwise regression, remove 'StationID' & 'reg' columns before fit
  model <- glm(final ~ ., data = train_data, family = binomial(link = "logit")) %>% 
    stepAIC(trace = FALSE, direction = "both")
  # Predict classes
  probabilities <- model %>% predict(test_data, type = "response")
  #plotROC(test_data$final, probabilities)
  optCutOff <- optimalCutoff(test_data$final, probabilities)[1] 
  #print(optCutOff)
  #sensitivity(test_data$final, probabilities, threshold = optCutOff)
  #specificity(test_data$final, probabilities, threshold = optCutOff)
  print(DescTools::VIF(model))
  predicted_classes <- ifelse(probabilities > optCutOff, 1, 0)
  
  return(list(model, train_data, test_data, predicted_classes) %>% 
           set_names('model', 'train_data', 'test_data', 'predicted_classes'))
})

# Save --------------------------------------------------------------------
saveRDS(compounds_logreg_alt, '../../models/compounds_logreg_alt11162020.rds')
