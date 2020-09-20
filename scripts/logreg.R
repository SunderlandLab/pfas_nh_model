# Logistic regression
# Library -----------------------------------------------------------------

library(tidyverse)
library(caret)
library(MASS)

# Load --------------------------------------------------------------------

compounds_data <- readRDS('../../modeling_data/compounds_data.rds')


# Model -------------------------------------------------------------------
# (Note: will have to revisit code if cross validation implemented)

# A list of 5 lists (1 per compound)
# - Each list contains 1) model object; 2) training data; 3) test data; 4) predicted classes
compounds_logreg <- map(compounds_data, function(data) {
  set.seed(123)
  # drop station ID, reg, and impact3 (counts)
  data<-data %>%
    dplyr::select(-c(StationID, reg, matches("3$")))
  # Divide into training and test sets, 70-30%
  train_samples <- data$final %>% createDataPartition(p = 0.7, list = F)
  train_data <- data[train_samples, ]
  test_data <- data[-train_samples, ]
  # Save model after stepwise regression
  model <- glm(final ~ ., data = train_data, family = binomial) %>% 
    stepAIC(trace = FALSE, direction = "both")
  # Predict classes
  probabilities <- model %>% predict(test_data, type = "response")
  predicted_classes <- ifelse(probabilities > 0.5, 1, 0)
  
  return(list(model, train_data, test_data, predicted_classes) %>% 
           set_names('model', 'train_data', 'test_data', 'predicted_classes'))
})

# test an alternative modeling where industry impact is characterized by counts, not sales volumes
compounds_logreg_alt <- map(compounds_data, function(data) {
  set.seed(123)
  # drop station ID, reg, and impact2 (sales volume)
  data<-data %>%
    dplyr::select(-c(StationID, reg, matches("2$")))
  # Divide into training and test sets, 70-30%
  train_samples <- data$final %>% createDataPartition(p = 0.7, list = F)
  train_data <- data[train_samples, ]
  test_data <- data[-train_samples, ]
  # Save model after stepwise regression
  model <- glm(final ~ ., data = train_data, family = binomial) %>% 
    stepAIC(trace = FALSE, direction = "both")
  # Predict classes
  probabilities <- model %>% predict(test_data, type = "response")
  predicted_classes <- ifelse(probabilities > 0.5, 1, 0)
  
  return(list(model, train_data, test_data, predicted_classes) %>% 
           set_names('model', 'train_data', 'test_data', 'predicted_classes'))
})


# Save --------------------------------------------------------------------

saveRDS(compounds_logreg, '../../models/compounds_logreg.rds')
saveRDS(compounds_logreg_alt, '../../models/compounds_logreg_alt.rds')



