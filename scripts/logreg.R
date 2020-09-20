# Logistic regression
# Library -----------------------------------------------------------------

library(tidyverse)
library(caret)
library(MASS)

# Load --------------------------------------------------------------------

compounds_data <- readRDS('modeling_data/compounds_data.rds')


# Model -------------------------------------------------------------------
# (Note: will have to revisit code if cross validation implemented)

# A list of 5 lists (1 per compound)
# - Each list contains 1) model object; 2) training data; 3) test data; 4) predicted classes
compounds_logreg <- map(compounds_data, function(data) {
  set.seed(123)
  # Divide into training and test sets, 70-30%
  train_samples <- data$final %>% createDataPartition(p = 0.7, list = F)
  train_data <- data[train_samples, ]
  test_data <- data[-train_samples, ]
  # Save model after stepwise regression, remove 'StationID' & 'reg' columns before fit
  model <- glm(final ~ ., data = train_data[,-c(1:2)], family = binomial) %>% 
    stepAIC(trace = FALSE, direction = "both")
  # Predict classes
  probabilities <- model %>% predict(test_data, type = "response")
  predicted_classes <- ifelse(probabilities > 0.5, 1, 0)
  
  return(list(model, train_data, test_data, predicted_classes) %>% 
           set_names('model', 'train_data', 'test_data', 'predicted_classes'))
})


# Save --------------------------------------------------------------------

saveRDS(compounds_logreg, 'models/compounds_logreg.rds')



