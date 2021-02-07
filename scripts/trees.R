# Random forest
# Library -----------------------------------------------------------------

library(tree)
library(randomForest)
library(caret)
library(tidyverse)
library(reshape2)
library(pROC)

# Load --------------------------------------------------------------------

compounds_data <- readRDS('../../modeling_data/compounds_data01232021.rds')
#compounds_logreg_alt <- readRDS('../../models/compounds_logreg_01232021.rds')

compounds <- names(compounds_data)
parameters <- data.frame(compound = c("PFOA","PFHXA","PFPEA","PFHPA","PFOS", "PFAS"),
                         ntree = c(1000, 1000, 1000, 1000, 1000, 1000),
                         mtry = c(10, 16, 6, 10, 22, 17),
                         nodesize = c(2, 9, 4, 4, 5, 6))

reg_parameters <- data.frame(compound = c("PFOA","PFHXA","PFPEA","PFHPA","PFOS", "PFAS"),
                            ntree = c(500, 500, 500, 500, 500, 500),
                            mtry = c(5, 5, 9, 5, 6, 5),
                            nodesize = c(2, 9, 5, 5, 4, 3))


compounds_forest <- list()
for (comp in compounds) {
  # remove stationID and continous outcome
  data <- compounds_data[[comp]] %>%
    dplyr::select(-c(StationID, reg))
  # Forest 1: Classification
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
    fit <- randomForest(final~., data=data[-folds[[v]],], 
                        ntree = parameters[parameters$compound == comp, 'ntree'],
                        mtry = parameters[parameters$compound == comp, 'mtry'], 
                        nodesize = parameters[parameters$compound == comp, 'nodesize'], 
                        importance = TRUE)
    pred <- predict(fit, newdata=data[folds[[v]],], type="prob")[,2] #keep the probability of Y = 1
    return(pred)
  }
  # create folds
  folds <- .cvFolds(Y=data$final, V=10)
  #CV train/predict
  rf.predictions <- sapply(seq(10), .doFit, folds=folds, data=data) 
  rf.labels <- sapply(folds, function(x){data$final[x]})
  # apply prediction function from RORC package
  pred.rf <- prediction(rf.predictions, rf.labels)
  perf.rf <- performance(pred.rf, 'tpr', 'fpr')
  # calculate the AUC for the 10 ROC curves
  auc <- performance(pred.rf, "auc")
  mean_auc <- auc@y.values %>% unlist() %>% mean()
  se_auc <- auc@y.values %>% unlist() %>% sd() / sqrt(10)
  # 95% CI
  auc_lb <- mean_auc - 1.96 * se_auc
  auc_ub <- mean_auc + 1.96 * se_auc
  # random forest classification model
  forest <- randomForest(final~., data = data,
                         ntree = reg_parameters[reg_parameters$compound == comp, 'ntree'],
                         mtry = reg_parameters[reg_parameters$compound == comp, 'mtry'],
                         nodesize = reg_parameters[reg_parameters$compound == comp, 'nodesize'],
                         importance = TRUE)
  # 
  # outbag_predictions <- reg_forest %>% predict(reg_test)
  # test.err <- mean((reg_test$reg_log - outbag_predictions)^2)
  # # test.err <- with(reg_test, mean((log(reg_test$reg) - outbag_predictions)^2))
  # test.rsq <- 1-(test.err/var(reg_test$reg_log))
  
  compounds_forest[[comp]] <- 
    list(forest = forest, 
         #reg_forest = reg_forest, 
         #train_data = clist[['train_data']], 
         #test_data = clist[['test_data']], 
         perf.rf = perf.rf,
         mean_auc = mean_auc,
         auc_lb = auc_lb,
         auc_ub = auc_ub)
         # outbag_predictions = outbag_predictions,
         # test_err = test.err,
         # test.rsq = test.rsq)
}



# Save --------------------------------------------------------------------

saveRDS(compounds_forest, '../../models/compounds_forest02022021.rds')




