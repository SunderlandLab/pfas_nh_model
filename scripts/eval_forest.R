# Evaluate random forests
# Library -----------------------------------------------------------------

library(tree)
library(randomForest)
library(caret)
library(tidyverse)
library(reshape2)
options(scipen=999)
# Load --------------------------------------------------------------------

compounds_forest <- readRDS('../../models/compounds_forest1207.rds')
compounds_data <- readRDS('../../modeling_data/compounds_data1207.rds')



# Tune model --------------------------------------------------------------
# Determine which combo minimizes error and maximizes predictive accuracy
# DO NOT RUN IT HERE!

compounds <- names(compounds_forest)

tuned_models <- list()
for (comp in compounds) {
  ntrees <- 1000
  form <- as.formula("final~.")
  num_vars <- ncol(compounds_forest[[comp]][['train_data']])
  predictions_table <- data.frame("mtry" = rep(0,(num_vars - 4)*10), 
                                  "nodesize" = rep(0,(num_vars - 4)*10), 
                                  "predictive accuracy" = rep(0,(num_vars - 4)*10))
  error_table <- data.frame("tree.index" = c(1:ntrees))
  tracker <- 1
  for (i in 1:2) {
    for (j in 1:2) { 
      mt <- i + 4 # Because starting at 5, not 1
      model <- randomForest(form, 
                            data = compounds_forest[[comp]][['train_data']], 
                            mtry = mt, 
                            ntree = ntrees, 
                            nodesize = j, 
                            importance = TRUE)
      error_table <- cbind(error_table, model$err.rate[,1])
      col_name <- paste0("m",i,"ns",j)
      print(col_name)
      names(error_table)[tracker + 1] <- col_name
      predictions <- model %>% predict(compounds_forest[[comp]][['test_data']], type = "class")
      predictions_table[tracker, 1] <- mt
      predictions_table[tracker, 2] <- j
      predictions_table[tracker, 3] <- mean(predictions == compounds_forest[[comp]][['test_data']]$final)
      print(tracker)
      tracker <- tracker + 1
    }
    tuned_models[[comp]] <- list(predictions_table = predictions_table, 
                   error_table = error_table)
  }
}

  


# Plot regression forests -------------------------------------------------

for (compound in compounds) {
  comp_folder <- paste0("../../models_forest_eval/", compound, '/')
  dir.create(comp_folder)
  jpeg(paste0(comp_folder, compound, '_fplot.jpg'))
  plot(compounds_forest[[compound]][['reg_forest']],
       main = paste(compound, 'Regression Random Forest'))
  dev.off()
}


# Sensitivity and specificity analysis ------------------------------------

sens_spec_tablesf <- map(compounds_forest, function(clist) {
  predicted_classes <- clist[['predictions']]
  observed_classes <- clist[['test_data']]$final
  print(mean(predicted_classes == observed_classes))
  return(table(predicted_classes, observed_classes))
})

map_df(sens_spec_tablesf, calc_model_performance, .id = "compound")%>%
  write_csv("../../output/sens_spec_alt_rf_10082020.csv")


# Variable Importance Plots
variable_names<- compounds_forest$PFOA$train_data%>%
  dplyr::select(-final)%>%
  colnames()

var_imp_df<-map_df(compounds_forest, function(x){importance(x[['forest']], type = 1, scale = FALSE)})
var_imp_df$variable <- variable_names  

#plot variable importance
var_imp_df%>%
  pivot_longer(-variable) %>%
  ggplot(aes(x= reorder(variable, value), y=value, fill = value)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient(low = "yellow", high = "darkblue")  +
  coord_flip() +
  facet_wrap(vars(name)) +
  theme_bw()+
  theme(text = element_text(size=16))+
  xlab("")+
  ylab("Mean Decrease in Accuracy")
ggsave("../../output/rf_class_var_imp.png")

# Save --------------------------------------------------------------------

saveRDS(tuned_models, '../../models_forest_eval/tuned_models1207.rds')
saveRDS(sens_spec_tablesf, '../../models_forest_eval/sens_spec_tablesf1207.rds')


