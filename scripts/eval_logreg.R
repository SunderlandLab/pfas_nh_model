# Evaluate logistic regression models
# Library -----------------------------------------------------------------

library(tidyverse)
library(broom)

# Load --------------------------------------------------------------------

compounds_data <- readRDS('modeling_data/compounds_data.rds')
compounds_logreg <- readRDS('models/compounds_logreg.rds')


# Evaluate models ---------------------------------------------------------

# Define independent variables 
ivs <- list(PFOA = c("sandvc_r","sandvf_r", "siltfine_r",
                     "claytotal_r","awc_r", "slopegradwta" ,"brockdepmin", "bedrock_M" ,
                     "hydgrpdcdB" , "hydgrpdcdC"),
            PFHXA = c("ImpactOI2","ImpactPl2","ImpactPr2","ImpactS2","sandvc_r","sandvf_r",
                      "siltfine_r","claytotal_r","cec7_r","soc0_999","brockdepmin","bedrock_M",
                      "drclassdcdW","drclassdcdP","hydgrpdcdC"),
            PFPEA = c( "ImpactPl2","ImpactPr2","sandvc_r", 
                       "siltfine_r","awc_r","wtdepannmin","bedrock_M",
                       "drclassdcdW","drclassdcdP","hydgrpdcdB","hydgrpdcdC"),
            PFHPA = c("precip","ImpactOI2","ImpactPl2","ImpactPr2","sandvc_r","siltfine_r",
                      "awc_r","brockdepmin", "wtdepannmin","hzdep","bedrock_M","drclassdcdW","drclassdcdP",
                      "hydgrpdcdC"),
            PFOS = c("precip","ImpactAW2","ImpactPr2","siltfine_r",
                     "claytotal_r","dbthirdbar_r", "awc_r","brockdepmin", 
                     "drclassdcdW", "drclassdcdP","hydgrpdcdB"))

# Calculate prediction probabilities and AIC for models with a single IV removed
eval_models <- list()
for (i in 1:length(compounds_logreg)) {
  clist <- compounds_logreg[[i]]
  probs = aics <- rep(0, length(ivs[[i]]))
  for (j in 1:length(ivs[[i]])) {
    form <- paste("final ~", paste(ivs[[i]][-j], collapse = " + ")) %>% as.formula
    # Remove 'StationID' and 'reg' columns before fitting
    model <- glm(form, data = clist[['train_data']][,-c(1:2)], family = binomial)
    probabilities <- model %>% predict(clist[['test_data']], type = "response")
    predicted_classes <- ifelse(probabilities > 0.5, 1, 0)
    probs[j] <- mean(predicted_classes == clist[['test_data']]$final)
    aics[j] <- AIC(model)
  }
  df <- data.frame(ivs = ivs[[i]], aics, probs)
  eval_models[[names(compounds_logreg)[i]]] <- df[order(-df$aics),]
}


# Sensitivity and specificity analysis ------------------------------------

sens_spec_tables <- map(compounds_logreg, function(clist) {
  predicted_classes <- clist[['predicted_classes']]
  observed_classes <- clist[['test_data']]$final
  return(table(predicted_classes, observed_classes))
})


# Extract beta’s and write to csv -----------------------------------------
for (i in 1:length(compounds_logreg)) {
  capture.output(coef(compounds_logreg[[i]][['model']]), 
                 file = paste0('models_coef/', names(compounds_logreg)[i], 'betas.csv'))
}


# Check for influential values --------------------------------------------
for (compound in names(compounds_data)) {
  comp_folder <- paste0("models_logreg_eval/", compound, '/')
  dir.create(comp_folder)
  # Cooks distance graph
  jpeg(paste0(comp_folder, compound, '_cooks.jpg'))
  plot(compounds_logreg[[compound]][['model']], which = 4, id.n = 3)
  dev.off()
  
  # Get top 3 
  model.data <- augment(compounds_logreg[[compound]][['model']]) %>% mutate(index = 1:n())
  write.csv(model.data %>% top_n(3, .cooksd),
            file = paste0(comp_folder, compound, '_top3.csv'))
  
  # Plot index by std.resid
  plt <- ggplot(model.data, aes(index, .std.resid)) + 
    geom_point(aes(color = final), alpha = .5) + 
    theme_bw() + 
    ggtitle(compound)
  ggsave(paste0(comp_folder, compound, '_stdresid.jpg'), plt)
  
  # Write observations with .std.resid > 3 to resid if exist
  g3 <- model.data %>% filter(abs(.std.resid) > 3)
  if (nrow(g3) > 0) {
    write.csv(g3, file = paste0(comp_folder, compound, '_stdresid3.csv'))
  }
}

# Save --------------------------------------------------------------------

saveRDS(eval_models, 'models_logreg_eval/eval_models.rds')
saveRDS(sens_spec_tables, 'models_logreg_eval/sens_spec_tables.rds')
