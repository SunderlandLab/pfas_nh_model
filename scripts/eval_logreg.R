# Evaluate logistic regression models
# Library -----------------------------------------------------------------

library(tidyverse)
library(broom)
library(stargazer)
# Load --------------------------------------------------------------------

compounds_data <- readRDS('../../modeling_data/compounds_data1207.rds')
compounds_logreg_alt <- readRDS('../../models/compounds_logreg_alt1207.rds')

# Evaluate models ---------------------------------------------------------

#chu: refactored on 9/20 to avoid hardcoding
# Define independent variables 
# ivs <- list(PFOA = c("sandvc_r","sandvf_r", "siltfine_r",
#                      "claytotal_r","awc_r", "slopegradwta" ,"brockdepmin", "bedrock_M" ,
#                      "hydgrpdcdB" , "hydgrpdcdC"),
#             PFHXA = c("ImpactOI2","ImpactPl2","ImpactPr2","ImpactS2","sandvc_r","sandvf_r",
#                       "siltfine_r","claytotal_r","cec7_r","soc0_999","brockdepmin","bedrock_M",
#                       "drclassdcdW","drclassdcdP","hydgrpdcdC"),
#             PFPEA = c( "ImpactPl2","ImpactPr2","sandvc_r", 
#                        "siltfine_r","awc_r","wtdepannmin","bedrock_M",
#                        "drclassdcdW","drclassdcdP","hydgrpdcdB","hydgrpdcdC"),
#             PFHPA = c("precip","ImpactOI2","ImpactPl2","ImpactPr2","sandvc_r","siltfine_r",
#                       "awc_r","brockdepmin", "wtdepannmin","hzdep","bedrock_M","drclassdcdW","drclassdcdP",
#                       "hydgrpdcdC"),
#             PFOS = c("precip","ImpactAW2","ImpactPr2","siltfine_r",
#                      "claytotal_r","dbthirdbar_r", "awc_r","brockdepmin", 
#                      "drclassdcdW", "drclassdcdP","hydgrpdcdB"))

# Calculate prediction probabilities and AIC for models with a single IV removed
# eval_models <- list()
# for (i in 1:length(compounds_logreg)) {
#   clist <- compounds_logreg[[i]]
#   ivs <- clist$model$coefficients[-1]%>%
#     names()%>% #-1 because the first coef is intercept
#     gsub("1$", "", .)#some variable has 1 at the end of the variable name, drop the 1
# 
#   probs = aics <- rep(0, length(ivs))
#   for (j in 1:length(ivs)) {
#     form <- paste("final ~", paste(ivs[-j], collapse = " + ")) %>% as.formula
#     model <- glm(form, data = clist[['train_data']], family = binomial)
#     probabilities <- model %>% predict(clist[['test_data']], type = "response")
#     predicted_classes <- ifelse(probabilities > 0.5, 1, 0)
#     probs[j] <- mean(predicted_classes == clist[['test_data']]$final)
#     aics[j] <- AIC(model)
#   }
#   df <- data.frame(ivs = ivs, aics, probs)
#   eval_models[[names(compounds_logreg)[i]]] <- df[order(-df$aics),]
# }


# Sensitivity and specificity analysis ------------------------------------


sens_spec_tables_alt<- map(compounds_logreg_alt, function(clist) {
  predicted_classes <- clist[['predicted_classes']]
  observed_classes <- clist[['test_data']]$final
  return(table(predicted_classes, observed_classes))
})

calc_model_performance<-function(x) {
  
  #input is a two by two table where observed classes are listed horizontally
  #and predicted classes are listed vertically
  
  accuracy <- (x[1,1] + x[2,2])/sum(x)
  spec <- x[1,1]/(x[1,1] + x[2,1])
  sens <- x[2,2]/(x[2,2] + x[1,2])
  
  return(list(accuracy = accuracy,
              spec = spec,
              sens = sens))
}


map_df(sens_spec_tables_alt, calc_model_performance, .id = "compound")%>%
  write_csv("../../output/sens_spec_alt_logreg_10082020.csv")
#chu: 09/20/2020, refactor to combine 5 compounds in one table
# # Extract betas and write to csv -----------------------------------------
# for (i in 1:length(compounds_logreg)) {
#   capture.output(coef(compounds_logreg[[i]][['model']]), 
#                  file = paste0('../../models_coef/', names(compounds_logreg)[i], 'betas.csv'))
# }

stargazer2 <- function(model, odd.ratio = F, ...) {
  if(!("list" %in% class(model))) model <- list(model)
  
  if (odd.ratio) {
    coefOR2 <- lapply(model, function(x) exp(coef(x))-1) # -1 to convert OR to % less
    seOR2 <- lapply(model, function(x) exp(coef(x)) * summary(x)$coef[, 2])
    p2 <- lapply(model, function(x) summary(x)$coefficients[, 4])
    stargazer(model, coef = coefOR2, se = seOR2, p = p2, ...)
    
  } else {
    stargazer(model, ...)
  }
}



# Impact characterized as the count of industry with exponential decay
c_stat_ls<-sapply(compounds_logreg_alt, 
                  function(x){DescTools::Cstat(x[["model"]])%>%
                      round(3)})
stargazer2(lapply(compounds_logreg_alt, function(x){x[["model"]]}), 
           odd.ratio = T, title="Industry impact calculated using counts",
           align=TRUE, type="text",
           column.labels=c("PFOA","PFHxA","PFPeA","PFHpA","PFOS"),
           model.numbers=FALSE, keep.stat=c("n","aic"),
           add.lines = list(c("C-Statistics", c_stat_ls)),
           dep.var.labels.include = FALSE, dep.var.caption="",
           star.char = c("*", "**", "***"),
           star.cutoffs = c(.05, .01, .001),
           out="../../output/logmodel_alt_100820.html")



# # Check for influential values --------------------------------------------
# for (compound in names(compounds_data)) {
#   comp_folder <- paste0("../../models_logreg_eval/", compound, '/')
#   dir.create(comp_folder)
#   # Cooks distance graph
#   jpeg(paste0(comp_folder, compound, '_cooks.jpg'))
#   plot(compounds_logreg[[compound]][['model']], which = 4, id.n = 3)
#   dev.off()
#   
#   # Get top 3 
#   model.data <- augment(compounds_logreg[[compound]][['model']]) %>% mutate(index = 1:n())
#   write.csv(model.data %>% top_n(3, .cooksd),
#             file = paste0(comp_folder, compound, '_top3.csv'))
#   
#   # Plot index by std.resid
#   plt <- ggplot(model.data, aes(index, .std.resid)) + 
#     geom_point(aes(color = final), alpha = .5) + 
#     theme_bw() + 
#     ggtitle(compound)
#   ggsave(paste0(comp_folder, compound, '_stdresid.jpg'), plt)
#   
#   # Write observations with .std.resid > 3 to resid if exist
#   g3 <- model.data %>% filter(abs(.std.resid) > 3)
#   if (nrow(g3) > 0) {
#     write.csv(g3, file = paste0(comp_folder, compound, '_stdresid3.csv'))
#   }
# }

# Save --------------------------------------------------------------------

# saveRDS(eval_models, '../../models_logreg_eval/eval_models.rds')
# saveRDS(sens_spec_tables, '../../models_logreg_eval/sens_spec_tables.rds')
