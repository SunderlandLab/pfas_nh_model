# Evaluate logistic regression models
# Library -----------------------------------------------------------------

library(tidyverse)
library(broom)
library(stargazer)
# Load --------------------------------------------------------------------

compounds_data <- readRDS('../../modeling_data/compounds_data01232021.rds')
compounds_logreg <- readRDS('../../models/compounds_logreg_02052021.rds')
compounds_glm <- readRDS("../../models/compounds_glm_02022021.rds")

# Evaluate models ---------------------------------------------------------

# # Sensitivity and specificity analysis ------------------------------------
# 
# 
# sens_spec_tables_alt<- map(compounds_logreg_alt, function(clist) {
#   predicted_classes <- clist[['predicted_classes']]
#   observed_classes <- clist[['test_data']]$final
#   return(table(predicted_classes, observed_classes))
# })
# 
# calc_model_performance<-function(x) {
#   
#   #input is a two by two table where observed classes are listed horizontally
#   #and predicted classes are listed vertically
#   
#   accuracy <- (x[1,1] + x[2,2])/sum(x)
#   spec <- x[1,1]/(x[1,1] + x[2,1])
#   sens <- x[2,2]/(x[2,2] + x[1,2])
#   
#   return(list(accuracy = accuracy,
#               spec = spec,
#               sens = sens))
# }

# # 
# map_df(sens_spec_tables_alt, calc_model_performance, .id = "compound")%>%
#   write_csv("../../output/sens_spec_alt_logreg_01252021.csv")


stargazer2 <- function(model, odd.ratio = F, ...) {
  if(!("list" %in% class(model))) model <- list(model)
  
  if (odd.ratio) {
    coefOR2 <- lapply(model, function(x) signif(exp(coef(x)), 3)) #convert to OR
    # ref: https://stackoverflow.com/questions/16236560/odds-ratios-instead-of-logits-in-stargazer-latex-output
    #Se of OR is OR * SE(coef)
    seOR2 <- lapply(model, function(x) signif(exp(coef(x)) * summary(x)$coef[, 2], 3))
    p2 <- lapply(model, function(x) signif(summary(x)$coefficients[, 4], 3))
    stargazer(model, coef = coefOR2, se = seOR2, p = p2, ...)
    
  } else {
    stargazer(model, ...)
  }
}

# AUROC
auroc_ls <- sapply(compounds_glm,
                   function(x){paste0(x$mean_auc %>% round(2),
                                     " (",
                                     x$auc_lb %>% round(2),
                                     ", ",
                                     x$auc_ub %>% round(2),
                                     ")")})
# C statistics
c_stat_ls<-sapply(compounds_logreg, 
                  function(x){DescTools::Cstat(x[["model_std"]])%>%
                      round(2)})

stargazer_custom <- function(model, ...) {
  if(!("list" %in% class(model))) model <- list(model)
  coef_formatted <- lapply(model, function(x) signif(coef(x), 3)) #format coefficient
  se_formatted <- lapply(model, function(x) round(summary(x)$coef[, 2], 3))
  p2 <- lapply(model, function(x) signif(summary(x)$coefficients[, 4], 3))
  a<-coef_formatted %>% sapply(unname)
  b<-se_formatted %>% sapply(unname)
  ci_list <- map2(a, b, paste, "+/-")
  stargazer(model, coef = coef_formatted, se = se_formatted, p = p2, 
            ci= T,
            ci.custom = ci_list,
            ...)
}

stargazer_custom(lapply(compounds_logreg, function(x){x[["model_std"]]}), 
           odd.ratio = F, title="",
           align=TRUE, type="text",
           column.labels=c("PFPeA","PFHxA","PFHpA","PFOA","PFOS", "PFAS5"),
           model.numbers=FALSE, keep.stat=c("n","aic"),
           add.lines = list(c("C-Statistics", c_stat_ls),
                            c("AUROC", auroc_ls)),
           dep.var.labels.include = FALSE, dep.var.caption="",
           star.char = c("*", "**", "***"),
           star.cutoffs = c(.05, .01, .001),
           # change the order of the variables
           order = c("Industry: P", "Industry: T", "Industry: A", 
                     "Industry: M", "Industry: W", "Industry: O",
                     "Geo", "Hydro", "Soil"))#,
           #out="../../output/logmodel_std_02052021.txt")





