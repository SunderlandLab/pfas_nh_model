# Evaluate logistic regression models
# Library -----------------------------------------------------------------

library(tidyverse)
library(broom)
library(stargazer)
# Load --------------------------------------------------------------------

compounds_data <- readRDS('../../modeling_data/compounds_data01052021.rds')
compounds_logreg_alt <- readRDS('../../models/compounds_logreg_alt01052021.rds')

# Evaluate models ---------------------------------------------------------

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
  write_csv("../../output/sens_spec_alt_logreg_01052021.csv")


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
           out="../../output/logmodel_alt_01052021.txt")


