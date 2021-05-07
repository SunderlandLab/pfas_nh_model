# Evaluate logistic regression models
# Library -----------------------------------------------------------------

library(tidyverse)
library(broom)
library(stargazer)
# Load --------------------------------------------------------------------

compounds_data <- readRDS('../../modeling_data/compounds_data.rds')
compounds_logreg <- readRDS('../../models/compounds_logreg.rds')
compounds_glm <- readRDS("../../models/compounds_glm.rds")

# Evaluate models ---------------------------------------------------------

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

stargazer(lapply(compounds_logreg, function(x){x[["model_std"]]}), 
           title="",
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
                     "Geo", "Hydro", "Soil"),
           out="../../output/logmodel_std.txt")





