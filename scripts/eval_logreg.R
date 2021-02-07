# Evaluate logistic regression models
# Library -----------------------------------------------------------------

library(tidyverse)
library(broom)
library(stargazer)
# Load --------------------------------------------------------------------

compounds_data <- readRDS('../../modeling_data/compounds_data01232021.rds')
compounds_logreg <- readRDS('../../models/compounds_logreg_02052021.rds')

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
    coefOR2 <- lapply(model, function(x) signif(exp(coef(x))-1, 3)) # -1 to convert OR to % less
    seOR2 <- lapply(model, function(x) signif(exp(coef(x)) * summary(x)$coef[, 2], 3))
    p2 <- lapply(model, function(x) signif(summary(x)$coefficients[, 4], 3))
    stargazer(model, coef = coefOR2, se = seOR2, p = p2, ...)
    
  } else {
    stargazer(model, ...)
  }
}



# Impact characterized as the count of industry with exponential decay
c_stat_ls<-sapply(compounds_logreg_alt, 
                  function(x){DescTools::Cstat(x[["model"]])%>%
                      round(3)})

level_key <- c("ImpactPlastics" = "Industry: Plastics and rubber", 
               "recharge" = "Hydro: Groundwater recharge",
               "precip" = "Hydro: Monthly precipitation",
               "ImpactTextile" = "Industry: Textiles manufacturing",
               "silttotal_r" = "Soil: Percent total silt",
               "cec7_r" = "Soil: Cation exchange capacity",
               "claytotal_r" = "Soil: Percent total clay",
               "slopegradwta" = "Hydro: Slope gradient",
               #"ImpactPr" = "Industry: Printing industry",
               "soc0_999" = "Soil: Organic carbon",
               "dbthirdbar_r" = "Soil: Bulk density",
               "awc_r" = "Soil: Available water capacity",
               "ImpactOI" = "Industry: Other",
               "ImpactAirports" = "Industry: Airports",
               "ImpactWWTP" = "Industry: Wastewater treatment plant",
               "ImpactMilitary" = "Industry: Military AFFF",
               #"ImpactM" = "Industry: Metal plating",
               "hzdep" = "Soil: Thickness of soil horizon",
               "bedrock_M" = "Geo: Bedrock type",
               "hydgrpdcdA" = "Hydro: Low runoff potential",
               #"ImpactS" = "Industry: Semiconductor manufacturing",
               "wtdepannmin" = "Hydro: Depth to water table",
               "brockdepmin" = "Geo: Depth to bedrock",
               "sandtotal_r" = "Soil: Percent total sand",
               "ksat_r" = "Soil: Saturated hydraulic conductivity")

stargazer2(lapply(compounds_logreg, function(x){x[["model"]]}), 
           odd.ratio = T, title="",
           align=TRUE, type="text",
           column.labels=c("PFOA","PFHxA","PFPeA","PFHpA","PFOS", "PFAS5"),
           model.numbers=FALSE, keep.stat=c("n","aic"),
           add.lines = list(c("C-Statistics", c_stat_ls)),
           dep.var.labels.include = FALSE, dep.var.caption="",
           star.char = c("*", "**", "***"),
           star.cutoffs = c(.05, .01, .001),
           out="../../output/logmodel_02052021.txt")


