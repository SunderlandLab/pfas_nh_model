# Evaluate random forests
# Library -----------------------------------------------------------------

library(tree)
library(randomForest)
library(caret)
library(tidyverse)
library(reshape2)
library(viridis)
library(sf)
library(tmap)
library(ggthemes)
options(scipen=999)
# Load --------------------------------------------------------------------

compounds_forest <- readRDS('../../models/compounds_forest.rds')
compounds_data <- readRDS('../../modeling_data/compounds_data.rds')
compounds_glm <- readRDS("../../modeling_data/compounds_glm.rds")
# compounds_data %>%
#   map(function(x){
#     x %>% 
#       dplyr::select(StationID, final)
#   }) %>%
#   saveRDS("../../modeling_data/wells_with_label.rds")


# Tune model --------------------------------------------------------------
# Determine which combo minimizes error and maximizes predictive accuracy
# TAKES A LONG TIME TO RUN
# 
# compounds <- names(compounds_forest)
# 
# tuned_models <- list()
# for (comp in compounds) {
#   ntrees <- 1000
#   form <- as.formula("final~.")
#   num_vars <- ncol(compounds_forest[[comp]][['train_data']])
#   predictions_table <- data.frame("mtry" = rep(0,(num_vars - 4)*10),
#                                   "nodesize" = rep(0,(num_vars - 4)*10),
#                                   "predictive accuracy" = rep(0,(num_vars - 4)*10))
#   error_table <- data.frame("tree.index" = c(1:ntrees))
#   tracker <- 1
#   for (i in 1:(num_vars - 4)) {
#     for (j in 1:10) {
#       mt <- i + 4 # Because starting at 5, not 1
#       model <- randomForest(form,
#                             data = compounds_forest[[comp]][['train_data']],
#                             mtry = mt,
#                             ntree = ntrees,
#                             nodesize = j,
#                             importance = TRUE)
#       error_table <- cbind(error_table, model$err.rate[,1])
#       col_name <- paste0("m",i,"ns",j)
#       print(col_name)
#       names(error_table)[tracker + 1] <- col_name
#       predictions <- model %>% predict(compounds_forest[[comp]][['test_data']], type = "class")
#       predictions_table[tracker, 1] <- mt
#       predictions_table[tracker, 2] <- j
#       predictions_table[tracker, 3] <- mean(predictions == compounds_forest[[comp]][['test_data']]$final)
#       print(tracker)
#       tracker <- tracker + 1
#     }
#     tuned_models[[comp]] <- list(predictions_table = predictions_table,
#                    error_table = error_table)
#   }
# }
# 
# saveRDS(tuned_models, '../../models_forest_eval/tuned_models01252021.rds')
# 
# for (comp in compounds) {
#   print(comp)
#   index <- which.max(tuned_models[[comp]]$predictions_table$predictive.accuracy)
#   print(tuned_models[[comp]]$predictions_table[index,])
# }

# Tune random forest regression model
# 
# tuned_reg_models <- list()
# for (comp in compounds) {
#   ntrees <- 500
#   # limit to wells with detectable levels
#   reg <- compounds_data[[comp]][compounds_data[[comp]]$final == 1, ]%>%
#     mutate(reg_log = log(reg))%>%
#     dplyr::select(-c(StationID, reg, final))
#   set.seed(123)
#   ids <- sample(0.7*nrow(reg))
#   reg_train <- reg[ids,]
#   reg_test <- reg[-ids,]
#   form <- as.formula("reg_log~.")
#   num_vars <- ncol(reg)
#   predictions_table <- data.frame("mtry" = rep(0,(num_vars - 4)*10),
#                                   "nodesize" = rep(0,(num_vars - 4)*10),
#                                   "RMSE" = rep(0,(num_vars - 4)*10))
#   error_table <- data.frame("tree.index" = c(1:ntrees))
#   tracker <- 1
#   for (i in 1:(num_vars - 4)) {
#     for (j in 1:10) {
#       mt <- i + 4 # Because starting at 5, not 1
#       model <- randomForest(form,
#                      data = reg_train,
#                      mtry = mt,
#                      ntree = ntrees,
#                      nodesize = j,
#                      importance = TRUE)
#       error_table <- cbind(error_table, model$mse)
#       col_name <- paste0("m",i,"ns",j)
#       print(col_name)
#       names(error_table)[tracker + 1] <- col_name
#       predictions <- model %>% predict(reg_test)
#       predictions_table[tracker, 1] <- mt
#       predictions_table[tracker, 2] <- j
#       predictions_table[tracker, 3] <- RMSE(predictions, reg_test$reg_log)
#       print(tracker)
#       tracker <- tracker + 1
#     }
#     tuned_reg_models[[comp]] <- list(predictions_table = predictions_table,
#                    error_table = error_table)
#   }
# }
# 
# saveRDS(tuned_reg_models, '../../models_forest_eval/tuned_reg_models02012021.rds')
# 
# for (comp in compounds) {
#   print(comp)
#   index <- which.min(tuned_reg_models[[comp]]$predictions_table$RMSE)
#   print(tuned_reg_models[[comp]]$predictions_table[index,])
# }


# Sensitivity and specificity analysis ------------------------------------

# sens_spec_tablesf <- map(compounds_forest, function(clist) {
#   predicted_classes <- clist[['predictions']]
#   observed_classes <- clist[['test_data']]$final
#   print(mean(predicted_classes == observed_classes))
#   return(table(predicted_classes, observed_classes))
# })
# 
# map_df(sens_spec_tablesf, calc_model_performance, .id = "compound")%>%
#  write_csv("../../output/sens_spec_alt_rf_02022021.csv")

# ROC curve and confidence interval
compounds <- names(compounds_data)
cols <- RColorBrewer::brewer.pal(12,'Paired')
# no.11 is light yellow
cols[11] <- "#E6AB02"

png("../../output/Figure1_model_performance.png",
    width = 7,
    height = 5,
    res = 300,
    units = "in")
par(mfrow = c(2,3))
for(i in 1:6){
  comp <- compounds[i]
  perf.rf <- compounds_forest[[comp]]$perf.rf
  perf.glm <- compounds_glm[[comp]]$perf.glm
  #mean_auc <- compounds_forest[[comp]]$mean_auc
  #auc_lb <- compounds_forest[[comp]]$auc_lb
  #auc_ub <- compounds_forest[[comp]]$auc_ub
  #corret the lower/upper case in PFAS names
  comp <- case_when(comp == "PFPEA" ~ "PFPeA",
                    comp == "PFHPA" ~ "PFHpA",
                    comp == "PFHXA" ~ "PFHxA",
                    comp == "PFAS" ~ "sumPFAS",
                    TRUE ~ comp)
  plot(perf.rf, lty=3, col=cols[2*i-1], main = comp)
  plot(perf.glm, lty=3, col=cols[2*i], add = TRUE)
  plot(perf.rf, avg="vertical", lwd=3, col = cols[2*i-1],
       spread.estimate="stderror", plotCI.lwd=2, add=TRUE)
  plot(perf.glm, avg="vertical", lwd=3, col = cols[2*i],
       spread.estimate="stderror", plotCI.lwd=2, add=TRUE)
  legend(0.1, 0.3 ,c('Random forest','Logistic regression'),col=c(cols[2*i-1], cols[2*i]), lwd=3,
         bty = "n")
}

dev.off()

# Variable Importance Plots
variable_names<- compounds_data$PFOA%>%
  dplyr::select(-c(StationID, final, reg))%>%
  colnames()

var_imp_df<-map_df(compounds_forest, function(x){importance(x[['forest']], type = 1, scale = FALSE)})
var_imp_df$variable <- variable_names  
var_imp_df <- var_imp_df%>%
  mutate(PFHxA = PFHXA,
         PFPeA = PFPEA,
         PFHpA = PFHPA) %>%
  dplyr::select(-c(PFHXA, PFPEA, PFHPA))

map(compounds_forest, function(x){
  print(paste("AUC", x$mean_auc %>% round(2), 
              "LB", x$auc_lb %>% round(2),
              "UB", x$auc_ub %>% round(2)))
})

level_key <- c("final" = "final",
              "ImpactPlastics" = "Industry: Plastics and rubber",
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
               "ImpactOI" = "Industry: Suspected sources",
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
##########################
#Option 1 facet_grid 2D  #
##########################
var_imp_df%>%
  mutate(variable = dplyr::recode(variable, !!!level_key)) %>%
  separate(variable, into = c("group", "varname"), sep = ":") %>%
  mutate(varname = trimws(varname),
         group = factor(group, levels = c("Industry", "Geo", "Hydro", "Soil")))%>%
  pivot_longer(-c(varname, group)) %>%
  mutate(name = factor(name, levels = c("PFPeA", "PFHxA", "PFHpA", "PFOA", "PFOS", "PFAS"),
                       ordered = T,
                       labels = c("PFPeA\n\nn:1618\nAUROC:0.79\n(0.76,0.82)", 
                                  "PFHxA\n\nn:1726\nAUROC:0.78\n(0.76,0.80)", 
                                  "PFHpA\n\nn:2221\nAUROC:0.85\n(0.84,0.87)", 
                                  "PFOA\n\nn:2377\nAUROC:0.84\n(0.83,0.85)", 
                                  "PFOS\n\nn:2376\nAUROC:0.74\n(0.71,0.78)",
                                  "sumPFAS\n\nn:2383\nAUROC:0.81\n(0.79, 0.83)")))%>%
  #mutate(name = dplyr::recode(name, !!!name_key)) %>%
  ggplot(aes(x= reorder(varname, value), 
             y=value, fill = value)) +
  geom_bar(stat = "identity", color = "grey50") +
  scale_fill_viridis(guide = guide_colorbar(frame.colour = "grey50", frame.linewidth = 2)) +
  coord_flip() +
  facet_grid(group ~ name, scales = "free_y", space = "free") +
  xlab("")+
  ylab("Relative contribution to accuracy") +
  theme_classic() + 
  theme(text = element_text(size = 16),
        strip.text = element_text(size = 16),
        axis.text.x = element_text(size = 16, angle = 90, hjust = 1),
        axis.text.y = element_text(size = 16),
        legend.title = element_blank(),
        legend.key.size = unit(1.2, "cm"),
        legend.text = element_text(size = 16),
        legend.position="bottom")
ggsave("../../output/Figure2_rf_class_var_imp.png",width = 13,
  height = 10,
  units = "in")

######################
# Figure 3 test PFOA #
######################

df_pfoa <- compounds_data[['PFOA']]
# make predictions on the entire data
predict_pfoa <- compounds_forest[['PFOA']]$forest %>%
  predict(df_pfoa, type = "class")
df_pfoa$predicted <- predict_pfoa
# add lat long to df_pfoa, keep only relevant columns
unique <- read.csv("../../raw_data/actual_unique.csv", header = TRUE, sep = ",")[,-1]
df_pfoa <- df_pfoa %>%
  left_join(unique, by = "StationID") %>%
  dplyr::select(StationID, observed = final, predicted, Longitude, Latitude) %>%
  filter(!is.na(Longitude) & !is.na(Latitude)) %>%
  mutate(Predicted = if_else(predicted == 0, "Non-detect", "Detected"),
         Observed = if_else(observed== 0, "Non-detect", "Detected"))

pfoa_sf <- st_as_sf(df_pfoa, coords = c("Longitude", "Latitude")) %>%
  st_set_crs("WGS84") 

# prepare a base map for NH
nh_map <- tigris::counties("New Hampshire", cb = TRUE) %>%
  st_as_sf()%>%
  st_transform(st_crs(pfoa_sf))

tmap_mode("plot")

m<-tm_shape(nh_map) +
  tm_polygons(border.col = "white") +
  tm_shape(pfoa_sf) +
  tm_bubbles(col = c("Observed","Predicted"), 
             size = 0.2, border.col = "transparent", 
             alpha = 0.7,
             palette = "RdYlGn") + 
  tm_layout(legend.title.size = 1.8,
            legend.text.size = 1.1,
            frame = F)
tmap_save(m, "../../output/Figure3_pred_vs_obs.png")

######################
# TOC map for PFOA + PFOS #
######################
us_map <- tigris::states(cb = TRUE) %>%
  st_as_sf()%>%
  st_transform(2163) %>%
  filter(!STUSPS %in% c("HI", "AK", "PR", "AS", "VI", "GU", "MP")) %>%
  mutate(flag = if_else(STUSPS == "NH", 1, 0))

m3<-tm_shape(us_map) +
  tm_borders("black") +
  tm_fill(col = "flag",
          palette = c("white", "red"),
          legend.show = FALSE) + 
  tm_layout(frame = F)
tmap_save(m3, "../../output/US_map.png")
