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

compounds_forest <- readRDS('../../models/compounds_forest1207.rds')
compounds_data <- readRDS('../../modeling_data/compounds_data1207.rds')



# Tune model --------------------------------------------------------------
# Determine which combo minimizes error and maximizes predictive accuracy
# DO NOT RUN IT HERE!

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
#   for (i in 1:2) {
#     for (j in 1:2) { 
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

  


# Plot regression forests -------------------------------------------------

# for (compound in compounds) {
#   comp_folder <- paste0("../../models_forest_eval/", compound, '/')
#   dir.create(comp_folder)
#   jpeg(paste0(comp_folder, compound, '_fplot.jpg'))
#   plot(compounds_forest[[compound]][['reg_forest']],
#        main = paste(compound, 'Regression Random Forest'))
#   dev.off()
# }
# 

# Sensitivity and specificity analysis ------------------------------------

sens_spec_tablesf <- map(compounds_forest, function(clist) {
  predicted_classes <- clist[['predictions']]
  observed_classes <- clist[['test_data']]$final
  print(mean(predicted_classes == observed_classes))
  return(table(predicted_classes, observed_classes))
})

#map_df(sens_spec_tablesf, calc_model_performance, .id = "compound")%>%
#  write_csv("../../output/sens_spec_alt_rf_10082020.csv")


# Variable Importance Plots
variable_names<- compounds_forest$PFOA$train_data%>%
  dplyr::select(-final)%>%
  colnames()

var_imp_df<-map_df(compounds_forest, function(x){importance(x[['forest']], type = 1, scale = FALSE)})
var_imp_df$variable <- variable_names  
var_imp_df <- var_imp_df%>%
  mutate(PFHxA = PFHXA,
         PFPeA = PFPEA,
         PFHpA = PFHPA) %>%
  select(-c(PFHXA, PFPEA, PFHPA))
level_key <- c("ImpactPl3" = "Industry: Plastics and rubber", 
               "recharge" = "Hydro: Groundwater recharge",
               "precip" = "Hydro: Monthly precipitation",
               "ImpactT3" = "Industry: Textiles manufacturing",
               "silttotal_r" = "Soil: Percent total silt",
               "cec7_r" = "Soil: Cation exchange capacity",
               "claytotal_r" = "Soil: Percent total clay",
               "slopegradwta" = "Hydro: Slope gradient",
               "ImpactPr3" = "Industry: Printing industry",
               "soc0_999" = "Soil: Organic carbon",
               "dbthirdbar_r" = "Soil: Bulk density",
               "awc_r" = "Soil: Available water capacity",
               "ImpactOI3" = "Industry: Other",
               "ImpactAW3" = "Industry: Airport and waste management",
               "hzdep" = "Soil: Thickness of soil horizon",
               "bedrock_M" = "Geo: Bedrock type",
               "hydgrpdcdA" = "Hydro: Low runoff potential",
               "ImpactS3" = "Industry: Semiconductor manufacturing",
               "wtdepannmin" = "Hydro: Depth to water table",
               "brockdepmin" = "Geo: Depth to bedrock")

##########################
#Option 1 facet_grid 2D  #
##########################
var_imp_df%>%
  mutate(variable = recode(variable, !!!level_key)) %>%
  separate(variable, into = c("group", "varname"), sep = ":") %>%
  mutate(varname = trimws(varname))%>%
  pivot_longer(-c(varname, group)) %>%
  mutate(name = factor(name, levels = c("PFPeA", "PFHxA", "PFHpA", "PFOA", "PFOS")))%>%
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
        axis.text.x = element_text(size = 16, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 16),
        legend.title = element_blank(),
        legend.key.size = unit(1, "cm"),
        legend.text = element_text(size = 16),
        legend.position="bottom")
ggsave("../../output/Figure2_rf_class_var_imp.png",width = 9,
  height = 9,
  units = "in")

##########################
#Option 2 facet_wrap 1D  #
##########################
# var_imp_df%>%
#   mutate(variable = recode(variable, !!!level_key)) %>%
#   pivot_longer(-variable) %>%
#   ggplot(aes(x= reorder(variable, value), 
#              y=value, fill = value)) +
#   geom_bar(stat = "identity", color = "grey50") +
#   scale_fill_viridis() +
#   coord_flip() +
#   facet_wrap(vars(name), scales = "free_x") +
#   xlab("")+
#   ylab("Mean Decrease in Accuracy") +
#   theme_classic() + 
#   theme(text = element_text(size=14),
#         axis.text.x = element_text(angle = 45, hjust = 1),
#         legend.title = element_blank(),
#         legend.key.size = unit(0.75, "cm"),
#         legend.position="top")
# ggsave("../../output/Figure2_rf_class_var_imp_v2.png")
# 
# Save --------------------------------------------------------------------

#saveRDS(tuned_models, '../../models_forest_eval/tuned_models1207.rds')
#saveRDS(sens_spec_tablesf, '../../models_forest_eval/sens_spec_tablesf1207.rds')

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
  select(StationID, observed = final, predicted, Longitude, Latitude) %>%
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
             size = 0.3, border.col = "transparent", 
             alpha = 0.7,
             palette = "RdYlGn") + 
  tm_layout(legend.title.size = 1.8,
            legend.text.size = 1.1,
            frame = F)
tmap_save(m, "../../output/Figure3_pred_vs_obs.png")

######################
# TOC map for PFOA + PFOS #
######################
map_pfoa_pfos <- compounds_data[['PFOA']] %>%
  left_join(compounds_data[['PFOS']] %>% select(StationID, reg), by = "StationID", suffix = c("_pfoa", "_pfos")) %>%
  left_join(unique, by = "StationID") %>%
  mutate(reg = reg_pfoa + reg_pfos,
         reg_cat =  cut(reg,
                        breaks=c(0, 10, 15, 30, 70, 53000),
                        labels=c("< 10", "10 - 15", "15 - 30", "30 - 70","> 70"),
                        include.lowest=TRUE)) %>%
  select(StationID, reg_cat,  Longitude, Latitude) %>%
  filter(!is.na(Longitude) & !is.na(Latitude)) %>%
  st_as_sf(coords = c("Longitude", "Latitude")) %>%
  st_set_crs("WGS84") 

m2<-tm_shape(nh_map) +
  tm_polygons(border.col = "white") +
  tm_shape(map_pfoa_pfos) +
  tm_bubbles(col = "reg_cat", 
             size = 0.3, border.col = "transparent", 
             alpha = 0.7,
             palette = "-RdYlGn") + 
  tm_layout(legend.title.size = 1.8,
            legend.text.size = 1.1,
            legend.outside = T,
            frame = F)
tmap_save(m2, "../../output/TOC_PFOA_PFOS.png")

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
tmap_save(m3, "../../output/TOC_PFOA_PFOS.png")
