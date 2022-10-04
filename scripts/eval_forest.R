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
               "soc0_999" = "Soil: Organic carbon",
               "dbthirdbar_r" = "Soil: Bulk density",
               "awc_r" = "Soil: Available water capacity",
               "ImpactOI" = "Industry: Potential sources",
               "ImpactAirports" = "Industry: Airports",
               "ImpactWWTP" = "Industry: Wastewater treatment plant",
               "ImpactMilitary" = "Industry: Military AFFF",
               "hzdep" = "Soil: Thickness of soil horizon",
               "bedrock_M" = "Geo: Bedrock type",
               "hydgrpdcdA" = "Hydro: Low runoff potential",
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
                                  "PFHxA\n\nn:1726\nAUROC:0.78\n(0.75,0.80)", 
                                  "PFHpA\n\nn:2221\nAUROC:0.86\n(0.84,0.87)", 
                                  "PFOA\n\nn:2377\nAUROC:0.84\n(0.83,0.85)", 
                                  "PFOS\n\nn:2376\nAUROC:0.74\n(0.72,0.77)",
                                  "sumPFAS\n\nn:2383\nAUROC:0.81\n(0.79, 0.83)")))%>%
  #mutate(name = dplyr::recode(name, !!!name_key)) %>%
  ggplot(aes(x= reorder(varname, value), 
             y=value, fill = value)) +
  geom_bar(stat = "identity", color = "grey50") +
  scale_fill_viridis(guide = guide_colorbar(frame.colour = "grey50", frame.linewidth = 2)) +
  coord_flip() +
 #ylim(0, 0.1)+
  facet_grid(group ~ name, scales = "free_y", space = "free") +
  xlab("")+
  ylab("Relative contribution to accuracy") +
  theme_classic() + 
  theme(text = element_text(size = 16),
        panel.spacing = unit(1, "lines"),
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


