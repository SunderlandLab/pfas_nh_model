# TOC art
# Library -----------------------------------------------------------------

library(tree)
library(randomForest)
library(caret)
library(tidyverse)
library(reshape2)
library(viridis)
library(sf)
library(tmap)
library(dismo)
library(pacman)
library(gstat)
p_load(deldir)
options(scipen=999)
# Load --------------------------------------------------------------------

compounds_forest <- readRDS('../../models/compounds_forest1207.rds')
compounds_data <- readRDS('../../modeling_data/compounds_data1207.rds')

df_pfoa <- compounds_data[['PFOA']]
# make predictions on the entire data
predict_pfoa <- compounds_forest[['PFOA']]$forest %>%
    predict(df_pfoa, type = "class")
df_pfoa$predicted <- predict_pfoa
# add lat long to df_pfoa, keep only relevant columns
unique <- read.csv("../../raw_data/actual_unique.csv", header = TRUE, sep = ",")[,-1]
df_pfoa <- df_pfoa %>%
    left_join(unique, by = "StationID") %>%
    #select(StationID, observed = final, predicted, Longitude, Latitude) %>%
    filter(!is.na(Longitude) & !is.na(Latitude)) 

TA <- CRS("+proj=tmerc +lat_0=42.5 +lon_0=-71.66666666666667 +k=0.999966667 +x_0=300000 +y_0=0 +ellps=GRS80 +units=us-ft +no_defs")

pfoa_sp <- SpatialPointsDataFrame(data = df_pfoa, coords = df_pfoa[,31:32],
                                  proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
pfoa_sp <- spTransform(pfoa_sp, TA)

# prepare a base map for NH
nh_map <- tigris::counties("New Hampshire", cb = TRUE) %>%
    spTransform(TA) %>%
    aggregate()

# spatial interpolation
# reference: https://rspatial.org/raster/analysis/4-interpolation.html
# tm_shape(pfoa_sp) +
#     tm_symbols(col = "precip", palette = "YlOrRd", alpha = 0.6, style = "quantile")

# prosimity polygons
#p <- matrix(c(df_pfoa$Longitude, df_pfoa$Latitude, df_pfoa$precip), ncol = 3)
create_raster_from_wells<-function(pfoa_sp, nh_map, varname = 'precip'){
    v <- voronoi(pfoa_sp) 
    vnh <- raster::intersect(v, nh_map)
    #spplot(vnh, "precip")
    # rasterize
    r <- raster(nh_map, res=10000)
    vr <- rasterize(vnh, r, varname)
    #plot(vr)
    # nearest neighbors
    gs <- gstat(formula = as.formula(paste0(varname, "~1")), locations = pfoa_sp, nmax=5, set=list(idp = 0))
    nn <- interpolate(r, gs)
    ## [inverse distance weighted interpolation]
    nnmsk <- mask(nn, vr)
    return(nnmsk)
}

raster_precip <- create_raster_from_wells(pfoa_sp, nh_map, "precip")
png("../../output/precip_raster.png")
plot(raster_precip, col = RColorBrewer::brewer.pal('RdYlGn', n=11), main = "Precipitation", axes=FALSE, box=FALSE, legend = FALSE)
dev.off()

raster_impactpl <- create_raster_from_wells(pfoa_sp, nh_map, "ImpactPl3")
plot(raster_impactpl, col = RColorBrewer::brewer.pal('YlOrRd', n=11), main = "Impact from plastic industry", axes=FALSE, box=FALSE, legend = FALSE)

raster_impactaw <- create_raster_from_wells(pfoa_sp, nh_map, "ImpactAW3")
png("../../output/airport_waste_raster.png")
plot(raster_impactaw, col = RColorBrewer::brewer.pal('YlOrRd', n=11), main = "Impact from airports and waste management",
     axes=FALSE, box=FALSE, legend = FALSE)
dev.off()

raster_recharge <- create_raster_from_wells(pfoa_sp, nh_map, "recharge")
png("../../output/gw_recharge_raster.png")
plot(raster_recharge, col = RColorBrewer::brewer.pal('Blues', n=11), main = "Groundwater recharge",
     axes=FALSE, box=FALSE, legend = FALSE)
dev.off()

######################
# TOC map for PFOA + PFOS #
######################
library(tidyverse)
library(tmap)
library(sf)
us_map <- tigris::states(cb = TRUE) %>%
  st_as_sf()%>%
  st_transform(2163) %>%
  filter(!STUSPS %in% c("HI", "AK", "PR", "AS", "VI", "GU", "MP")) %>%
  mutate(flag = if_else(STUSPS == "NC", 1, 0))

m3<-tm_shape(us_map) +
  tm_borders("black") +
  tm_fill(col = "flag",
          palette = c("white", "black"),
          legend.show = FALSE) + 
  tm_layout(frame = F)
tmap_save(m3, "../../output/US_map_NC.png")
