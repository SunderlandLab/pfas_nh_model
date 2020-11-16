# Purpose of this script is to calculate industry impact for private wells in NH

# Library -----------------------------------------------------------------

library(tidyverse)
library(rgeos)
library(rgdal)
library(sp)
library(sf)
library(raster)
library(tmap)

# Load --------------------------------------------------------------------

# read in well locations
unique_wells <- read.csv("../../raw_data/actual_unique.csv", header = TRUE, sep = ",")[,-1] %>%
  distinct_at(.vars = c("Longitude", "Latitude", "StationID")) %>%
  filter(!is.na(Latitude))%>%
  # convert data frame to sf
  st_as_sf(coords = c("Longitude", "Latitude")) %>%
  st_set_crs("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0") %>%
  st_transform("+proj=utm +zone=18 +datum=WGS84 +units=m")

unique_wells %>%
  st_write("../../raw_data/unique_wells.shp", append = FALSE)

# read in NH business information
proj_businesses <- st_read(dsn = "../../raw_data/NH_businesses_2016", layer = "NH_businesses_2016") %>%
  filter(grepl("^22132|^313|^314110|^314999|^322|^323|^324|^325|^3328|^332999|^3344|^48811|^562|^326|^333318|^333316|^333249|^424690|^442291|^561740", NAICS))%>%
  dplyr::select(c("OBJECTID", "CONAME", "NAICS"))%>%
  # transform to be the same CRS as wells
  st_transform(st_crs(unique_wells)) %>%
  # create industry group label based on NAICS code
  mutate(industry_group = case_when(grepl("^313|^314", NAICS) ~ "T",
                                    grepl("^326", NAICS) ~ "Pl",
                                    grepl("^562|^488|^221", NAICS) ~ "AW",
                                    grepl("^322|^323", NAICS) ~ "Pr",
                                    grepl("^334", NAICS) ~ "S",
                                    grepl("^324|^325|^332|^333|^424|^442|^561", NAICS) ~ "OI",
                                    TRUE ~ NA_character_))

#proj_businesses %>%
#  st_write("../../raw_data/nh_industry.shp", append = FALSE)


# Draw a bigger buffer for SG & TCI -------------------------------------------
# Industries for which airborne transport was confirmed, use 10km based on communication with NHDES
SG <- proj_businesses%>%
  filter(CONAME == "SAINT-GOBAIN PERFORMANCE PLSTC")
SG_buffer10km <-  SG%>%
  st_buffer(dist = 10000)
SG_buffer30km <-  SG%>%
  st_buffer(dist = 30000)

# Identify wells within the 10km radius buffer zone
SG_wells <- st_join(unique_wells, SG_buffer10km, join = st_intersects) %>%
  filter(!is.na(NAICS))
SG_distances <- st_distance(SG_wells, SG)
SG_wells<- cbind(SG_wells,SG_distances)

SG_wells_30km <- st_join(unique_wells, SG_buffer30km, join = st_intersects) %>%
  filter(!is.na(NAICS))


# tmap
options(tigris_class = "sf")
nh<-tigris::counties(state = "New Hampshire", cb = TRUE)
tmap_mode("plot")

tm_shape(nh) + 
  tm_polygons(border.col = "white") +
  tm_shape(unique_wells) +
  tm_bubbles(size = 0.1, border.col = "transparent", 
             alpha = 0.5, col = "grey30") +
  tm_shape(SG_buffer30km) +
  tm_polygons(border.col = "blue", alpha = 0) +
  tm_shape(SG_wells_30km) +
  tm_bubbles(size = 0.1, border.col = "blue",
             alpha = 0.5, col = "grey70") +
  tm_shape(SG_buffer10km) +
  tm_polygons(border.col = "red", alpha = 0) +
  tm_shape(SG_wells) +
  tm_bubbles(size = 0.1, border.col = "red", 
             alpha = 0.5, col = "grey70") +
  tm_layout(legend.title.size = 1.8,
              legend.text.size = 1.1,
              legend.outside = T,
              frame = F)
# TCI <- proj_businesses[proj_businesses@data$CONAME == "TEXTILES COATED INC",]
# TCI_buffer10km <- gBuffer(TCI, byid = TRUE, width = 10000)
# #proj_TCIbuffer <-spTransform(TCI_buffer10km, CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"))
# TCI_wells <- raster::intersect(proj_wells,TCI_buffer10km)
# #proj_TCIwells <-spTransform(TCI_wells, CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"))
# TCI_distances <- pointDistance(TCI,TCI_wells,lonlat = FALSE)
# TCI_data <- TCI_wells@data
# TCI_data$distances <- TCI_distances
# TCI_data <- TCI_data[,-2]
# 
# new_finalind <- setNames(data.frame(matrix(ncol = 3, nrow = 0)), 
#                          c("StationID","NAICS", "SALESVOL"))
# #new_finalind <- setNames(data.frame(matrix(ncol = 4, nrow = 0)), 
# # c("StationID","CONAME","NAICS", "SALESVOL"))
# for(i in 1:nrow(proj_wells)) {
#   #tryCatch skips over any weird things that throw an error with the intersect function
#   tryCatch({
#     # Draw buffer of size 1 km around wells
#     well_buffer <- gBuffer(proj_wells[i,], width = 1000)
#     # Intersect all potential businesses with buffer
#     intersection <- raster::intersect(proj_businesses, well_buffer)
#     # Filter by businesses we care about
#     biz_subset <- intersection[grep("^22132|^313|^314110|^314999|^322|^323|^324|^325|^3328|^332999|^3344|^48811|^562|^326|^333318|^333316|^333249|^424690|^442291|^561740", intersection@data$NAICS),]
#     len <- length(biz_subset)
#     y <- proj_wells[i,]
#     x <- y[rep(seq_len(1), each = len),]
#     # Calculate distance between well and point source
#     biz_distances <- pointDistance(proj_wells[i,], biz_subset, lonlat = FALSE)
#     biz_subset$distances <- biz_distances
#     biz_subset <- as.data.frame(biz_subset)
#     x <- as.data.frame(x)
#     x <- cbind(x,biz_subset)
#     new_finalind <- rbind(new_finalind,x[,c(1,4:7)])
#   }, error = function(e){})
# }
# 
# 
# # Merge -------------------------------------------------------------------
# 
# #new_finalind <- rbind(new_finalind, TCI_data, SG_data)
# new_finalind <- rbind(new_finalind[,-2], TCI_data, SG_data)
# sapply(new_finalind, function(x){sum(is.na(x))})
# # Save --------------------------------------------------------------------
# 
# saveRDS(new_finalind, '../../modeling_data/new_finalind.rds')
