# Business extraction
# Library -----------------------------------------------------------------

library(tidyverse)
library(rgeos)
library(rgdal)
library(sp)
library(raster)

# Load --------------------------------------------------------------------

unique <- read.csv("raw_data/actual_unique.csv", header = TRUE, sep = ",")

# Project wells -----------------------------------------------------------

unique_wells <- unique(unique[,-1])
unique_wells <- unique_wells[!is.na(unique_wells$Latitude),]
coordinates(unique_wells) <- c("Longitude", "Latitude")

proj4string(unique_wells) <- CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0")
proj_wells <- spTransform(unique_wells,CRS("+proj=utm +zone=18 +datum=WGS84 +units=m"))

businesses <- readOGR(dsn = "raw_data/NH_businesses_2016", layer = "NH_businesses_2016")
businesses <- businesses[, c("CONAME","NAICS","SALESVOL")]
proj_businesses <- spTransform(businesses,CRS("+proj=utm +zone=18 +datum=WGS84 +units=m"))


# Do bigger buffer for SG & TCI -------------------------------------------
# (industries for which airborne transport was confirmed)

SG <- proj_businesses[proj_businesses@data$CONAME == "SAINT-GOBAIN PERFORMANCE PLSTC",]
SG_buffer10km <- gBuffer(SG, byid = TRUE, width = 10000)
#proj_SGbuffer <-spTransform(SG_buffer10km, CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"))
SG_wells <- raster::intersect(proj_wells,SG_buffer10km)
#proj_SGwells <-spTransform(SG_wells, CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"))
SG_distances <- pointDistance(SG,SG_wells,lonlat = FALSE)
SG_data <- SG_wells@data
SG_data$distances <- SG_distances
SG_data <- SG_data[,-2]

TCI <- proj_businesses[proj_businesses@data$CONAME == "TEXTILES COATED INC",]
TCI_buffer10km <- gBuffer(TCI, byid = TRUE, width = 10000)
#proj_TCIbuffer <-spTransform(TCI_buffer10km, CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"))
TCI_wells <- raster::intersect(proj_wells,TCI_buffer10km)
#proj_TCIwells <-spTransform(TCI_wells, CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"))
TCI_distances <- pointDistance(TCI,TCI_wells,lonlat = FALSE)
TCI_data <- TCI_wells@data
TCI_data$distances <- TCI_distances
TCI_data <- TCI_data[,-2]

new_finalind <- setNames(data.frame(matrix(ncol = 3, nrow = 0)), 
                         c("StationID","NAICS", "SALESVOL"))
for(i in 1:nrow(proj_wells)) {
  #tryCatch skips over any weird things that throw an error with the intersect function
  tryCatch({
    # Draw buffer of size 1 km around wells
    well_buffer <- gBuffer(proj_wells[i,], width = 1000)
    # Intersect all potential businesses with buffer
    intersection <- raster::intersect(proj_businesses, well_buffer)
    # Filter by businesses we care about
    biz_subset <- intersection[grep("^22132|^313|^314110|^314999|^322|^323|^324|^325|^3328|^332999|^3344|^48811|^562|^326|^333318|^333316|^333249|^424690|^442291|^561740", intersection@data$NAICS),]
    len <- length(biz_subset)
    y <- proj_wells[i,]
    x <- y[rep(seq_len(1), each = len),]
    # Calculate distance between well and point source
    biz_distances <- pointDistance(proj_wells[i,], biz_subset, lonlat = FALSE)
    biz_subset$distances <- biz_distances
    biz_subset <- as.data.frame(biz_subset)
    x <- as.data.frame(x)
    x <- cbind(x,biz_subset)
    new_finalind <- rbind(new_finalind,x[,c(1,4:7)])
  }, error = function(e){})
}


# Merge -------------------------------------------------------------------

new_finalind <- rbind(new_finalind[,-2], TCI_data, SG_data)


# Save --------------------------------------------------------------------

saveRDS(new_finalind, 'modeling_data/new_finalind.rds')



