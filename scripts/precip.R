# # Precipitation Data
# 
# # Library -----------------------------------------------------------------
# 
# library(raster)
# library(rgdal)
# library(lubridate)
# 
# # Load --------------------------------------------------------------------
# 
# PFASwells <- read.csv("raw_data/PFAS_R2.csv", header = TRUE, sep = ",")
# 
# # Separate dates into year, month, & day
# dates <- as.Date(PFASwells$Sample.Date, "%m/%d/%y")
# datetxt <- as.Date(dates)
# PFASwells$Year = as.factor(format(datetxt, format = "%Y"))
# PFASwells$Month = as.factor(format(datetxt, format = "%m"))
# PFASwells$Day = as.factor(format(datetxt, format = "%d"))
# 
# # Extract New Hampshire precipitation data --------------------------------
# 
# coords <- subset(PFASwells, Month == "01" & Year == "2016", select = c(Latitude,Longitude))
# coordinates(coords) <- c("Longitude","Latitude")
# precip <- raster('raw_data/Precip/january16/PRISM_ppt_stable_4kmM3_201601_bil.bil')
# nh <- shapefile('raw_data/NHOutline.shp')
# nh_precip <- mask(precip, nh)
# val <- extract(x = precip, y = coords)
# 
# 
# # Loop through 2016 data --------------------------------------------------
# 
# path = "raw_data/Precip/2016/"
# file.names <- dir(path, pattern = "_bil.bil")
# wells16 <- subset(PFASwells, Year == "2016")
# wells16$Precip <- NA
# for (i in 1:nrow(wells16)) {
#   month <- wells16$Month[i]
#   filename <- paste0(path, file.names[month])
#   precip <- raster(filename)
#   lon = wells16$Longitude[i]
#   lat = wells16$Latitude[i]
#   
#   if (!is.na(lon) && !is.na(lat)) {
#     coords = data.frame(lon, lat)
#     coordinates(coords) <- c("lon", "lat")
#     val <- extract(x = precip, y = coords)
#     wells16$Precip[i] <- val
#   }
# }
# 
# 
# 
# # 2017 data ---------------------------------------------------------------
# 
# path = "/Users/beverlyge/Research/NH_PFAS/Precip/2017/"
# #Retrieve all monthly bil files in the directory for that year
# file.names <- dir(path, pattern = "_bil.bil")
# #Subset the PFASwells data to just the 2017 data
# wells17 <- subset(PFASwells, Year == "2017")
# #Convert the month column to an integer to make it easier to work with later
# wells17$Month <- as.integer(wells17$Month)
# #Create a new column for precipitation, set all values to NA
# wells17$Precip <- NA
# 
# #Iterate through each month's bil file
# for(month in 1:length(file.names)) {
#   #Load the precipitation raster for that month
#   filename <- paste0(path, file.names[month])
#   precip <- raster(filename)
#   #Iterate through all the 2017 wells (yes, this can probably be improved)
#   for (i in 1:nrow(wells17)) {
#     #If a given well's data was taken in the same month
#     if (wells17$Month[i] == month) {
#       #Get the latitude and longitude
#       lon = wells17$Longitude[i]
#       lat = wells17$Latitude[i]
#       #If lat/lon are not NA
#       if (!is.na(lon) && !is.na(lat)) {
#         #Make a dataframe for lon/lat and then convert them to actual coordinates 
#         coords = data.frame(lon, lat)
#         coordinates(coords) <- c("lon", "lat")
#         #Extract the precipitation value at those coordinates
#         val <- extract(x = precip, y = coords)
#         #Add this value to the table
#         wells17$Precip[i] <- val
#       }
#     }
#   }
# }
# 
# 
# # 2015 Data ---------------------------------------------------------------
# 
# path = "raw_data/Precip/2015/"
# file.names <- dir(path, pattern = "_bil.bil")
# wells15 <- subset(PFASwells, Year == "2015")
# wells15$Month <- as.integer(wells15$Month)
# wells15$Precip <- NA
# 
# for (month in 1:length(file.names)) {
#   filename <- paste0(path, file.names[month])
#   precip <- raster(filename)
#   for (i in 1:nrow(wells15)) {
#     if (wells15$Month[i] == month) {
#       lon = wells15$Longitude[i]
#       lat = wells15$Latitude[i]
#       if (!is.na(lon) && !is.na(lat)) {
#         coords = data.frame(lon, lat)
#         coordinates(coords) <- c("lon", "lat")
#         val <- extract(x = precip, y = coords)
#         wells15$Precip[i] <- val
#       }
#     }
#   }
# }
# 
# 
# # 2014 Data ---------------------------------------------------------------
# 
# #2014
# path = "raw_data/Precip/2014/"
# file.names <- dir(path, pattern = "_bil.bil")
# wells14 <- subset(PFASwells, Year == "2014")
# wells14$Month <- as.integer(wells14$Month)
# wells14$Precip <- NA
# 
# for (month in 1:length(file.names)) {
#   filename <- paste0(path, file.names[month])
#   precip <- raster(filename)
#   for (i in 1:nrow(wells14)) {
#     if (wells14$Month[i] == month) {
#       lon = wells14$Longitude[i]
#       lat = wells14$Latitude[i]
#       if (!is.na(lon) && !is.na(lat)) {
#         coords = data.frame(lon, lat)
#         coordinates(coords) <- c("lon", "lat")
#         val <- extract(x = precip, y = coords)
#         wells14$Precip[i] <- val
#       }
#     }
#   }
# }
# 
# 
# # Combine data ------------------------------------------------------------
# 
# total_precip <- rbind(wells14, wells15, wells16, wells17)
# # save
# write.csv(total_precip, "precip_PFAS.csv")
# 
