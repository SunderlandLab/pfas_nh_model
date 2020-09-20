# Process soil data
# Library -----------------------------------------------------------------

library(raster)
library(rgdal)
library(maptools)
library(proj4)

# Load --------------------------------------------------------------------
# Note: soil data come from gSSURGO, organized by "mukeys" (Map Unit Keys)

unique <- read.csv("raw_data/actual_unique.csv", header = TRUE, sep = ",")
soildata <- raster('raw_data/GCS_Raster_1/GCSraster1.bil')
project.crs <- crs(soildata)


# Get unique well locations and project onto correct coordinate sy --------

unique1 <- unique(unique[,-1])
# Distinguish unique wells
unique1 <- unique1[!is.na(unique1$Latitude),] 
coordinates(unique1) <- c("Longitude", "Latitude")
proj4string(unique1) <- CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0")


# Extract mukeys from soil data raster to unique well locations -----------

unique1$mukey <- raster::extract(x = soildata, y = unique1)
# Remove wonky values
unique1$mukey[unique1$mukey == 2147483647] <- NA
# Convert back to regular data frame
unique1 <- as.data.frame((unique1))

# Get all soil properties we care about
component <- read.table("raw_data/component.txt", header = TRUE, sep = ",")
component1 <- component[, c(3,109:136)]



# Calculate weighted average ----------------------------------------------
# Each map unit (mukey) is made of a bunch of cokeys (given the % that each cokey occupies)
component1$percentage <- component1$comppct_r/100
for (i in c(4:27)) {
  component1[,i] <- as.numeric(component1[,i])
  component1[,i] <- component1[,i] * component1$percentage
}

# Remove duplicates for categorical variables
component_categorical <- component1[,c(2, 28:29)]
x <- duplicated(component_categorical$mukey)
y <- component_categorical[!x,]

component_numeric <- component1[, c(2,4:27)]
# Revisit: perhaps there's a better way in which for a given mukey, if all the values are 
# NA, the final value should be NA...ask Cindy - replace NAs with means?

z <- aggregate(component_numeric[,-1], by = list(component_numeric$mukey), FUN = sum, na.rm = T)
colnames(z)[colnames(z) == "Group.1"] <- "mukey"
all_vars <- merge(z,y, by = "mukey")
merged <- merge(unique1, all_vars, by = "mukey", all.x=TRUE)
merged$hzdep <- merged$hzdepb_r - merged$hzdept_r
merged$hzdepb_r = merged$hzdept_r = merged$ph01mcacl2_r = merged$slopegraddcp <- NULL
final_soildata <- merged[, c(2,5:24,27,25:26)]


# Remove rows with all NA's -----------------------------------------------

final_soildata <- final_soildata[-c(2365:2369),]


# Save --------------------------------------------------------------------

saveRDS(final_soildata, 'modeling_data/final_soildata.rds')


