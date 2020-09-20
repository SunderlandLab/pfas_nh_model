## Script for importing raw data and coercing into tidy form

# Library -----------------------------------------------------------------

library(tidyverse)
library(raster)
library(rgeos)
library(rgdal)
library(maptools)
library(proj4)


# Load --------------------------------------------------------------------

precip <- read.csv('raw_data/precip_PFAS.csv', header = TRUE, sep = ",", na.strings = c("", " ", "NA"))
bedrock <- read.csv('raw_data/bedrock_extraction.csv', header = TRUE, sep = ",", na.strings = c("", " ", "NA"))
recharge <- read.csv('raw_data/recharge.csv', header = TRUE, sep = ",", na.strings = c("", " ", "NA"))
unique <- read.csv('raw_data/actual_unique.csv', header = TRUE, sep = ",")
# soildata <- raster('raw_data/GCSraster1.bil') %>% crs()
PFASwells <- read.csv('raw_data/PFAS_R2.csv', header = TRUE, sep = ",")





