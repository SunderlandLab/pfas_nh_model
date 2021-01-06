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

#unique_wells %>%
#  st_write("../../raw_data/unique_wells.shp", append = FALSE)

# read in NH business information from ESRI business analyst
proj_businesses <- st_read(dsn = "../../raw_data/NH_businesses_2016", layer = "NH_businesses_2016") %>%
  filter(grepl("^22132|^313|^314110|^314999|^322|^323|^324|^325|^3328|^332999|^3344|^48811|^562|^326|^333318|^333316|^333249|^424690|^442291|^561740", NAICS))%>%
  dplyr::select(c("OBJECTID", "CONAME", "NAICS"))%>%
  # transform to be the same CRS as wells
  st_transform(st_crs(unique_wells)) %>%
  # create industry group label based on NAICS code
  mutate(industry_group = case_when(grepl("^313|^314", NAICS) ~ "T",
                                    grepl("^326", NAICS) ~ "Pl",
                                    grepl("^562|^221", NAICS) ~ "W",
                                    grepl("^488", NAICS) ~ "A",
                                    grepl("^322|^323", NAICS) ~ "Pr",
                                    grepl("^3344", NAICS) ~ "S",
                                    grepl("^3328|^332999", NAICS) ~ "M",
                                    grepl("^324|^325|^333|^424|^442|^561", NAICS) ~ "OI",
                                    TRUE ~ NA_character_),
         detailed_industry = case_when(grepl("^326", NAICS) ~ "Plastics and rubber products manufacturing",
                                       grepl("^221", NAICS) ~ "Sewage treatment facilities",
                                       grepl("^562", NAICS) ~ "Waste management and remediation services",
                                       grepl("^488", NAICS) ~ "Airport operations",
                                       grepl("^322", NAICS) ~ "Paper manufacturing",
                                       grepl("^323", NAICS) ~ "Printing and related support activities",
                                       grepl("^3344", NAICS) ~ "Semiconductor and other electronic component",
                                       grepl("^313", NAICS) ~ "Textile mills",
                                       grepl("^314110", NAICS) ~ "Carpet and rug mills",
                                       grepl("^314999", NAICS) ~ "All other miscellaneous textile product mills",
                                       grepl("^324", NAICS) ~ "Petroleum and coal products manufacturing",
                                       grepl("^325", NAICS) ~ "Chemical manufacturing",
                                       grepl("^3328", NAICS) ~ "Metal coating, engraving, heat treating and allied activities",
                                       grepl("^332999", NAICS) ~ "All other miscellaneous fabricated metal product manufacturing",
                                       grepl("^333316", NAICS) ~ "Photographic and photocopying equipment manufacturing",
                                       grepl("^333318", NAICS) ~ "Other commercial and service industry machinery manufacturing",
                                       grepl("^333249", NAICS) ~ "Other industrial machinery manufacturing",
                                       grepl("^424690", NAICS) ~ "Other chemical and allied products merchant wholesalers",
                                       grepl("^442291", NAICS) ~ "Window treatment stores",
                                       grepl("^561740", NAICS) ~ "Carpet and upholstery cleaning services",
                                       TRUE ~ NA_character_))

table(proj_businesses$detailed_industry, useNA = 'ifany')
table(proj_businesses$industry_group, useNA = 'ifany')
# proj_businesses %>%
#   st_write("../../raw_data/nh_industry_01042021.shp", append = FALSE)

# read in NH business information from FRS, downloaded from https://www.epa.gov/frs/epa-state-combined-csv-download-files
frs_businesses <- read_csv("../../raw_data/FRS/NH_NAICS_FILE.CSV") %>%
  left_join(read_csv("../../raw_data/FRS/NH_FACILITY_FILE.CSV"), by = "REGISTRY_ID") %>%
  mutate(NAICS = NAICS_CODE,
         CREATE_DATE = lubridate::dmy(CREATE_DATE)) %>%
  filter(CREATE_DATE <= as.Date('2017-10-31'),
         grepl("^22132|^313|^314110|^314999|^322|^323|^324|^325|^3328|^332999|^3344|^48811|^562|^326|^333318|^333316|^333249|^424690|^442291|^561740", NAICS)) %>%   
  distinct(LATITUDE83, LONGITUDE83, .keep_all = T) %>%
  # create industry group label based on NAICS code 
  mutate(industry_group = case_when(grepl("^313|^314", NAICS) ~ "T",
                                  grepl("^326", NAICS) ~ "Pl",
                                  grepl("^562|^221", NAICS) ~ "W",
                                  grepl("^488", NAICS) ~ "A",
                                  grepl("^322|^323", NAICS) ~ "Pr",
                                  grepl("^3344", NAICS) ~ "S",
                                  grepl("^3328|^332999", NAICS) ~ "M",
                                  grepl("^324|^325|^333|^424|^442|^561", NAICS) ~ "OI",
                                  TRUE ~ NA_character_),
       detailed_industry = case_when(grepl("^326", NAICS) ~ "Plastics and rubber products manufacturing",
                                     grepl("^221", NAICS) ~ "Sewage treatment facilities",
                                     grepl("^562", NAICS) ~ "Waste management and remediation services",
                                     grepl("^488", NAICS) ~ "Airport operations",
                                     grepl("^322", NAICS) ~ "Paper manufacturing",
                                     grepl("^323", NAICS) ~ "Printing and related support activities",
                                     grepl("^3344", NAICS) ~ "Semiconductor and other electronic component",
                                     grepl("^313", NAICS) ~ "Textile mills",
                                     grepl("^314110", NAICS) ~ "Carpet and rug mills",
                                     grepl("^314999", NAICS) ~ "All other miscellaneous textile product mills",
                                     grepl("^324", NAICS) ~ "Petroleum and coal products manufacturing",
                                     grepl("^325", NAICS) ~ "Chemical manufacturing",
                                     grepl("^3328", NAICS) ~ "Metal coating, engraving, heat treating and allied activities",
                                     grepl("^332999", NAICS) ~ "All other miscellaneous fabricated metal product manufacturing",
                                     grepl("^333316", NAICS) ~ "Photographic and photocopying equipment manufacturing",
                                     grepl("^333318", NAICS) ~ "Other commercial and service industry machinery manufacturing",
                                     grepl("^333249", NAICS) ~ "Other industrial machinery manufacturing",
                                     grepl("^424690", NAICS) ~ "Other chemical and allied products merchant wholesalers",
                                     grepl("^442291", NAICS) ~ "Window treatment stores",
                                     grepl("^561740", NAICS) ~ "Carpet and upholstery cleaning services",
                                     TRUE ~ NA_character_)) %>%
  filter(!is.na(industry_group), !is.na(LATITUDE83), !is.na(LONGITUDE83)) %>%
  dplyr::select(REGISTRY_ID, PRIMARY_NAME,  LATITUDE83, LONGITUDE83, NAICS, industry_group, detailed_industry) %>%
  st_as_sf(coords = c("LONGITUDE83", "LATITUDE83"), crs = 4269) %>% #NAD83
  # transform to be the same CRS as wells
  st_transform(st_crs(unique_wells)) 


table(frs_businesses$detailed_industry, useNA = 'ifany')
table(frs_businesses$industry_group, useNA = 'ifany')
# frs_businesses %>%
#    st_write("../../raw_data/frs_industry_01042021.shp", append = FALSE)

# ewg sites
afff_sites <- data.frame("name"=c("New Boston AFM", "Center Strafford Training Site", "Newington", "Pease Air Force Base"), 
                        'lat' = c(42.94953788784724,  43.27252715038138, 43.100428055835174, 43.080836771144526), 
                        'long' = c(-71.62159231164273,  -71.12722267231433, -70.83346568498517, -70.80057928425452)) %>%
  st_as_sf(coords = c("long", "lat"), crs = 4269) %>% #NAD83
  # transform to be the same CRS as wells
  st_transform(st_crs(unique_wells)) 

#afff_sites %>%
#    st_write("../../raw_data/afff_sites_01042021.shp", append = FALSE)


# well location and industry location are used in impact.py
ewg <- read_csv("../../modeling_data/ewg_impact_huc12.csv")%>%
  set_names(paste0('Impact', names(.))) %>%
  rename(StationID = ImpactStationID)

impact <- read_csv("../../modeling_data/potential_impact_huc12.csv")%>%
  set_names(paste0('Impact', names(.))) %>%
  rename(StationID = ImpactStationID) %>%
  left_join(ewg, by = "StationID")

frs <- read_csv("../../modeling_data/frs_impact_huc12.csv")%>%
  set_names(paste0('Impact', names(.))) %>%
  rename(StationID = ImpactStationID) %>%
  left_join(ewg, by = "StationID")

# number of wells with non-zero impact by detailed industry
# read_csv("../../modeling_data/potential_impact_huc12_detailed_indsutry.csv")%>%
#   pivot_longer(-StationID) %>%
#   mutate(value = if_else(value>0, 1, 0)) %>%
#   group_by(name) %>%
#   summarise(n = sum(value))

# Draw a bigger buffer for SG & TCI -------------------------------------------
# Industries for which airborne transport was confirmed, use 10km based on communication with NHDES
SG <- proj_businesses%>%
  filter(CONAME == "SAINT-GOBAIN PERFORMANCE PLSTC")
SG_buffer10km <-  SG%>%
  st_buffer(dist = 10000)
SG_buffer30km <-  SG%>%
  st_buffer(dist = 30000)

# Identify wells within the 10km radius buffer zone of SG
SG_wells <- st_join(unique_wells, SG_buffer10km, join = st_intersects) %>%
  filter(!is.na(NAICS))
SG_distances <- st_distance(SG_wells, SG)
SG_wells<- cbind(SG_wells,SG_distances) %>%
  mutate(SG_distances = as.numeric(SG_distances)) %>%
  mutate(ImpactPl = 1/exp(SG_distances/1000)) %>%
  dplyr::select(StationID, ImpactPl)

# Identify wells within the 30km radius buffer zone of SG
SG_wells_30k <- st_join(unique_wells, SG_buffer30km, join = st_intersects) %>%
  filter(!is.na(NAICS))
SG_distances <- st_distance(SG_wells_30k, SG)
SG_wells_30k<- cbind(SG_wells_30k,SG_distances) %>%
  mutate(SG_distances = as.numeric(SG_distances)) %>%
  mutate(ImpactPl = 1/exp(SG_distances/1000)) %>%
  dplyr::select(StationID, ImpactPl)

# TCI
TCI <- proj_businesses%>%
  filter(CONAME == "TEXTILES COATED INC")
TCI_buffer10km <-  TCI%>%
  st_buffer(dist = 10000)
TCI_buffer30km <-  TCI%>%
  st_buffer(dist = 30000)


TCI_wells <- st_join(unique_wells, TCI_buffer10km, join = st_intersects) %>%
  filter(!is.na(NAICS))
TCI_distances <- st_distance(TCI_wells, TCI)
TCI_wells<- cbind(TCI_wells,TCI_distances) %>%
  mutate(TCI_distances = as.numeric(TCI_distances)) %>%
  mutate(ImpactT = 1/exp(TCI_distances/1000)) %>%
  dplyr::select(StationID, ImpactT)


TCI_wells_30k <- st_join(unique_wells, TCI_buffer30km, join = st_intersects) %>%
  filter(!is.na(NAICS))
TCI_distances <- st_distance(TCI_wells_30k, TCI)
TCI_wells_30k<- cbind(TCI_wells_30k,TCI_distances) %>%
  mutate(TCI_distances = as.numeric(TCI_distances)) %>%
  mutate(ImpactT = 1/exp(TCI_distances/1000)) %>%
  dplyr::select(StationID, ImpactT)


# Merge -------------------------------------------------------------------
impact %>%
  dplyr::select(StationID, ImpactPl) %>%
  bind_rows(SG_wells) %>%
  group_by(StationID) %>%
  summarise(ImpactPl = sum(ImpactPl)) -> a

impact %>%
  dplyr::select(StationID, ImpactT) %>%
  bind_rows(TCI_wells) %>%
  group_by(StationID) %>%
  summarise(ImpactT = sum(ImpactT)) -> b

final_industries <- impact %>%
  dplyr::select(-c(ImpactPl, ImpactT)) %>%
  left_join(a, by = "StationID") %>%
  left_join(b, by = "StationID")

# sensitivity analysis for 30k buffer
impact %>%
  dplyr::select(StationID, ImpactPl) %>%
  bind_rows(SG_wells_30k) %>%
  group_by(StationID) %>%
  summarise(ImpactPl = sum(ImpactPl)) -> a

impact %>%
  dplyr::select(StationID, ImpactT) %>%
  bind_rows(TCI_wells_30k) %>%
  group_by(StationID) %>%
  summarise(ImpactT = sum(ImpactT)) -> b

final_industries_30k <- impact %>%
  dplyr::select(-c(ImpactPl, ImpactT)) %>%
  left_join(a, by = "StationID") %>%
  left_join(b, by = "StationID")


sapply(final_industries, function(x){sum(is.na(x))})

sapply(final_industries_30k, function(x){sum(is.na(x))})
# should not have any missing
options(scipen = 0)
options(digits = 3)
# compare two buffer sizes
p1<-final_industries%>%
  left_join(final_industries_30k, by = "StationID", suffix = c(".10k", ".30k")) %>%
  #filter(ImpactPl10k != ImpactPl30k | ImpactT10k != ImpactT30k) %>%
  dplyr::select(contains("ImpactPl") | contains("ImpactT")) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
  separate(variable, into = c("industry", "distance")) %>%
  ggplot(aes(x = value)) +
  geom_density(aes(fill = distance), alpha = 0.4) +
  facet_wrap(~ industry, scales = "free",  
             labeller = labeller(industry = c(ImpactPl = "Plastics and rubber",
                                              ImpactT ="Textile manufacturing"))) +
  scale_fill_brewer(palette = "Set1")+
  scale_x_continuous(name = "Industrial impact") +
  theme_classic() +
  labs(fill=NULL, y = "Density")+
  theme(text = element_text(size = 16),
        strip.text = element_text(size = 16),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        legend.position = "none")
p2<-final_industries%>%
  left_join(final_industries_30k, by = "StationID", suffix = c(".10k", ".30k")) %>%
  #filter(ImpactPl10k != ImpactPl30k | ImpactT10k != ImpactT30k) %>%
  dplyr::select(contains("ImpactPl") | contains("ImpactT")) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
  separate(variable, into = c("industry", "distance")) %>%
  ggplot(aes(x = value)) +
  geom_density(aes(fill = distance), alpha = 0.4) +
  facet_wrap(~ industry, scales = "free",  
             labeller = labeller(industry = c(ImpactPl = "Plastics and rubber",
                                              ImpactT ="Textile manufacturing"))) +
  scale_fill_brewer(palette = "Set1")+
  scale_x_continuous(trans = "log10",
    name = "Industrial impact") +
  theme_classic() +
  labs(fill='Buffer distance', y = "Density")+
  theme(text = element_text(size = 16),
        strip.text = element_text(size = 16),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        legend.title = element_text(size = 16),
        legend.key.size = unit(1, "cm"),
        legend.text = element_text(size = 16),
        legend.position="bottom")
ggpubr::ggarrange(p1, p2, 
          labels = c("A", "B"),
          ncol = 1, nrow = 2)
ggsave("../../output/Figure_sens_buffer_size.png",width = 9,
       height = 9,
       units = "in")

# Save --------------------------------------------------------------------
saveRDS(final_industries, '../../modeling_data/final_industries01052021.rds')


# tmap
# options(tigris_class = "sf")
# nh<-tigris::counties(state = "New Hampshire", cb = TRUE)
# tmap_mode("plot")
# 
# tm_shape(nh) + 
#   tm_polygons(border.col = "white") +
#   tm_shape(unique_wells) +
#   tm_bubbles(size = 0.1, border.col = "transparent", 
#              alpha = 0.5, col = "grey30") +
#   tm_shape(SG_buffer30km) +
#   tm_polygons(border.col = "blue", alpha = 0) +
#   tm_shape(SG_wells_30km) +
#   tm_bubbles(size = 0.1, border.col = "blue",
#              alpha = 0.5, col = "grey70") +
#   tm_shape(SG_buffer10km) +
#   tm_polygons(border.col = "red", alpha = 0) +
#   tm_shape(SG_wells) +
#   tm_bubbles(size = 0.1, border.col = "red", 
#              alpha = 0.5, col = "grey70") +
#   tm_layout(legend.title.size = 1.8,
#               legend.text.size = 1.1,
#               legend.outside = T,
#               frame = F)
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
