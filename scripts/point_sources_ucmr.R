# extract point sources in NH from the UCMR3 paper

# Library -----------------------------------------------------------------

library(tidyverse)
library(rgeos)
library(rgdal)
library(sp)
library(sf)
library(raster)
library(tmap)
library(geojsonR)
library(geojsonsf)

# NH state outline
nh_state <- tigris::states(cb=TRUE) %>%
    filter(NAME == "New Hampshire") 
# read in well locations
unique_wells <- read.csv("../../raw_data/actual_unique.csv", header = TRUE, sep = ",")[,-1] %>%
    distinct_at(.vars = c("Longitude", "Latitude", "StationID")) %>%
    filter(!is.na(Latitude))%>%
    # convert data frame to sf
    st_as_sf(coords = c("Longitude", "Latitude")) %>%
    st_set_crs("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0") %>%
    st_transform("+proj=utm +zone=18 +datum=WGS84 +units=m")

find_within_nh <- function(layername){
    # read shapefile
    shp<- st_read(dsn = "../../raw_data",  layer = layername) %>%
        st_transform(st_crs(nh_state))
    
    # filter down to NH
    shp_nh <- shp %>%
        st_join(nh_state, join = st_intersects) %>%
        filter(!is.na(NAME))
    
    return(shp_nh)
}

airport_nh <- find_within_nh("Airports139")%>%
    mutate(industry_group = "Airports",
           ID = as.character(ID)) %>%
    dplyr::select(ID, geometry, industry_group)
# confirm there are 2 part 139 airports in NH using FAA data
# https://www.faa.gov/airports/airport_safety/part139_cert/

# WWTP from Xianming
wwtp_nh <- readxl::read_excel("../../raw_data/PFOSEmissionEstimation_WWTP_USA.xlsx") %>%
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4269) %>%
  st_join(nh_state, join = st_intersects) %>%
  filter(!is.na(NAME)) %>%
  mutate(industry_group = "WWTP",
         ID = FACILITY_NAME)%>%
  dplyr::select(ID, geometry, industry_group)


dod_nh <- find_within_nh("DoD268")
# check with EWG list, this is not as complete, use EWG instead
# ewg sites
dod_nh <- data.frame("ID"=c("New Boston AFM", "Center Strafford Training Site", "Newington", "Pease Air Force Base", "AASF Concord"), 
                         "industry_group" = "Military sites",
                         'lat' = c(42.94953788784724,  43.27252715038138, 43.100428055835174, 43.080836771144526, 43.21026), 
                         'long' = c(-71.62159231164273,  -71.12722267231433, -70.83346568498517, -70.80057928425452, -71.51248)) %>%
    st_as_sf(coords = c("long", "lat"), crs = 4269) 


# read in NH business information from FRS, downloaded from https://www.epa.gov/frs/epa-state-combined-csv-download-files
frs_businesses <- read_csv("../../raw_data/FRS/NH_NAICS_FILE.CSV") %>%
  left_join(read_csv("../../raw_data/FRS/NH_FACILITY_FILE.CSV"), by = "REGISTRY_ID") %>%
  mutate(NAICS = NAICS_CODE,
         CREATE_DATE = lubridate::dmy(CREATE_DATE)) %>%
  filter(CREATE_DATE <= as.Date('2017-10-31'),
         grepl("^313|^322|^323|^324|^3255|^32591|^3328|^3344", NAICS)) %>%
  distinct(LATITUDE83, LONGITUDE83, .keep_all = T) %>%
  # create industry group label based on NAICS code
  mutate(industry_group = case_when(grepl("^313", NAICS) ~ "T",
                                    grepl("^322|^323", NAICS) ~ "Pr",
                                    grepl("^3344", NAICS) ~ "S",
                                    grepl("^3328", NAICS) ~ "M",
                                    TRUE ~ "OI")) %>%
  filter(!is.na(industry_group), !is.na(LATITUDE83), !is.na(LONGITUDE83)) %>%
  st_as_sf(coords = c("LONGITUDE83", "LATITUDE83"), crs = 4269) %>%
  #as_Spatial()%>%
  dplyr::select(ID = "PRIMARY_NAME", industry_group, geometry)
#frs_businesses %>% 
#  writeOGR("../../raw_data", layer = "frs", driver = "ESRI Shapefile")
#combine point sources and write out
# table s3
a<-read_csv("../../raw_data/FRS/NH_NAICS_FILE.CSV") %>%
  left_join(read_csv("../../raw_data/FRS/NH_FACILITY_FILE.CSV"), by = "REGISTRY_ID") %>%
  mutate(NAICS = NAICS_CODE,
         CREATE_DATE = lubridate::dmy(CREATE_DATE)) %>%
  filter(CREATE_DATE <= as.Date('2017-10-31'),
         grepl("^313|^322|^323|^324|^3255|^32591|^3328|^3344", NAICS)) %>%
  distinct(LATITUDE83, LONGITUDE83, .keep_all = T) %>%
  # create industry group label based on NAICS code
  mutate(industry_group = case_when(grepl("^313", NAICS) ~ "T",
                                    grepl("^322", NAICS) ~ "Paper",
                                    grepl("^323", NAICS) ~ "Printing",
                                    grepl("^324", NAICS) ~ "Petroleum",
                                    grepl("^3255", NAICS) ~ "Chemical",
                                    grepl("^32591", NAICS) ~ "Printing ink",
                                    grepl("^3344", NAICS) ~ "S",
                                    grepl("^3328", NAICS) ~ "M",
                                    TRUE ~ "OI")) 
table(a$industry_group)

bind_rows(airport_nh, wwtp_nh, dod_nh, frs_businesses) %>%
  st_write("../../raw_data/nh_ucmr_01232021.shp", append = FALSE)


#impact.py

# read in impact
impact <- read_csv("../../modeling_data/potential_impact_huc12.csv")%>%
set_names(paste0('Impact', names(.))) %>%
    rename(StationID = ImpactStationID,
           ImpactMilitary = `ImpactMilitary sites`) 

# Draw a bigger buffer for SG & TCI -------------------------------------------
proj_businesses <- st_read(dsn = "../../raw_data/NH_businesses_2016", layer = "NH_businesses_2016") %>%
    filter(grepl("^22132|^313|^314110|^314999|^322|^323|^324|^325|^3328|^332999|^3344|^48811|^562|^326|^333318|^333316|^333249|^424690|^442291|^561740", NAICS))%>%
    dplyr::select(c("OBJECTID", "CONAME", "NAICS"))%>%
    # transform to be the same CRS as wells
    st_transform(st_crs(unique_wells))
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
    mutate(d = as.numeric(SG_distances)) %>%
    mutate(ImpactPlastics = 1/exp(d/1000)) %>%
    dplyr::select(StationID, ImpactPlastics)

# Identify wells within the 30km radius buffer zone of SG
SG_wells_30k <- st_join(unique_wells, SG_buffer30km, join = st_intersects) %>%
  filter(!is.na(NAICS))
SG_distances <- st_distance(SG_wells_30k, SG)
SG_wells_30k<- cbind(SG_wells_30k,SG_distances) %>%
  mutate(d = as.numeric(SG_distances)) %>%
  mutate(ImpactPlastics = 1/exp(d/1000)) %>%
  dplyr::select(StationID, ImpactPlastics)

# TCI
TCI <- proj_businesses%>%
    filter(CONAME == "TEXTILES COATED INC")
TCI_buffer10km <-  TCI%>%
    st_buffer(dist = 10000)
TCI_buffer30km <-  TCI%>%
  st_buffer(dist = 30000)

# identify wells within 10km of TCI
TCI_wells <- st_join(unique_wells, TCI_buffer10km, join = st_intersects) %>%
    filter(!is.na(NAICS))
TCI_distances <- st_distance(TCI_wells, TCI)
TCI_wells<- cbind(TCI_wells,TCI_distances) %>%
    mutate(d = as.numeric(TCI_distances)) %>%
    mutate(ImpactTextile = 1/exp(d/1000)) %>%
    dplyr::select(StationID, ImpactTextile)

# identify wells within 30km of TCI
TCI_wells_30k <- st_join(unique_wells, TCI_buffer30km, join = st_intersects) %>%
  filter(!is.na(NAICS))
TCI_distances <- st_distance(TCI_wells_30k, TCI)
TCI_wells_30k<- cbind(TCI_wells_30k,TCI_distances) %>%
  mutate(d = as.numeric(TCI_distances)) %>%
  mutate(ImpactTextile = 1/exp(d/1000)) %>%
  dplyr::select(StationID, ImpactTextile)


# Merge -------------------------------------------------------------------
final_industries <- impact %>%
  left_join(TCI_wells %>% st_drop_geometry(), by = "StationID") %>%
  left_join(SG_wells %>% st_drop_geometry(), by = "StationID") %>%
  mutate_all(funs(replace_na(.,0))) %>%
  dplyr::select(c("StationID", contains("Impact")))

final_industries%>%
  mutate(ImpactOI = ImpactOI + ImpactS + ImpactPr + ImpactM + ImpactT) %>%
  dplyr::select(-c(ImpactS, ImpactPr, ImpactM, ImpactT)) %>%
      pivot_longer(-StationID) %>%
      mutate(value = if_else(value>0, 1, 0)) %>%
      group_by(name) %>%
      summarise(n = sum(value),
                prop = n/n())
    
# Save --------------------------------------------------------------------
saveRDS(final_industries, '../../modeling_data/final_industries.rds')

# sensitivity analysis of buffer size
df_a <- left_join(SG_wells %>% st_drop_geometry(), 
                  TCI_wells %>% st_drop_geometry(), 
                  by = "StationID")
df_b <- left_join(SG_wells_30k %>% st_drop_geometry(), 
                  TCI_wells_30k %>% st_drop_geometry(), 
                  by = "StationID")
options(scipen = 0)
options(digits = 3)
# compare two buffer sizes
p1<-df_a%>%
  left_join(df_b, by = "StationID", suffix = c(".10k", ".30k")) %>%
  #filter(ImpactPl10k != ImpactPl30k | ImpactT10k != ImpactT30k) %>%
  dplyr::select(contains("ImpactPlastics") | contains("ImpactTextile")) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
  separate(variable, into = c("industry", "distance")) %>%
  ggplot(aes(x = value)) +
  geom_density(aes(fill = distance), alpha = 0.4) +
  facet_wrap(~ industry, scales = "free",  
             labeller = labeller(industry = c(ImpactPlastics = "Plastics and rubber",
                                              ImpactTextile ="Textile manufacturing"))) +
  scale_fill_brewer(palette = "Set1")+
  scale_x_continuous(name = bquote('Industrial impact'~(km^-1))) +
  theme_classic() +
  labs(fill=NULL, y = "Density")+
  theme(text = element_text(size = 16),
        strip.text = element_text(size = 16),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        legend.position = "bottom")
# p2<-df_a%>%
#   left_join(df_b, by = "StationID", suffix = c(".10k", ".30k")) %>%
#   #filter(ImpactPl10k != ImpactPl30k | ImpactT10k != ImpactT30k) %>%
#   dplyr::select(contains("ImpactPlastics") | contains("ImpactTextile")) %>%
#   pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
#   separate(variable, into = c("industry", "distance")) %>%
#   ggplot(aes(x = value)) +
#   geom_density(aes(fill = distance), alpha = 0.4) +
#   facet_wrap(~ industry, scales = "free",  
#              labeller = labeller(industry = c(ImpactPlastics = "Plastics and rubber",
#                                               ImpactTextile ="Textile manufacturing"))) +
#   scale_fill_brewer(palette = "Set1")+
#   scale_x_continuous(trans = "log10",
#                      name = bquote('Industrial impact'~(km^-1))) +
#   theme_classic() +
#   labs(fill='Buffer distance', y = "Density")+
#   theme(text = element_text(size = 16),
#         strip.text = element_text(size = 16),
#         axis.text.x = element_text(size = 16),
#         axis.text.y = element_text(size = 16),
#         legend.title = element_text(size = 16),
#         legend.key.size = unit(1, "cm"),
#         legend.text = element_text(size = 16),
#         legend.position="bottom")
# ggpubr::ggarrange(p1, p2, 
#                   labels = c("A", "B"),
#                   ncol = 1, nrow = 2)
ggsave(plot = p1, "../../output/Figure_sens_buffer_size.png",width = 9,
       height = 5,
       units = "in")

# visualize point sources
industries_nh<-bind_rows(airport_nh, wwtp_nh, dod_nh, frs_businesses,
          proj_businesses%>%
            filter(CONAME == "TEXTILES COATED INC") %>%
            mutate(industry_group = "Textiles") %>%
            dplyr::select(ID = CONAME, industry_group, geometry) %>%
            st_transform(st_crs(airport_nh)),
          proj_businesses%>%
            filter(CONAME == "SAINT-GOBAIN PERFORMANCE PLSTC")  %>%
            mutate(industry_group = "Plastics") %>%
            dplyr::select(ID = CONAME, industry_group, geometry) %>%
            st_transform(st_crs(airport_nh)) 
          ) %>%
  mutate(industry_group = if_else(industry_group %in% c("M", "OI", "Pr", "S", "T"), "Other industries", industry_group))
  
tmap_mode("plot")
m<-tm_shape(nh_state) +
  tm_fill() +
  tm_shape(unique_wells) +
  tm_dots(col = "white") +
  tm_shape(industries_nh) +
  tm_symbols(col = "industry_group", scale = 0.5) +
  tm_facets(by=c("industry_group"), ncol  = 3, showNA = F, free.coords= F) +
  tm_layout(#panel.labels = c(),
            legend.show = F,
            scale = 2)
tmap::tmap_save(m,"../../output/FigureSX_major_point_sources_in_NH.png",width = 10,
                height = 7,
                units = "in")
