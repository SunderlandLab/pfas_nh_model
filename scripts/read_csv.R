# Read in csv's and clean compound data
# Library -----------------------------------------------------------------
library(tidyverse)

# Load --------------------------------------------------------------------

PFASwells1 <- read_csv("../../raw_data/PFAS_Ronly.csv")
pfoapfhxa <- read_csv("../../raw_data/PFOAPFHXA.csv")


# Remove wells with very high detection limits ----------------------------

pfoapfhxa <- pfoapfhxa%>%filter(StationID != "MTBE_2725")
# This is 1531010_008
pfoapfhxa <- pfoapfhxa[-which(pfoapfhxa$PFOAnumeric == 18.5),] 
# This is 1531010_009
pfoapfhxa <- pfoapfhxa[-which(pfoapfhxa$PFOAnumeric == 17.9),] 


# Remove incorrect original columns for PFOA/PFHxA ------------------------

PFASwells1 <- PFASwells1[,-c(2:3)]
PFASwells1$StationID <- as.character(PFASwells1$StationID)
pfoapfhxa$StationID <- as.character(pfoapfhxa$StationID)



# Save --------------------------------------------------------------------

saveRDS(PFASwells1, '../../modeling_data/PFASwells1.rds')
saveRDS(pfoapfhxa, '../../modeling_data/pfoapfhxa.rds')


