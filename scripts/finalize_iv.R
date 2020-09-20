# Finalize Independent Vars (IV's), as there should only be 1 row for each station
# Library -----------------------------------------------------------------

library(tidyverse)

# Load --------------------------------------------------------------------
# Raw datasets
bedrock <- read.csv("raw_data/bedrock_extraction.csv", header = TRUE, sep = ",", na.strings = c("", " ", "NA"))
recharge <- read.csv("raw_data/recharge.csv", header = TRUE, sep = ",", na.strings = c("", " ", "NA"))
precip <- read.csv("raw_data/precip_PFAS.csv", header = TRUE, sep = ",", na.strings = c("", " ", "NA"))
recharge <- read.csv("raw_data/recharge.csv", header = TRUE, sep = ",", na.strings = c("", " ", "NA"))
# Processed RDS's
final_industries <- readRDS('modeling_data/final_industries.rds')
final_soildata <- readRDS('modeling_data/final_soildata.rds')
PFASwells <- readRDS('modeling_data/PFASwells.rds')

# Remove duplicates and aggregate -----------------------------------------

unique_bedrock <- unique(bedrock)
unique_recharge <- unique(recharge)

agg_precip <- aggregate(precip$Precip, by = list(StationID = precip$StationID), FUN = mean)
colnames(agg_precip)[colnames(agg_precip) == "x"] <- "precip"


# Merge -------------------------------------------------------------------

merged_variables <- Reduce(function(dtf1, dtf2) merge(dtf1, dtf2, by = "StationID", all.x = TRUE),
                           list(PFASwells, 
                                agg_precip, 
                                unique_recharge, 
                                final_industries, 
                                final_soildata, 
                                unique_bedrock))

indvars <- merged_variables[,c(1,12:length(merged_variables))]
unique_ivs <- unique(indvars)


# Clean NA’s & blanks -----------------------------------------------------
# (For auantitative variables only)
# Replace each variable's NAs & 0's with the mean for that variable

for (var in 2:(ncol(unique_ivs) - 3)) {
  avg <- mean(unique_ivs[,var], na.rm = TRUE)
  unique_ivs[is.na(unique_ivs[, var]), var] <- avg
  unique_ivs[unique_ivs[,var] == 0, var] <- avg
}

# Condense number of categories for categorical variables -----------------

unique_ivs$hydgrpdcd[unique_ivs$hydgrpdcd == "A/D"] <- "D"
unique_ivs$hydgrpdcd[unique_ivs$hydgrpdcd == "B/D"] <- "D"
unique_ivs$hydgrpdcd[unique_ivs$hydgrpdcd == "C/D"] <- "D"
unique_ivs$drclassdcd[unique_ivs$drclassdcd == "Somewhat excessively drained"] <- "Excessively drained"
unique_ivs$drclassdcd[unique_ivs$drclassdcd == "Moderately well drained"] <- "Well drained"
unique_ivs$drclassdcd[unique_ivs$drclassdcd == "Very poorly drained"] <- "Poorly drained"


# Recode as binary indicators ---------------------------------------------
# Bedrock
unique_ivs$bedrock <- as.character(unique_ivs$bedrock)
unique_ivs$bedrock <- 
  ifelse(unique_ivs$bedrock %in% c("OZrb","SOb","SObc","SObg","SOe","SOec","SOk","Zmz"),
         1, 0)
colnames(unique_ivs)[colnames(unique_ivs) == "bedrock"] <- "bedrock_M"
unique_ivs$bedrock_M <- as.numeric(unique_ivs$bedrock_M)

# Others (doing this way because there are NA's)
unique_ivs$hydgrpdcdD = unique_ivs$hydgrpdcdC = unique_ivs$hydgrpdcdB = 
  unique_ivs$hydgrpdcdA = unique_ivs$drclassdcdP = unique_ivs$drclassdcdW =
  unique_ivs$drclassdcdE <- rep(0, nrow(unique_ivs))

# unique_ivs$bedrockP = unique_ivs$bedrockCV = unique_ivs$bedrockCM = 
#   unique_ivs$bedrockM <- rep(0, nrow(unique_ivs))


unique_ivs$drclassdcdE[unique_ivs$drclassdcd == "Excessively drained"] <- 1
unique_ivs$drclassdcdW[unique_ivs$drclassdcd == "Well drained"] <- 1
unique_ivs$drclassdcdP[unique_ivs$drclassdcd == "Poorly drained"] <- 1
unique_ivs$hydgrpdcdA[unique_ivs$hydgrpdcd == "A"] <- 1
unique_ivs$hydgrpdcdB[unique_ivs$hydgrpdcd == "B"] <- 1
unique_ivs$hydgrpdcdC[unique_ivs$hydgrpdcd == "C"] <- 1
unique_ivs$hydgrpdcdD[unique_ivs$hydgrpdcd == "D"] <- 1
# unique_ivs$bedrockM[unique_ivs$bedrock == "Merrimack"] <- 1
# unique_ivs$bedrockCM[unique_ivs$bedrock == "Central Maine"] <- 1
# unique_ivs$bedrockCV[unique_ivs$bedrock == "Connecticut Valley"] <- 1
# unique_ivs$bedrockP[unique_ivs$bedrock == "Plutonic"] <- 1


# Remove extraneous columns
unique_ivs$drclassdcd = unique_ivs$hydgrpdcd = unique_ivs$bedrock <- NULL

# Coerce into factor 
unique_ivs$bedrock_M <- as.factor(unique_ivs$bedrock_M)
unique_ivs$drclassdcdW <- as.factor(unique_ivs$drclassdcdW)
unique_ivs$drclassdcdP <- as.factor(unique_ivs$drclassdcdP)
unique_ivs$hydgrpdcdB <- as.factor(unique_ivs$hydgrpdcdB)
unique_ivs$hydgrpdcdC <- as.factor(unique_ivs$hydgrpdcdC)



# Remove unnecessary columns ----------------------------------------------
# Option 1:
# (previously unique_ivs <- unique_ivs[,-c(10,12:14,17,21,24:25,32,35,38)])
rm <- c("sandtotal_r", "sandco_r", "sandmed_r", "sandfine_r", "siltco_r", "ksat_r", 
       "ph1to1h2o_r", "aws0_999", "drclassdcdE", "hydgrpdcdA","hydgrpdcdD") 
unique_ivs <- unique_ivs[, -which(names(unique_ivs) %in% rm)] 

# Option 2: use total number of industries 
# (previously unique_ivs <- unique_ivs[,-c(5,7:9,12,16,19:20,27,30,33)])

# rm2 <- c("ImpactPl2", "ImpactPr2", "ImpactS2", "ImpactT2", "sandco_r", "silttotal_r",
#          "claytotal_r", "dbthirdbar_r", "slopegradwta", "hzdep", "drclassdcdW")
# unique_ivs <- unique_ivs[,-rm]

# Save --------------------------------------------------------------------

saveRDS(merged_variables, 'modeling_data/merged_variables.rds')
saveRDS(unique_ivs, 'modeling_data/unique_ivs.rds')


