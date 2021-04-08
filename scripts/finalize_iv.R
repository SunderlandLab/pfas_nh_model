# Finalize Independent Vars (IV's), as there should only be 1 row for each station
# Library -----------------------------------------------------------------

library(tidyverse)

# Load --------------------------------------------------------------------
# Raw datasets
bedrock <- read.csv("../../raw_data/bedrock_extraction.csv", header = TRUE, sep = ",", na.strings = c("", " ", "NA"))
precip <- read.csv("../../raw_data/precip_PFAS.csv", header = TRUE, sep = ",", na.strings = c("", " ", "NA"))
recharge <- read.csv("../../raw_data/recharge1122.csv", header = TRUE, sep = ",", na.strings = c("", " ", "NA"))
# Processed RDS's
final_industries <- readRDS('../../modeling_data/final_industries.rds')
final_soildata <- readRDS('../../modeling_data/final_soildata.rds')
PFASwells <- readRDS('../../modeling_data/PFASwells.rds')
PFASwells$PFASreg <- PFASwells %>%
  dplyr::select(ends_with("reg")) %>%
  rowSums(na.rm = T)
PFASwells$PFASfinal <- PFASwells %>%
  dplyr::select(ends_with("final")) %>%
  as.matrix() %>%
  matrixStats::rowMaxs( na.rm = T)

# Remove duplicates and aggregate -----------------------------------------

unique_bedrock <- unique(bedrock)
unique_recharge <- unique(recharge[,-1])

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

indvars <- merged_variables[,c(1,15:length(merged_variables))]
unique_ivs <- unique(indvars)

sapply(unique_ivs, function(x){sum(is.na(x))})

# Clean NA's & blanks -----------------------------------------------------
# (For quantitative variables only)
# Replace each variable's NAs & 0's with the mean for that variable

# for (var in 2:(ncol(unique_ivs) - 3)) {
#   avg <- mean(unique_ivs[,var], na.rm = TRUE)
#   unique_ivs[is.na(unique_ivs[, var]), var] <- avg
#   unique_ivs[unique_ivs[,var] == 0, var] <- avg
# }

#replacing with column mean
#where var is iterating through the precip, recharge, and soil columns
#soil properties, impute missing as column average
for (var in c(2:3,14:ncol(unique_ivs))) {
  if (var <= 3)
  {
    avg <- mean(unique_ivs[,var], na.rm = TRUE)
    unique_ivs[is.na(unique_ivs[, var]), var] <- avg
  }
  else
  {
    avg <- mean(unique_ivs[!unique_ivs[,var] %in% c(0,NA), var])
    unique_ivs[unique_ivs[,var] %in% c(0,NA), var] <- avg
  }
}

#impact from industry, impute missing as zero
for(var in c(4:13)) {
  unique_ivs[is.na(unique_ivs[,var]), var] <- 0
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
#replacing NAs for categorical variables with most common category
unique_ivs[is.na(unique_ivs$bedrock),"bedrock"] <- "Zmz"
#only 19 NAs for each of these, but also random rows with not just NAs but missing values entirely ("")...assuming those are essentially NAs
#there are 92 of these for drclassdcd, and 100 for hydgrpdcd
unique_ivs[is.na(unique_ivs$drclassdcd),"drclassdcd"] <- "Excessively drained"
unique_ivs[unique_ivs$drclassdcd=="","drclassdcd"]<- "Excessively drained"
unique_ivs[is.na(unique_ivs$hydgrpdcd),"hydgrpdcd"] <- "A"
unique_ivs[unique_ivs$hydgrpdcd=="","hydgrpdcd"] <- "A"

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
unique_ivs$drclassdcdE <- as.factor(unique_ivs$drclassdcdE)
unique_ivs$drclassdcdW <- as.factor(unique_ivs$drclassdcdW)
unique_ivs$drclassdcdP <- as.factor(unique_ivs$drclassdcdP)
unique_ivs$hydgrpdcdA <- as.factor(unique_ivs$hydgrpdcdA)
unique_ivs$hydgrpdcdB <- as.factor(unique_ivs$hydgrpdcdB)
unique_ivs$hydgrpdcdC <- as.factor(unique_ivs$hydgrpdcdC)

#combine M, T, Pr, S into OI
unique_ivs<-unique_ivs %>%
  mutate(ImpactOI = ImpactOI + ImpactS + ImpactPr + ImpactM + ImpactT) %>%
  dplyr::select(-c(ImpactS, ImpactPr, ImpactM, ImpactT))

# summarize the number of wells with non-zero impact from each industry type
unique_ivs %>%
  dplyr::select(contains("Impact")) %>%
  map(., function(x){sum(x!=0)})
M<-unique_ivs %>%
  dplyr::select_if(is.numeric) %>%
  cor()

corrplot::corrplot(M, method = "number", type = "lower", diag = F)

# Remove collinear columns ----------------------------------------------
rm <- c("ph1to1h2o_r", "aws0_999", "drclassdcdW","drclassdcdP","drclassdcdE", "hydgrpdcdB","hydgrpdcdC","hydgrpdcdD")
unique_ivs <- unique_ivs[, -which(names(unique_ivs) %in% rm)]

# Save --------------------------------------------------------------------

saveRDS(merged_variables, '../../modeling_data/merged_variables.rds')
saveRDS(unique_ivs, '../../modeling_data/unique_ivs.rds')
