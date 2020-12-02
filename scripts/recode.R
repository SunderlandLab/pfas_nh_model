# Recode chemical data
# Library -----------------------------------------------------------------

library(tidyverse)

# Load --------------------------------------------------------------------

PFASwells1 <- readRDS('../../modeling_data/PFASwells1.rds')
pfoapfhxa <- readRDS('../../modeling_data/pfoapfhxa.rds')


# Recode: 0 = below detection limit, 1 = above detection limit ------------
# Interpretation of original columns: 
# - results = concentration, qualifier = "<" if non detect, numeric = detection limit
# - Any well with concentration below median DL for compound is considered a 
# nondetect with value recoded to median/sqrt(2) for purposes of summary statistics
# 
# PFOA: median = 2, recoded value = 1.414
# PFHXA: median = 4.5, recoded value = 3.182 
# PFPEA: median = 4.5, recoded value = 3.182 - note: we end up not needing to throw out any PFPEA!
# PFHPA: median = 2, recoded value = 1.414 
# PFOS: median = 4, recoded value = 2.828


recode <- function(PFAS, data){
  numeric_name <- paste0(PFAS, 'numeric') 
  qualifier_name <- paste0(PFAS, 'qualifier')
  results_name <- paste0(PFAS, 'results')
  med <- median(data[[numeric_name]][data[[qualifier_name]] == '<'], na.rm = T)
  # Note: opted to round cutoff to replicate original data, is possible to include full float
  # 03/19/19 update: corrected previous mistake of recoding values below med/sqrt(2); should recode values below med
  recode_value <- round(med/sqrt(2), digits = 3)
  data[[qualifier_name]][data[[qualifier_name]] == '' & data[[results_name]] < med] <- '<'
  data[[results_name]][data[[results_name]] < med] <- recode_value
  return(data)
}

#LODs for PFOS
PFASwells1 %>% 
  filter(PFOSqualifier == "<") %>%
  dplyr::select(StationID, PFOSnumeric) %>%
  distinct() ->a

#
pfoapfhxa <- recode('PFOA', pfoapfhxa)
pfoapfhxa <- recode('PFHXA', pfoapfhxa)
PFASwells1 <- recode('PFPEA', PFASwells1)
PFASwells1 <- recode('PFHPA', PFASwells1)
PFASwells1 <- recode('PFOS', PFASwells1)

# PFHPA: Remove wells that are NDs with DLs of over 10 ---------------------

nds <- which(PFASwells1$PFHPAqualifier == "<" & PFASwells1$PFHPAnumeric > 10)
PFASwells1[nds,"PFHPAresults"] <- NA
PFASwells1[nds,"PFHPAqualifier"] <- ""
PFASwells1[nds,"PFHPAnumeric"] <- NA


# PFOS: Remove wells that are NDs with DLs of over 20 ---------------------

nds <- which(PFASwells1$PFOSqualifier == "<" & PFASwells1$PFOSnumeric > 20)
PFASwells1[nds,"PFOSresults"] <- NA
PFASwells1[nds,"PFOSqualifier"] <- ""
PFASwells1[nds,"PFOSnumeric"] <- NA


# Create binary classifier for detect/non-detect --------------------------

# Initialize columns
pfoapfhxa$PFOAfinal = pfoapfhxa$PFHXAfinal <- NA
PFASwells1$PFPEAfinal = PFASwells1$PFHPAfinal = PFASwells1$PFOSfinal <- NA

code_detect <- function(PFAS, data){
  numeric_name <- paste0(PFAS, 'numeric') 
  qualifier_name <- paste0(PFAS, 'qualifier')
  results_name <- paste0(PFAS, 'results')
  final_name <- paste0(PFAS, 'final')
  data[[final_name]][data[[results_name]] & data[[qualifier_name]] == ""] <- 1
  data[[final_name]][data[[qualifier_name]] == '<'] <- 0
  return(data)
}

pfoapfhxa <- code_detect('PFOA', pfoapfhxa)
pfoapfhxa <- code_detect('PFHXA', pfoapfhxa)
PFASwells1 <- code_detect('PFPEA', PFASwells1)
PFASwells1 <- code_detect('PFHPA', PFASwells1)
PFASwells1 <- code_detect('PFOS', PFASwells1)

# Remove unnecessary 'qualifier' and 'numeric' columns
pfoapfhxa <- pfoapfhxa[ , -which(str_detect(names(pfoapfhxa), 'qualifier'))]
pfoapfhxa <- pfoapfhxa[ , -which(str_detect(names(pfoapfhxa), 'numeric'))]
PFASwells1 <- PFASwells1[ , -which(str_detect(names(PFASwells1), 'qualifier'))]
PFASwells1 <- PFASwells1[ , -which(str_detect(names(PFASwells1), 'numeric'))]


# Aggregate ---------------------------------------------------------------
# Create separate data frames for each compound & store in list
compounds <- list('PFOA', 'PFHXA', 'PFPEA', 'PFHPA', 'PFOS') %>% set_names()

for (i in 1:length(compounds)) {
  if (i < 3) { 
    compounds[[i]] <- pfoapfhxa[, c('StationID', grep(compounds[[i]], names(pfoapfhxa), value = T))]
  } else {
    compounds[[i]] <- PFASwells1[, c('StationID', grep(compounds[[i]], names(PFASwells1), value = T))]
  }
}

# Aggregate by well such that each well only has one value
compounds <- map(compounds, function(df){aggregate(.~StationID, df, FUN = mean)})

# Combine into 1 data frame 
PFASwells <- Reduce(function(dtf1, dtf2) merge(dtf1, dtf2, by = "StationID", all = TRUE),
                    compounds)

# Aggregate results in values other than 0 or 1, so recodeback to 0/1 based on cutoff of 0.25
# for (col in grep('final', names(PFASwells), value = T)) {
#   PFASwells[[col]][PFASwells[[col]] <= 0.25] <- 0
#   PFASwells[[col]][PFASwells[[col]] > 0] <- 1
# }

#03/19/19 update: using old code for now because of the discovery of a mistake due to aggregation.
PFASwells$PFOAfinal[PFASwells$PFOAfinal <= 0.25] <- 0
PFASwells$PFOAfinal[PFASwells$PFOAfinal > 0.25 & PFASwells$PFOAresults < 2] <- 0
PFASwells$PFOAfinal[PFASwells$PFOAfinal > 0] <- 1

PFASwells$PFHXAfinal[PFASwells$PFHXAfinal <= 0.25] <- 0
PFASwells$PFHXAfinal[PFASwells$PFHXAfinal > 0.25 & PFASwells$PFHXAresults < 4.5] <- 0
PFASwells$PFHXAfinal[PFASwells$PFHXAfinal > 0] <- 1

PFASwells$PFPEAfinal[PFASwells$PFPEAfinal <= 0.25] <- 0
PFASwells$PFPEAfinal[PFASwells$PFPEAfinal > 0.25 & PFASwells$PFPEAresults < 4.5] <- 0
PFASwells$PFPEAfinal[PFASwells$PFPEAfinal > 0] <- 1

PFASwells$PFHPAfinal[PFASwells$PFHPAfinal <= 0.25] <- 0
PFASwells$PFHPAfinal[PFASwells$PFHPAfinal > 0.25 & PFASwells$PFHPAresults < 2] <- 0
PFASwells$PFHPAfinal[PFASwells$PFHPAfinal > 0] <- 1

PFASwells$PFOSfinal[PFASwells$PFOSfinal <= 0.25] <- 0
PFASwells$PFOSfinal[PFASwells$PFOSfinal > 0.25 & PFASwells$PFOSresults < 4] <- 0
PFASwells$PFOSfinal[PFASwells$PFOSfinal > 0] <- 1


# Reorder & Rename columns ------------------------------------------------

PFASwells <- PFASwells %>% 
  select(StationID, grep('results', names(PFASwells), value = T), everything())

colnames(PFASwells)[2] <- "PFOAreg"
colnames(PFASwells)[3] <- "PFHXAreg"
colnames(PFASwells)[4] <- "PFPEAreg"
colnames(PFASwells)[5] <- "PFHPAreg"
colnames(PFASwells)[6] <- "PFOSreg"


# Save --------------------------------------------------------------------

saveRDS(PFASwells, '../../modeling_data/PFASwells.rds')

