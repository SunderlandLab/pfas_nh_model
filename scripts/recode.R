# Recode chemical data
# Library -----------------------------------------------------------------

library(tidyverse)

# Load --------------------------------------------------------------------

PFASwells1 <- readRDS('../../modeling_data/PFASwells1.rds')

# Recode: 0 = below detection limit, 1 = above detection limit ------------
# Interpretation of original columns: 
# - results = concentration, qualifier = "<" if non detect, numeric = detection limit
# - Any well with concentration below 98th DL for compound is considered a 
# nondetect with value recoded to median/sqrt(2) for purposes of summary statistics


recode <- function(data, PFAS){
  numeric_name <- paste0(PFAS, 'numeric') 
  qualifier_name <- paste0(PFAS, 'qualifier')
  results_name <- paste0(PFAS, 'results')
  #med <- median(data[[numeric_name]][data[[qualifier_name]] == '<'], na.rm = T)
  p98 <- quantile(data[[numeric_name]][data[[qualifier_name]] == '<'], prob = 0.98, na.rm = T)
  # Note: opted to round cutoff to replicate original data, is possible to include full float
  # 03/19/19 update: corrected previous mistake of recoding values below med/sqrt(2); should recode values below med
  recode_value <- round(p98/sqrt(2), digits = 3)
  data[[qualifier_name]][data[[qualifier_name]] == '' & data[[results_name]] < p98] <- '<'
  data[[results_name]][data[[results_name]] < p98] <- recode_value
  return(data)
}

PFASwells1 <- PFASwells1 %>%
  recode('PFPEA') %>%
  recode('PFHXA') %>%
  recode('PFHPA') %>%
  recode('PFOA') %>%
  recode('PFOS')

# Create binary classifier for detect/non-detect --------------------------
# Initialize columns
PFASwells1 <- PFASwells1 %>%
  mutate(PFOAfinal = NA,
         PFOSfinal = NA,
         PFPEAfinal = NA,
         PFHXAfinal = NA,
         PFHPAfinal = NA)
code_detect <- function(data, PFAS){
  numeric_name <- paste0(PFAS, 'numeric') 
  qualifier_name <- paste0(PFAS, 'qualifier')
  results_name <- paste0(PFAS, 'results')
  final_name <- paste0(PFAS, 'final')
  data[[final_name]][data[[results_name]] & data[[qualifier_name]] == ""] <- 1
  data[[final_name]][data[[qualifier_name]] == '<'] <- 0
  return(data)
}

PFASwells1 <- PFASwells1 %>%
  code_detect('PFPEA') %>%
  code_detect('PFHPA') %>%
  code_detect('PFOS') %>%
  code_detect("PFOA") %>%
  code_detect("PFHXA")

# drop unnecessary columns, qualifiers and numeric
PFASwells1 <- PFASwells1 %>%
  dplyr::select(-contains("qualifier")) %>%
  dplyr::select(-contains("numeric"))

# Aggregate ---------------------------------------------------------------
# Aggregate by well such that each well only has one value
# if a well is measured multiple times and detected for once then consider this a detect
PFASwells <- PFASwells1 %>% 
  group_by(StationID) %>%
  mutate_at(vars(contains("results")), mean, na.rm = T) %>%
  mutate_at(vars(contains("final")), function(x){if_else(x > 0, 1, 0)}) %>%
  distinct() %>%
  ungroup()
colnames(PFASwells) <- gsub("results$", "reg", colnames(PFASwells))
# Save --------------------------------------------------------------------
saveRDS(PFASwells, '../../modeling_data/PFASwells.rds')
