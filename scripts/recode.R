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

# # drop the data that have LODs exceeding the 98th percentile
# drop_lod_g98 <- function(data, PFAS){
#   numeric_name <- paste0(PFAS, 'numeric') 
#   qualifier_name <- paste0(PFAS, 'qualifier')
#   results_name <- paste0(PFAS, 'results')
#   p98 <- quantile(data[[numeric_name]][data[[qualifier_name]] == '<'], prob = 0.98, na.rm = T)
#   data <- data %>%
#     filter(!(!!sym(qualifier_name) == "<" & !!sym(numeric_name) > p98))
#   return(data)
# }
# 
# PFASwells1 <- PFASwells1 %>%
#   drop_lod_g98('PFPEA') %>%
#   drop_lod_g98('PFHXA') %>%
#   drop_lod_g98('PFHPA') %>%
#   drop_lod_g98('PFOA') %>%
#  drop_lod_g98('PFOS')

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
# 
# # describe the samples dropped
# PFASwells0 <- read.csv("../../raw_data/PFAS_Ronly.csv", header = TRUE, sep = ",") %>%
#   dplyr::select(-c(2,3))
# colnames(PFASwells0)[1]<-"StationID"
# pfoapfhxa <- read.csv("../../raw_data/PFOAPFHXA.csv")
# colnames(pfoapfhxa)[1]<-"StationID"
# 
# PFASwells0 <- PFASwells0 %>%
#   full_join(pfoapfhxa, by = "StationID") %>%
#   rownames_to_column("SampleID")
# 
# glimpse(PFASwells0)
# # read PFASwells1 from line 55
# glimpse(PFASwells1)
# 
# dropped_samples <- PFASwells0 %>%
#   filter(!SampleID %in% PFASwells1$SampleID)
# # Remove wells with very high detection limits ----------------------------
# summarize_lod2 <- function(data, PFAS){
#   # PFAS is a string
#   lods <- data %>% 
#     filter(!!rlang::sym(paste0(PFAS, "qualifier")) == "<") %>%
#     pull(!!rlang::sym(paste0(PFAS, "numeric")))
#   hist(lods, main = PFAS, breaks = 10, xlab = "LOD (ng/L)")
#   abline(v = case_when(PFAS %in% c("PFPEA", "PFHPA", "PFOS") ~ 5,
#                        PFAS %in% c("PFOA", "PFHXA") ~ 8,
#                        TRUE ~ NA_real_),
#          col = "red")
# }
# par(mfrow= c(2,3))
# summarize_lod2(dropped_samples, "PFPEA")
# summarize_lod2(dropped_samples, "PFHXA")
# summarize_lod2(dropped_samples, "PFHPA")
# summarize_lod2(dropped_samples, "PFOA")
# summarize_lod2(dropped_samples, "PFOS")
# dropped_samples %>%
#   dplyr::select(SampleID, contains("results")) %>%
#   pivot_longer(-SampleID, names_to = "compounds", values_to = "results") %>%
#   mutate(compounds = gsub("results", "", compounds)) %>%
#   ggplot(aes(results)) +
#   geom_histogram() +
#   facet_wrap(~compounds, scales = "free_x") +
#   xlab("numeric results (ng/L)")
