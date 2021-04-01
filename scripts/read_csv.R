# Read in csv's and clean compound data
# Library -----------------------------------------------------------------
library(tidyverse)
library(tidylog)

# Load --------------------------------------------------------------------

PFASwells1 <- read.csv("../../raw_data/PFAS_Ronly.csv", header = TRUE, sep = ",") %>%
  dplyr::select(-c(2,3))
colnames(PFASwells1)[1]<-"StationID"
pfoapfhxa <- read.csv("../../raw_data/PFOAPFHXA.csv")
colnames(pfoapfhxa)[1]<-"StationID"

PFASwells1 <- PFASwells1 %>%
  full_join(pfoapfhxa, by = "StationID")

# Remove wells with very high detection limits ----------------------------
summarize_lod <- function(PFAS){
  # PFAS is a string
  lods <- PFASwells1 %>% 
    filter(!!rlang::sym(paste0(PFAS, "qualifier")) == "<") %>%
    pull(!!rlang::sym(paste0(PFAS, "numeric")))
  hist(lods, main = PFAS, breaks = 10)
  abline(v = median(lods, na.rm = T), col = "red")
  return(quantile(lods, probs = c(0, 0.5, 0.75, 0.98, 1), na.rm = T))
}

summarize_lod("PFPEA")
summarize_lod("PFHXA")
summarize_lod("PFHPA")
summarize_lod("PFOA")
summarize_lod("PFOS")

# drop wells with LOD which were more than five times the median LOD
PFASwells1 <- PFASwells1 %>%
  filter(!(PFPEAqualifier == "<" & PFPEAnumeric > 5*summarize_lod("PFPEA")["50%"]))%>%
  filter(!(PFHXAqualifier == "<" & PFHXAnumeric > 5*summarize_lod("PFHXA")["50%"]))%>%
  filter(!(PFHPAqualifier == "<" & PFHPAnumeric > 5*summarize_lod("PFHPA")["50%"]))%>%
  filter(!(PFOAqualifier == "<" & PFOAnumeric > 5*summarize_lod("PFOA")["50%"]))%>%
  filter(!(PFOSqualifier == "<" & PFOSnumeric > 5*summarize_lod("PFOS")["50%"]))
# removed 254(1.6%) samples
nrow(PFASwells1)/16315-1
# LODs in the updated data
summarize_lod("PFPEA")
summarize_lod("PFHXA")
summarize_lod("PFHPA")
summarize_lod("PFOA")
summarize_lod("PFOS")

# Save --------------------------------------------------------------------

saveRDS(PFASwells1, '../../modeling_data/PFASwells1.rds')

