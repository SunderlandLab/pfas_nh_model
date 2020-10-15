# Finalize data for 5 compounds
# Library -----------------------------------------------------------------

library(tidyverse)

# Load --------------------------------------------------------------------

merged_variables <- readRDS('../../modeling_data/merged_variables1207.rds')
unique_ivs <- readRDS('../../modeling_data/unique_ivs1207.rds')


# Generate separate dataframe for each compound ---------------------------

compounds_data <- list("PFOA", "PFHXA","PFPEA", "PFHPA", "PFOS") %>% set_names()

for (i in 1:length(compounds_data)) {
  # Extract 'StationID' and compound 'reg' and 'final' columns
  df <- merged_variables[, c('StationID', grep(compounds_data[i], names(merged_variables), value = T))]
  # Aggregate + Merge
  df_agg <- aggregate(.~StationID, df, FUN = mean)
  df_final <- merge(df_agg, unique_ivs, by = "StationID", all.x = TRUE)
  # Rename columns
  colnames(df_final)[2] <- "reg"
  colnames(df_final)[3] <- "final"
  df_final$final <- as.factor(df_final$final)
  compounds_data[[i]] <- df_final
}

#check for quasi-completeness
lapply(compounds_data, function(x){
  table(x[['final']], x[['bedrock_M']])
})
lapply(compounds_data, function(x){
  table(x[['final']], x[['hydgrpdcdA']])
})
#looks good

# Save --------------------------------------------------------------------

saveRDS(compounds_data, '../../modeling_data/compounds_data1207.rds')
