# Finalize data for 5 compounds
# Library -----------------------------------------------------------------

library(tidyverse)
library(kableExtra)
library(flextable)
library(docxtools)

# Load --------------------------------------------------------------------

merged_variables <- readRDS('../../modeling_data/merged_variables.rds')
unique_ivs <- readRDS('../../modeling_data/unique_ivs.rds')


# Generate separate dataframe for each compound ---------------------------

compounds_data <- list("PFOA", "PFHXA","PFPEA", "PFHPA", "PFOS", "PFAS") %>% set_names()

for (i in 1:length(compounds_data)) {
  # Extract 'StationID' and compound 'reg' and 'final' columns
  df <- merged_variables[, c('StationID', grep(compounds_data[i], names(merged_variables), value = T))]
  # Aggregate + Merge
  df_agg <- aggregate(.~StationID, df, FUN = mean)
  df_final <- merge(df_agg, unique_ivs, by = "StationID", all.x = TRUE)
  # Rename columns
  colnames(df_final)[2] <- "reg"
  colnames(df_final)[3] <- "final"
  df_final$final <- if_else(df_final$final>0, 1, 0) %>%
    factor(levels = c(0, 1))
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

saveRDS(compounds_data, '../../modeling_data/compounds_data.rds')


# Table 1
compounds_data <- readRDS('../../modeling_data/compounds_data.rds')

table1_df<-bind_rows(!!!compounds_data, .id = "compound") %>%
  mutate(DL = case_when(compound == "PFOA" ~ 8, # uniform detection limit (ng/L)
                        compound == "PFHXA" ~ 8,
                        compound == "PFHPA" ~ 5,
                        compound == "PFPEA" ~ 5,
                        compound == "PFOS" ~ 5,
                        compound == "PFAS" ~ 31,
                        TRUE ~ NA_real_),
         compound = case_when(compound == "PFHXA" ~ "PFHxA",
                              compound == "PFHPA" ~ "PFHpA",
                              compound == "PFPEA" ~ "PFPeA",
                              compound == "PFAS" ~ "sumPFAS",
                              TRUE ~ compound)) 

table1_master<-table1_df %>%
  dplyr::select(-c(final, bedrock_M, hydgrpdcdA, StationID)) %>%
  pivot_longer(-c("compound", "DL")) %>%
  group_by(compound, name) %>%
  summarise(N = n(),
            DL = min(DL),
            pct_detect = sum(value>DL)/N *100,
            q1= quantile(value, 0.25),
            median = quantile(value, 0.5),
            q3 = quantile(value, 0.75),
            p98 = quantile(value, 0.98),
            max = max(value)) %>%
  ungroup() %>%
  mutate(compound = factor(compound, levels = c("PFPeA", "PFHxA", "PFHpA", "PFOA", "PFOS", "sumPFAS"))) %>%
  arrange(compound, name)


table1_master %>% 
  format_engr(., sigdig = 3) %>%
  filter(name == "reg") %>%
  dplyr::select(-name) %>%
  flextable() %>%
  add_header_lines(values = "PFAS concentrations (ng/L)") %>%
  theme_booktabs() %>%
  save_as_docx(path = "../../output/Table1_pfas_conc.docx")


level_key <- c("ImpactPlastics" = "Industry: Plastics and rubber", 
               "recharge" = "Hydro: Groundwater recharge",
               "precip" = "Hydro: Monthly precipitation",
               "ImpactTextile" = "Industry: Textiles manufacturing",
               "silttotal_r" = "Soil: Percent total silt",
               "cec7_r" = "Soil: Cation exchange capacity",
               "claytotal_r" = "Soil: Percent total clay",
               "slopegradwta" = "Hydro: Slope gradient",
               #"ImpactPr" = "Industry: Printing industry",
               "soc0_999" = "Soil: Organic carbon",
               "dbthirdbar_r" = "Soil: Bulk density",
               "awc_r" = "Soil: Available water capacity",
               "ImpactOI" = "Industry: Suspected sources",
               "ImpactWWTP" = "Industry: Wastewater treatment plant",
               "ImpactAirports" = "Industry: Airport",
               #"ImpactMilitary" = "Industry: Military AFFF",
               #"ImpactM" = "Industry: Metal plating",
               "hzdep" = "Soil: Thickness of soil horizon",
               "bedrock_M" = "Geo: Bedrock type",
               "hydgrpdcdA" = "Hydro: Low runoff potential",
               #"ImpactS" = "Industry: Semiconductor manufacturing",
               "wtdepannmin" = "Hydro: Depth to water table",
               "brockdepmin" = "Geo: Depth to bedrock")

options(pillar.sigfig = 3)
table1_df %>%
  dplyr::select(-c(StationID, final, bedrock_M, hydgrpdcdA)) %>%
  pivot_longer(-c("compound", "DL")) %>%
  group_by(compound, name) %>%
  summarise(min = min(value),
            q1= quantile(value, 0.25),
            median = quantile(value, 0.5),
            q3 = quantile(value, 0.75),
            p98 = quantile(value, 0.98),
            max = max(value)) %>%
  ungroup() %>%
  mutate(compound = factor(compound, levels = c("PFPeA", "PFHxA", "PFHpA", "PFOA", "PFOS", "PFAS"))) %>%
  arrange(compound, name) %>%
  mutate(name = dplyr::recode(name, !!!level_key)) %>%
  format_engr(., sigdig = 3) %>%
  filter(compound == "PFOA" & name != "reg") %>%
  dplyr::select(name, everything(.)) %>%
  dplyr::select(-c(compound)) %>%
  separate(name, c("group", "variable"), sep = ":")%>%
  arrange(group, variable) %>%
  flextable() %>%
  merge_v(j = c("group", "variable")) %>%
  add_header_lines(values = "Continuous independent variables") %>%
  theme_booktabs() %>%
  save_as_docx(path = "../../output/Table1_continuous_var_combined.docx")


table1_cat<-table1_df %>%
  dplyr::select(c(compound, bedrock_M, hydgrpdcdA)) %>%
  pivot_longer(-c("compound")) %>%
  group_by(compound, name) %>%
  summarise(N = n(),
            N1 = sum(value == 1),
            pct_1 = N1/N * 100,
            N0 = sum(value == 0),
            pct_0 = N0/N * 100) %>%
  ungroup() %>%
  mutate(compound = factor(compound, levels = c("PFPeA", "PFHxA", "PFHpA", "PFOA", "PFOS"))) %>%
  arrange(compound, name)

table1_cat %>%
  mutate(name = dplyr::recode(name, !!!level_key)) %>%
  #format_engr(., sigdig = 3) %>%
  dplyr::select(name, everything(.)) %>%
  separate(name, c("group", "variable"), sep = ":")%>%
  arrange(group, variable, compound) %>%
  flextable() %>%
  colformat_num(j = c(3), digits = 1) %>%
  merge_v(j = c("group", "variable", "compound")) %>%
  add_header_lines(values = "Categorical independent variables") %>%
  theme_booktabs() %>%
  save_as_docx(path = "../../output/Table1_categorical_var_combined.docx")

