# Industries
# Library -----------------------------------------------------------------

library(tidyverse)

# Load --------------------------------------------------------------------

new_finalind <- readRDS('modeling_data/new_finalind.rds')


# Clean and coerce types --------------------------------------------------

industries1 <- new_finalind[!duplicated(new_finalind),]
industries1$NAICS <- as.numeric(levels(industries1$NAICS))[industries1$NAICS]
industries1$NAICSabbv <- substr(as.character(industries1$NAICS), 0, 3)
industries1$NAICSabbv <- as.numeric(industries1$NAICSabbv)


# Define impact using total number of industries --------------------------

# industries_agg <- aggregate(.~StationID, industries1, FUN = length)
# final_industries <- industries_agg[,c(1:2)]
# colnames(final_industries)[2] <- 'ImpactTotalCount'


# Split industries into groups --------------------------------------------
# NAICSabbv to NAICSgroup key:
# 'T': 313 or 314
# 'Pl': 326
# 'AW': 562 or 488
# 'Pr': 322 or 488
# 'S': 334
# 'OI': 324, 325, 332, 333, 424, 442, 561

convert <- function(number){
  if (number == 313 | number == 314) {return('T')}
  if (number == 326) {return('Pl')}
  if (number == 562 | number == 488) {return('AW')}
  if (number == 322 | number == 323) {return('Pr')}
  if (number == 334) {return('S')}
  if (number == 324 | number == 325 | number == 332 | 
      number == 333 | number == 424 | number == 442 |
      number == 561) {return('OI')}
}

industries1$NAICSgroups <- sapply(industries1$NAICSabbv, convert)


# Define impact -----------------------------------------------------------

industries1$impactD <- 1/exp(industries1$distances/1000)
industries1$impactDxS <- industries1$impactD*industries1$SALESVOL

# Run only 1 of the following
# 1. Define by number of types of industries by category
# industries_agg <- aggregate(.~StationID+NAICSgroups, industries1, FUN = length)
# 2. Define by distance (default)
industries_agg <- aggregate(.~StationID+NAICSgroups, industries1, FUN = sum)


# Recode impact -----------------------------------------------------------

codes <- c('OI', 'Pl', 'AW', 'Pr', 'S', 'T')
ind <- paste0('Impact', codes)
ind2 <- paste0('Impact', codes, '2')

for (i in 1:length(codes)) {
  industries_agg[[ind[i]]] <- ifelse(industries_agg$NAICSgroups == codes[i], 1, 0)
}

# 1/e^d * sales volume by industry
for (i in 1:length(ind)) {
  industries_agg[[ind2[i]]] <- industries_agg[[ind[i]]]*industries_agg$impactDxS
  industries_agg[[ind[i]]] <- NULL
}


# Aggregate
final_industries <- aggregate(.~StationID, 
                              industries_agg[, c('StationID', ind2)], 
                              FUN = sum)

# Rename columns
colnames(final_industries)[-1] <- ind2

# Save --------------------------------------------------------------------

saveRDS(final_industries, 'modeling_data/final_industries.rds')
