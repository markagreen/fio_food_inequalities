###############################
### Tidying geospatial data ###
###############################

# Aim: To load in all the geospatial datasets, clean and join into analysis ready files that can be matched to the consumer data. 

# Libraries
library(data.table)

## Load and tidy variables ##

# Load files
imd <- fread("N:/_incoming-Fiofood/2024-07-29/for_laser_upload/imd2019.csv") # Index of multiple deprivation (IMD)
imd$V1 <- NULL # Drop column not required

oac <- fread("N:/_incoming-Fiofood/2024-07-29/for_laser_upload/oac_for_lsoa_eng_long.csv") # Output area classification (OAC) at LSOA level
oac$V1 <- NULL # Drop column not required

urbrur <- fread("N:/_incoming-Fiofood/2024-07-29/for_laser_upload/urban_rural_indicators.csv") # Urban and rural classification
urbrur$V1 <- NULL # Drop column not required

sainsburys <- fread("N:/_incoming-Fiofood/2024-07-29/for_laser_upload/sainsburys_presence.csv") # Accessibility to Sainsbury's stores
sainsburys$V1 <- NULL # Drop column not required
sainsburys$V1 <- NULL # There are two for whatever reason

hhold_pop_all <- fread("N:/_incoming-Fiofood/2024-10-04/census_age_hhold_allusualres.csv") # Population counts for LSOAs by age_group - all resident population
hhold_pop_all$V1 <- NULL # Drop column not required

hhold_pop_hrf <- fread("N:/_incoming-Fiofood/2024-10-04/census_age_hhold_hrp.csv") # Population counts for LSOAs by age_group - household reference person
hhold_pop_hrf$V1 <- NULL # Drop column not required

# Select OAC subgroup that is most common within a LSOA
oac_lsoa <- oac[, .SD[which.max(proportion)], by = "LSOA11CD"] # May not use in analysis, but create just in case
oac_lsoa$proportion <- NULL

# Recode age bands to match Sainsbury's descriptors
hhold_pop_all$age_group[hhold_pop_all$age_group == "Aged 18 to 24 years"] <- "18 TO 24"
hhold_pop_all$age_group[hhold_pop_all$age_group == "Aged 25 to 34 years"] <- "25 TO 34"
hhold_pop_all$age_group[hhold_pop_all$age_group == "Aged 35 to 44 years"] <- "35 TO 44"
hhold_pop_all$age_group[hhold_pop_all$age_group == "Aged 45 to 54 years"] <- "45 TO 54"
hhold_pop_all$age_group[hhold_pop_all$age_group == "Aged 55 to 64 years"] <- "55 TO 64"
hhold_pop_all$age_group[hhold_pop_all$age_group == "Aged 65 to 74 years"] <- "65 TO 74"
hhold_pop_all$age_group[hhold_pop_all$age_group == "Aged 75 years and over"] <- "75 AND OVER"

hhold_pop_hrf$age_group[hhold_pop_hrf$age_group == "Aged 18 to 24 years"] <- "18 TO 24"
hhold_pop_hrf$age_group[hhold_pop_hrf$age_group == "Aged 25 to 34 years"] <- "25 TO 34"
hhold_pop_hrf$age_group[hhold_pop_hrf$age_group == "Aged 35 to 44 years"] <- "35 TO 44"
hhold_pop_hrf$age_group[hhold_pop_hrf$age_group == "Aged 45 to 54 years"] <- "45 TO 54"
hhold_pop_hrf$age_group[hhold_pop_hrf$age_group == "Aged 55 to 64 years"] <- "55 TO 64"
hhold_pop_hrf$age_group[hhold_pop_hrf$age_group == "Aged 65 to 74 years"] <- "65 TO 74"
hhold_pop_hrf$age_group[hhold_pop_hrf$age_group == "Aged 75 years and over"] <- "75 AND OVER"

# Drop age bands not required
hhold_pop_all <- hhold_pop_all[hhold_pop_all$age_group != "Aged 17 years and under"]
hhold_pop_hrf <- hhold_pop_hrf[hhold_pop_hrf$age_group != "Aged 17 years and under"]

# Rename variables to match
setnames(hhold_pop_all, old = "age_group", new = "AGE_BAND") 
setnames(hhold_pop_hrf, old = "age_group", new = "AGE_BAND")
setnames(oac_lsoa, old = "SUBGRP", new = "OAC_SubGroup")
setnames(imd, old = "imd_decile", new = "IMD_Decile")


## Create long version of dataset ###

# Join the files together
core_file <- merge(imd, oac_lsoa, by = "LSOA11CD", all.x = TRUE) # Join IMD onto the OAC which is in long format
core_file <- merge(core_file, urbrur, by = "LSOA11CD", all.x = TRUE) # Then join on urban-rural classification
core_file <- merge(core_file, sainsburys, by = c("LSOA11CD", "LSOA21CD"), all.x = TRUE) # Then join on sainsbury's access data

# Join onto household counts - all usual residents
hold <- merge(hhold_pop_all, core_file, by = "LSOA21CD", all.x = TRUE) # Join together
hold <- hold[!is.na(hold$AGE_BAND) & hold$AGE_BAND != "",] # Drop missing age group as missing data
fwrite(hold, "./Processed data/census_hhold_all.csv") # Save

# Join onto household counts - household reference persons
hold <- merge(hhold_pop_hrf, core_file, by = "LSOA21CD", all.x = TRUE) # Join together
hold <- hold[!is.na(hold$AGE_BAND) & hold$AGE_BAND != "",] # Drop missing age group as missing data
fwrite(hold, "./Processed data/census_hhold_hrf.csv") # Save

# Tidy
rm(list = ls()) # Delete all objects in memory
gc()





