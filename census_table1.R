#########################################
### Table 1: Census data descriptives ###
#########################################

# Aim: To load in all the geospatial datasets, clean and join into a table to be used for the sample characteristics (Table 1). 

# Libraries
library(data.table)

## Load and tidy variables ##

# Load files
imd <- fread("../Census data/for_laser_upload/imd2019.csv") # Index of multiple deprivation (IMD)
imd$V1 <- NULL # Drop column not required

oac <- fread("../Census data/for_laser_upload/oac_for_lsoa_eng_long.csv") # Output area classification (OAC) at LSOA level
oac$V1 <- NULL # Drop column not required

urbrur <- fread("../Census data/for_laser_upload/urban_rural_indicators.csv") # Urban and rural classification
urbrur$V1 <- NULL # Drop column not required

sainsburys <- fread("../Census data/for_laser_upload/sainsburys_presence.csv") # Accessibility to Sainsbury's stores
sainsburys$V1 <- NULL # Drop column not required
sainsburys$V1 <- NULL # There are two for whatever reason

# hhold_pop_all <- fread("../Census data/census_age_hhold_allusualres.csv") # Population counts for LSOAs by age_group - all resident population
# hhold_pop_all$V1 <- NULL # Drop column not required

hhold_pop_hrf <- fread("../Census data/for_laser_upload/census_age_hhold_hrp.csv") # Population counts for LSOAs by age_group - household reference person
hhold_pop_hrf$V1 <- NULL # Drop column not required

# Select OAC subgroup that is most common within a LSOA
oac_lsoa <- oac[, .SD[which.max(proportion)], by = "LSOA11CD"] # May not use in analysis, but create just in case
oac_lsoa$proportion <- NULL

# Recode age bands to match Sainsbury's descriptors
# hhold_pop_all$age_group[hhold_pop_all$age_group == "Aged 18 to 24 years"] <- "18 TO 24"
# hhold_pop_all$age_group[hhold_pop_all$age_group == "Aged 25 to 34 years"] <- "25 TO 34"
# hhold_pop_all$age_group[hhold_pop_all$age_group == "Aged 35 to 44 years"] <- "35 TO 44"
# hhold_pop_all$age_group[hhold_pop_all$age_group == "Aged 45 to 54 years"] <- "45 TO 54"
# hhold_pop_all$age_group[hhold_pop_all$age_group == "Aged 55 to 64 years"] <- "55 TO 64"
# hhold_pop_all$age_group[hhold_pop_all$age_group == "Aged 65 to 74 years"] <- "65 TO 74"
# hhold_pop_all$age_group[hhold_pop_all$age_group == "Aged 75 years and over"] <- "75 AND OVER"

hhold_pop_hrf$age_group[hhold_pop_hrf$age_group == "Aged 18 to 24 years"] <- "18 TO 24"
hhold_pop_hrf$age_group[hhold_pop_hrf$age_group == "Aged 25 to 34 years"] <- "25 TO 34"
hhold_pop_hrf$age_group[hhold_pop_hrf$age_group == "Aged 35 to 44 years"] <- "35 TO 44"
hhold_pop_hrf$age_group[hhold_pop_hrf$age_group == "Aged 45 to 54 years"] <- "45 TO 54"
hhold_pop_hrf$age_group[hhold_pop_hrf$age_group == "Aged 55 to 64 years"] <- "55 TO 64"
hhold_pop_hrf$age_group[hhold_pop_hrf$age_group == "Aged 65 to 74 years"] <- "65 TO 74"
hhold_pop_hrf$age_group[hhold_pop_hrf$age_group == "Aged 75 years and over"] <- "75 AND OVER"

# Drop age bands not required
#hhold_pop_all <- hhold_pop_all[hhold_pop_all$age_group != "Aged 17 years and under"]
hhold_pop_hrf <- hhold_pop_hrf[hhold_pop_hrf$age_group != "Aged 17 years and under"]

# Rename variables to match
# setnames(hhold_pop_all, old = "age_group", new = "AGE_BAND") 
setnames(hhold_pop_hrf, old = "age_group", new = "AGE_BAND")
setnames(oac_lsoa, old = "SUBGRP", new = "OAC_SubGroup")
setnames(imd, old = "imd_decile", new = "IMD_Decile")


## Create long version of dataset ###

# Join the files together
core_file <- merge(imd, oac_lsoa, by = "LSOA11CD", all.x = TRUE) # Join IMD onto the OAC which is in long format
core_file <- merge(core_file, urbrur, by = "LSOA11CD", all.x = TRUE) # Then join on urban-rural classification
core_file <- merge(core_file, sainsburys, by = c("LSOA11CD", "LSOA21CD"), all.x = TRUE) # Then join on sainsbury's access data

# # Join onto household counts - all usual residents
# hold <- merge(hhold_pop_all, core_file, by = "LSOA21CD", all.x = TRUE) # Join together
# hold <- hold[!is.na(hold$AGE_BAND) & hold$AGE_BAND != "",] # Drop missing age group as missing data
# fwrite(hold, "./Processed data/census_hhold_all.csv") # Save

# Join onto household counts - household reference persons
hold <- merge(hhold_pop_hrf, core_file, by = "LSOA21CD", all.x = TRUE) # Join together
hold <- hold[!is.na(hold$AGE_BAND) & hold$AGE_BAND != "",] # Drop missing age group as missing data

# Subset areas of analysis
final_data <- hold[hold$tab == "CT21_0329 Yorkshire&The Humber"  | hold$tab == "CT21_0329 South East",]
rm(hold)

## Calculate population proportions for each measure ##

# Household size
hhold <- final_data[, list(`1 person in household` = sum(`1 person in household`, na.rm = TRUE), `2 people in household` = sum(`2 people in household`, na.rm = TRUE), `3 people in household` = sum(`3 people in household`, na.rm = TRUE), `4 or more people in household` = sum(`4 or more people in household`, na.rm = TRUE))] # Aggregate to total
hhold$total <- hhold$`1 person in household` + hhold$`2 people in household` + hhold$`3 people in household` + hhold$`4 or more people in household` # Get total
hhold$pc_1 <- (hhold$`1 person in household` / hhold$total) * 100 # Calculate percentages for each category
hhold$pc_2 <- (hhold$`2 people in household` / hhold$total) * 100
hhold$pc_3 <- (hhold$`3 people in household` / hhold$total) * 100
hhold$pc_4 <- (hhold$`4 or more people in household` / hhold$total) * 100
hhold # Print

# Age band
ageband <- final_data[, .(population = sum(`1 person in household` + `2 people in household` + `3 people in household` + `4 or more people in household`)), by = AGE_BAND] # Aggregate to total population
ageband <- ageband[ageband$AGE_BAND != "Aged 17 years and under",] # Remove as not considered in analysis
ageband$percent <- (ageband$population / sum(ageband$population)) * 100 # Percent

# IMD
imd <- final_data[, .(population = sum(`1 person in household` + `2 people in household` + `3 people in household` + `4 or more people in household`)), by = IMD_Decile] # Aggregate to total population
imd$percent <- (imd$population / sum(imd$population)) * 100 # Percent

# IMD
imd <- final_data[, .(population = sum(`1 person in household` + `2 people in household` + `3 people in household` + `4 or more people in household`)), by = IMD_Decile] # Aggregate to total population
imd$percent <- (imd$population / sum(imd$population)) * 100 # Percent

# RUC
ruc <- final_data[, .(population = sum(`1 person in household` + `2 people in household` + `3 people in household` + `4 or more people in household`)), by = RUC11CD] # Aggregate to total population
ruc$percent <- (ruc$population / sum(ruc$population)) * 100 # Percent
ruc # Print

# Region
region <- final_data[, .(population = sum(`1 person in household` + `2 people in household` + `3 people in household` + `4 or more people in household`)), by = tab] # Aggregate to total population
region$percent <- (region$population / sum(region$population)) * 100 # Percent
region # Print

# Distance to supermarket
summary(sainsburys)

# Tidy
rm(list = ls()) # Delete all objects in memory
gc()





