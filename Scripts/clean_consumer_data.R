###############################
#### Cleaning consumer data ###
####### Latest extract ######## 
###############################

# Aim: To load the consumer datasets, clean into analysis ready format and join on emissions estimates. 

# Libraries
library(data.table) # For general data wrangling (I ain't no tidyverse fan)
library(readr)

## Load in new data (for testing purposes)
#chunk <- read.csv("N:/_incoming-Fiofood/2025-04-01/EXT_FACT_LIDA_FIO.csv", nrows = 1000) # Load 1000 rows of new data
#write.csv(chunk, "./test_new.csv") # for testing code later
#chunk <- fread("./test_new.csv") # Test data
#old <- fread("N:/_incoming-Fiofood/2023-05-22/Output CSV for YTH 2022/LIDA_TXN_YTH_2022-01-01.csv") # Load old files
#hash <- fread("N:/_incoming-Fiofood/2025-01-30/HASH_OA_RGN.csv") # Load area level data


### Load in regional data and process into a single aggregated file ###

## Get everything required for processing steps ##

# Lookup table for SKUs/item code to sustainability and health metrics of products
#lkup <- read.csv("../Product Data - from EW/OLD/25-04-08 Final product dataset for paper 2.csv") # Load file (original)
#lkup <- lkup[, c("sku", "Weight_prod", "HFSS_status", "Energyper100gkcal", "GHGE_perkg", "Landuse_perkg", "Wateruse_perkg")] # Subset only variables required (original)
lkup <- read.csv("../Product Data - from EW/25-12-04 Final Product Dataset for Paper 2.csv") # Load file (original)
lkup <- lkup[, c("sku", "Weight_prod", "HFSS_narrow", "HFSS_broad", "Energyper100gkcal", "GHGE_perkg", "Landuse_perkg", "Wateruse_perkg")] # Subset only variables required (original)
names(lkup)[names(lkup) == "sku"] <- "ITEM_CD" # Rename


## Process 2022 food purchasing records ##

# Note: 2022 data for all regions is 164GB in total, so will need to load this in stages and process the information needed as we go to avoid running into memory issues. I suggest that you do this for each region separately to save memory

# Create function to clean dataset and extract information we need
process_chunk <- function(chunk, pos) {
  # Get data into format required
  setDT(chunk) # Convert to data.table format
  #cat("\nChunk position:", pos, "\n") # Print row of dataset loaded in so far (for testing purposes)
  #print(head(chunk[, .(ENTERPRISE_CUSTOMER_NUM)])) # Print first few IDs to see if printing different rows each time (for testing purposes)
  
  # Join on key information
  chunk <- merge(chunk, lkup, by = "ITEM_CD", all.x = TRUE) # Join on health and sustainability lookup table
  chunk$ITEM_QUANTITY[chunk$ITEM_QUANTITY < 0] <- 1 # Impute missing data to median value
  
  # Get product weight
  chunk$weight <- chunk$ADJUSTED_STD_ITEM_TOTAL_WGT_OR_VOL_QTY # Use item weight if Sainsbury's has stored it (this is converted to grams/ML)
  chunk$weight[chunk$ADJUSTED_STD_ITEM_TOTAL_WGT_OR_VOL_QTY == 0] <- chunk$Weight_prod[chunk$ADJUSTED_STD_ITEM_TOTAL_WGT_OR_VOL_QTY == 0] # If missing then use Mariana's estimated weight (average for item)
  chunk$weight[is.na(chunk$weight)] <- median(chunk$weight, na.rm = TRUE) # If still missing, then impute median weight
  
  # Estimate total values for each sustainability metric
  chunk$ghg_emissions <- chunk$ITEM_QUANTITY * (chunk$weight * (chunk$GHGE_perkg/1000)) # Greenhouse gas emissions [Number of items x (emissions/1000 x weight in g)]
  chunk$water_use <- chunk$ITEM_QUANTITY * (chunk$weight * (chunk$Landuse_perkg/1000)) # Land use
  chunk$land_use <- chunk$ITEM_QUANTITY * (chunk$weight * (chunk$Wateruse_perkg/1000)) # Water use
  
  # Get indicators for health metrics
  chunk$total_weight <- chunk$ITEM_QUANTITY * chunk$weight # Total weight of all purchases
  
  chunk$total_weight_hfss_narrow <- 0 # Create blank variable to capture weight purchased of HFSS items (narrow definition)
  chunk$total_weight_hfss_narrow[!is.na(chunk$HFSS_narrow) & chunk$HFSS_narrow == 1] <- chunk$total_weight[!is.na(chunk$HFSS_narrow) & chunk$HFSS_narrow == 1] # If HFSS, then store weight (narrow definition)
  chunk$total_weight_nonhfss_narrow <- 0 # Repeat variable but for those marked as non-HFSS items (narrow definition)
  chunk$total_weight_nonhfss_narrow[!is.na(chunk$HFSS_narrow) & chunk$HFSS_narrow == 0] <- chunk$total_weight[!is.na(chunk$HFSS_narrow) & chunk$HFSS_narrow == 0] # If not HFSS, then store weight (narrow definition)
  
  chunk$total_weight_hfss_broad <- 0 # Create blank variable to capture weight purchased of HFSS items (broad definition)
  chunk$total_weight_hfss_broad[!is.na(chunk$HFSS_broad) & chunk$HFSS_broad == 1] <- chunk$total_weight[!is.na(chunk$HFSS_broad) & chunk$HFSS_broad == 1] # If HFSS, then store weight (broad definition)
  chunk$total_weight_nonhfss_broad <- 0 # Repeat variable but for those marked as non-HFSS items (broad definition)
  chunk$total_weight_nonhfss_broad[!is.na(chunk$HFSS_broad) & chunk$HFSS_broad == 0] <- chunk$total_weight[!is.na(chunk$HFSS_broad) & chunk$HFSS_broad == 0] # If not HFSS, then store weight (broad definition)
  
  chunk$total_energy <- 0 # Create blank variable for kcals
  chunk$Energyper100gkcal[is.na(chunk$Energyper100gkcal)] <- 0 # If missing set as 0
  chunk$total_energy <- ((chunk$Energyper100gkcal / 100) * chunk$weight) * chunk$ITEM_QUANTITY # Take energy per 100g kcals, divide it by 100 to get per g, then multiply by weight and quantity to get get total kcals
  
  # Get total emissions, purchases and HFSS weight purchased per person
  chunk_aggregates <- chunk[, list(ghg_emissions = sum(ghg_emissions, na.rm = TRUE), water_use = sum(water_use, na.rm = TRUE), land_use = sum(land_use, na.rm = TRUE), total_energy = sum(total_energy, na.rm = TRUE), total_items = sum(ITEM_QUANTITY, na.rm = TRUE), total_purchases = .N, total_weight_purchased = sum(total_weight, na.rm = TRUE), total_weight_purchased_hfss_narrow = sum(total_weight_hfss_narrow, na.rm = TRUE), total_weight_purchased_nonhfss_narrow = sum(total_weight_nonhfss_narrow, na.rm = TRUE), total_weight_purchased_hfss_broad = sum(total_weight_hfss_broad, na.rm = TRUE), total_weight_purchased_nonhfss_broad = sum(total_weight_nonhfss_broad, na.rm = TRUE)), by = c("ENTERPRISE_CUSTOMER_NUM", "AGE_BAND_NAME", "HOUSEHOLD_SIZE", "CUSTOMER_REGION")] # Aggregate daily emissions and total number of purchases per person. Note: If we aggregate using age band and household size, it preserves this information for us
  
  # Join aggregated results with the previous files
  if (nrow(final_aggregates$running_total)==0) { # If the first occasion (object is empty)
      final_aggregates$running_total <- chunk_aggregates # Save as object
  } else { # If not
    final_aggregates$running_total <- rbindlist(list(final_aggregates$running_total, chunk_aggregates))[ # Join on the new data to ongoing file
      , .( # Aggregate these measures
        ghg_emissions = sum(ghg_emissions, na.rm = TRUE),
        water_use = sum(water_use, na.rm = TRUE),
        land_use = sum(land_use, na.rm = TRUE),
        total_energy = sum(total_energy, na.rm = TRUE),
        total_items = sum(total_items, na.rm = TRUE),
        total_purchases = sum(total_purchases, na.rm = TRUE), 
        total_weight_purchased = sum(total_weight_purchased, na.rm = TRUE), 
        total_weight_purchased_hfss_narrow = sum(total_weight_purchased_hfss_narrow, na.rm = TRUE), 
        total_weight_purchased_nonhfss_narrow = sum(total_weight_purchased_nonhfss_narrow, na.rm = TRUE),
        total_weight_purchased_hfss_broad = sum(total_weight_purchased_hfss_broad, na.rm = TRUE), 
        total_weight_purchased_nonhfss_broad = sum(total_weight_purchased_nonhfss_broad, na.rm = TRUE)
      ),
      by = c("ENTERPRISE_CUSTOMER_NUM", "AGE_BAND_NAME", "HOUSEHOLD_SIZE", "CUSTOMER_REGION") # Aggregate by these characteristics
    ]
  }
  
  return(invisible()) # Return nothing as results accumulate in new environment
  
  # Tidy
  rm(chunk_aggregates, chunk)
  gc()
  
}

# Create blank place to store the aggregated dataset
final_aggregates <- new.env()
final_aggregates$running_total <- data.table()

start <- proc.time()
# Load file in chunks and process the files to get information needed
read_csv_chunked(
  file = "N:/_incoming-Fiofood/2025-04-01/EXT_FACT_LIDA_FIO.csv", # File to chunk # "./test_new.csv", # For testing code (smaller file)
  callback = DataFrameCallback$new(process_chunk), # Function to call
  chunk_size = 1000000) # Split file into 1M parts and process one at a time
end <- proc.time()
end - start

# Save
final_data <- final_aggregates$running_total # Get data
fwrite(final_data, "./Processed data/all_2022.csv") # Save

# Tidy
rm(process_chunk, final_data, lkup, end, start, final_aggregates)
gc()

# Check output
final_data <- fread("./Processed data/all_2022.csv")
final_data$HOUSEHOLD_SIZE[final_data$HOUSEHOLD_SIZE > 4] <- 4
final_data <- final_data[final_data$total_items > 100]
final_data[, list(ghg_emissions = mean(ghg_emissions, na.rm = TRUE), land_use = mean(land_use, na.rm = TRUE), water_use = mean(water_use, na.rm = TRUE), total_purchases_rate = sum(total_purchases, na.rm = TRUE)/.N), by = c("HOUSEHOLD_SIZE")] # Repeat so only store one record per person




