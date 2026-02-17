#####################################
###### Cleaning consumer data #######
####### Aggregate by Eatwell ######## 
#####################################

# Aim: To load the consumer datasets, clean and aggregate the datasets by eatwell categories for each household over the study period. 

# Libraries
library(data.table) # For general data wrangling (I ain't no tidyverse fan)
library(readr)

## Load in new data
#new <- read.csv("N:/_incoming-Fiofood/2025-04-01/EXT_FACT_LIDA_FIO.csv", nrows = 1000) # Load 1000 rows of new data
#write.csv(new, "./test_new.csv") # for testing code later
#test <- fread("./test_new.csv") # Test data
#old <- fread("N:/_incoming-Fiofood/2023-05-22/Output CSV for YTH 2022/LIDA_TXN_YTH_2022-01-01.csv") # Load old files
#hash <- fread("N:/_incoming-Fiofood/2025-01-30/HASH_OA_RGN.csv") # Load area level data


### Load in regional data and process into a single aggregated file ###

## Get everything required for processing steps ##

# Lookup table for SKUs/item code to sustainability and health metrics of products
lkup <- read.csv("../Product Data - from EW/25-04-08 Final product dataset for paper 2.csv") # Load file
lkup <- lkup[, c("sku", "Weight_prod")] # Subset only variables required
names(lkup)[names(lkup) == "sku"] <- "ITEM_CD" # Rename

# Lookup table for households to subset
all_ids_mlm <- fread("./Processed data/all_ids_mlm.csv") # Load IDs to subset data later

# Lookup table for EatWell categories
eatwell <- read.csv("./Lookup files/data with categories Aug 2021.csv") # Load file
eatwell <- eatwell[, c("SKU", "Eatwell.segment")] # Subset only variables required
names(eatwell)[names(eatwell) == "SKU"] <- "ITEM_CD" # Rename


## Process 2022 food purchasing records ##

# Note: 2022 data for all regions is 164GB in total, so will need to load this in stages and process the information needed as we go to avoid running into memory issues. I suggest that you do this for each region separately to save memory

# Create function to clean dataset and extract information we need
process_chunk <- function(chunk, pos) {
  # Get data into format required
  setDT(chunk) # Convert to data.table format
  #cat("\nChunk position:", pos, "\n") # Print row of dataset loaded in so far (for testing purposes)
  #print(head(chunk[, .(ENTERPRISE_CUSTOMER_NUM)])) # Print first few IDs to see if printing different rows each time (for testing purposes)
  
  # Subset only household IDs from MLM model that need
  chunk <- merge(chunk, all_ids_mlm, by = "ENTERPRISE_CUSTOMER_NUM", all.x = TRUE) # Join on health and sustainability lookup table
  chunk <- chunk[chunk$IMD_Decile >= 1,] # Subset only IMD deciles 1 and 10
  
  # Join on key information
  chunk <- merge(chunk, lkup, by = "ITEM_CD", all.x = TRUE) # Join on health and sustainability lookup table
  chunk$ITEM_QUANTITY[chunk$ITEM_QUANTITY < 0] <- 1 # Impute missing data to median value
  chunk <- merge(chunk, eatwell, by = "ITEM_CD", all.x = TRUE) # Join on eatwell guide categories
  
  # Get product weight
  chunk$weight <- chunk$ADJUSTED_STD_ITEM_TOTAL_WGT_OR_VOL_QTY # Use item weight if Sainsbury's has stored it (this is converted to grams/ML)
  chunk$weight[chunk$ADJUSTED_STD_ITEM_TOTAL_WGT_OR_VOL_QTY == 0] <- chunk$Weight_prod[chunk$ADJUSTED_STD_ITEM_TOTAL_WGT_OR_VOL_QTY == 0] # If missing then use Mariana's estimated weight (average for item)
  chunk$weight[is.na(chunk$weight)] <- median(chunk$weight, na.rm = TRUE) # If still missing, then impute median weight
  chunk$total_weight <- chunk$ITEM_QUANTITY * chunk$weight # Total weight of all purchases
  
  # Get total purchases and weight purchased per household by food category
  chunk_eatwell <- chunk[, list(total_items = sum(ITEM_QUANTITY, na.rm = TRUE), total_purchases = .N, total_weight_purchased = sum(total_weight, na.rm = TRUE)), by = c("ENTERPRISE_CUSTOMER_NUM", "Eatwell.segment", "IMD_Decile")] # Aggregate for eatwell categories
  
  ## Join aggregated results with the previous files ##
  
  # Eatwell
  if (nrow(final_eatwell$running_total)==0) { # If the first occassion (object is empty)
    final_eatwell$running_total <- chunk_eatwell # Save as object
  } else { # If not
    final_eatwell$running_total <- rbindlist(list(final_eatwell$running_total, chunk_eatwell))[ # Join on the new data to ongoing file
      , .( # Aggregate these measures
        total_items = sum(total_items, na.rm = TRUE),
        total_purchases = sum(total_purchases, na.rm = TRUE), 
        total_weight_purchased = sum(total_weight_purchased, na.rm = TRUE) 
      ),
      by = c("ENTERPRISE_CUSTOMER_NUM", "Eatwell.segment", "IMD_Decile") # Aggregate by these characteristics
    ]
  }
  
  return(invisible()) # Return nothing as results accumulate in new environment
  
  # Tidy
  rm(chunk_eatwell, chunk)
  gc()
  
}

# Create blank place to store the aggregated dataset
final_eatwell <- new.env()
final_eatwell$running_total <- data.table()

start <- proc.time()
# Load file in chunks and process the files to get information needed
read_csv_chunked(
  file = "N:/_incoming-Fiofood/2025-04-01/EXT_FACT_LIDA_FIO.csv", # File to chunk # "./test_new.csv", # For testing code (smaller file)
  callback = DataFrameCallback$new(process_chunk), # Function to call
  chunk_size = 1000000) # Split file into 1M parts and process one at a time
end <- proc.time()
end - start

# Save
final_eatwell <- final_eatwell$running_total # Get data
fwrite(final_eatwell, "./Processed data/agg_eatwell_all_2022.csv") # Save

# Tidy
#rm(process_chunk, final_data, lkup, end, start, final_aggregates)
gc()