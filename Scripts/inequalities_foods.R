######################################
### Inequalities in food products ####
######################################

# Aim: To compare differences in food products purchased between the most and least deprived areas.

# Libraries
library(data.table)
library(ggplot2)


## Load data and clean ##


# Load in sample weights
sample_weights <- fread("./Processed data/sample_weights.csv") # Load
sample_weights <- dplyr::distinct(sample_weights, ENTERPRISE_CUSTOMER_NUM, .keep_all = TRUE) # Has some duplicates in it so keep only one


## Calculate aggregated summary statistics for visualisations ##


# LCFS categories overall #

# Load and clean
imd_hfss <- fread("./Processed data/agg_hfss_imd_2022.csv") # Load
imd_hfss <- imd_hfss[imd_hfss$lcfs_cat != "",] # Drop missing category
lcfs <- imd_hfss[, list(total_weight_purchased = sum(total_weight_purchased, na.rm = TRUE), total_purchases = sum(total_purchases, na.rm = TRUE)), by = c("ENTERPRISE_CUSTOMER_NUM", "lcfs_cat", "IMD_Decile")] # Aggregate to LCFS categories (currently split by HFSS status)
rm(imd_hfss)

# Join on sample weights
lcfs <- merge(lcfs, sample_weights, by = "ENTERPRISE_CUSTOMER_NUM", all.x = TRUE) # Join on sample weights
lcfs <- lcfs[!is.na(lcfs$weight),] # Drop if missing

# Aggregate to get weighted mean value
lcfs_agg <- lcfs[, list(total_weight_purchased = weighted.mean(total_weight_purchased, weight, na.rm = TRUE)), by = c("lcfs_cat", "IMD_Decile")]

# Calculate percentage values - weighted data
lcfs_agg$percent_weight <- NA # Blank variable
lcfs_agg$percent_weight[lcfs_agg$IMD_Decile == 1] <- (lcfs_agg$total_weight_purchased[lcfs_agg$IMD_Decile == 1] / sum(lcfs_agg$total_weight_purchased[lcfs_agg$IMD_Decile == 1])) * 100 # Do for just IMD=1
lcfs_agg$percent_weight[lcfs_agg$IMD_Decile == 10] <- (lcfs_agg$total_weight_purchased[lcfs_agg$IMD_Decile == 10] / sum(lcfs_agg$total_weight_purchased[lcfs_agg$IMD_Decile == 10])) * 100 # Repeat for IMD=10

# Save for outputting
write.csv(lcfs_agg, "../Outgoing/19082025/imd_decile_lcfs_purchased.csv")

# Get data ready to plot
lcfs_agg_wide <- dcast(lcfs_agg, lcfs_cat ~ IMD_Decile, value.var = c("percent_weight", "total_weight_purchased")) # Shift to wide format
lcfs_agg_wide$abs_difference <- lcfs_agg_wide$total_weight_purchased_1 - lcfs_agg_wide$total_weight_purchased_10 # Calculate absolute difference
lcfs_agg_wide$rel_difference <- (lcfs_agg_wide$percent_weight_1 / lcfs_agg_wide$percent_weight_10) - 1 # Calculate relative difference
rm(lcfs_agg)

# Plots
ggplot(lcfs_agg_wide, aes(x = rel_difference, y = lcfs_cat)) + # Relative differences
  geom_bar(stat = "identity") +
  scale_x_continuous(breaks = seq(-1,3,0.5), labels = seq(0,4,0.5))

ggplot(lcfs_agg_wide, aes(x = abs_difference, y = lcfs_cat)) + # Absolute differences
  geom_bar(stat = "identity") 

# Tidy
rm(lcfs_agg_wide)
gc()



# # LCFS categories by IMD and HFSS #
# 
# # Load and clean
# imd_hfss <- fread("./Processed data/agg_hfss_imd_2022.csv") # Load
# imd_hfss <- imd_hfss[imd_hfss$lcfs_cat != "",] # Drop missing category
# imd_hfss <- imd_hfss[!is.na(imd_hfss$HFSS_status),] # Drop NAs
# 
# # Get total weight purchased for each person
# imd_hfss_tot <- imd_hfss[, list(all_weight_purchased = sum(total_weight_purchased, na.rm = TRUE), all_purchases = sum(total_purchases, na.rm = TRUE)), by = "ENTERPRISE_CUSTOMER_NUM"] # Aggregate
# imd_hfss <- merge(imd_hfss, imd_hfss_tot, by = "ENTERPRISE_CUSTOMER_NUM", all.x = TRUE) # Join back on
# rm(imd_hfss_tot)
# 
# # Calculate percentage values
# imd_hfss$percent_purchases <- (imd_hfss$total_purchases / imd_hfss$all_purchases) * 100
# imd_hfss$percent_weight <- (imd_hfss$total_weight_purchased / imd_hfss$all_weight_purchased) * 100
# 
# # Join on sample weights
# imd_hfss <- merge(imd_hfss, sample_weights, by = "ENTERPRISE_CUSTOMER_NUM", all.x = TRUE) # Join on sample weights
# imd_hfss <- imd_hfss[!is.na(imd_hfss$weight),] # Drop if missing
# 
# # Aggregate to get weighted mean value
# imd_hfss_agg <- imd_hfss[, list(percent_weight = weighted.mean(percent_weight, weight, na.rm = TRUE), total_weight_purchased = weighted.mean(total_weight_purchased, weight, na.rm = TRUE)), by = c("lcfs_cat", "IMD_Decile", "HFSS_status")]
# rm(imd_hfss)
# 
# # Get data ready to plot
# imd_hfss_agg_wide <- dcast(imd_hfss_agg, lcfs_cat  + HFSS_status ~ IMD_Decile, value.var = c("percent_weight", "total_weight_purchased")) # Shift to wide format
# imd_hfss_agg_wide$abs_difference <- imd_hfss_agg_wide$total_weight_purchased_1 - imd_hfss_agg_wide$total_weight_purchased_10 # Calculate absolute difference
# imd_hfss_agg_wide$rel_difference <- (imd_hfss_agg_wide$percent_weight_1 / imd_hfss_agg_wide$percent_weight_10) -1 # Calculate relative difference
# rm(imd_hfss_agg)
# 
# # Plot
# ggplot(imd_hfss_agg_wide, aes(x = rel_difference, y = lcfs_cat)) +
#   geom_bar(stat = "identity") +
#   facet_wrap(~HFSS_status) +
#   scale_x_continuous(breaks = seq(-1,3,0.5), labels = seq(0,4,0.5))
# 
# ggplot(imd_hfss_agg_wide, aes(x = rel_difference, y = lcfs_cat)) +
#   geom_point(stat = "identity") # , position = position_dodge(0.1)) +
#   scale_x_continuous(breaks = seq(-1,3,0.5), labels = seq(0,4,0.5))
# 
# ggplot(imd_hfss_agg_wide, aes(x = abs_difference, y = lcfs_cat)) +
#   geom_point(stat = "identity") 
# 
# # Tidy
# rm(imd_hfss_agg_wide)
# gc()



# LCFS categories by GHGE and IMD #
# Ignoring teriles

# Load and clean
imd_ghge <- fread("./Processed data/agg_ghge_imd_2022.csv") # Load
imd_ghge <- imd_ghge[imd_ghge$lcfs_cat != "",] # Drop missing category

# Get total weight purchased for each person
imd_ghge <- imd_ghge[, list(total_items = sum(total_items , na.rm = TRUE), total_purchases = sum(total_purchases, na.rm = TRUE), total_weight_purchased = sum(all_total_items = sum(total_items , na.rm = TRUE), total_weight_purchased, na.rm = TRUE), ghg_emissions = sum(ghg_emissions, na.rm = TRUE)), by = c("ENTERPRISE_CUSTOMER_NUM", "lcfs_cat", "IMD_Decile")] # Aggregate as some cases are split for whatever reason

# Join on sample weights
imd_ghge <- merge(imd_ghge, sample_weights, by = "ENTERPRISE_CUSTOMER_NUM", all.x = TRUE) # Join on sample weights
imd_ghge <- imd_ghge[!is.na(imd_ghge$weight),] # Drop if missing

# Aggregate to get weighted mean value
lcfs_agg <- lcfs[, list(total_weight_purchased = weighted.mean(total_weight_purchased, weight, na.rm = TRUE)), by = c("lcfs_cat", "IMD_Decile")]

# Aggregate to get weighted mean value (average total values)
imd_ghge_agg <- imd_ghge[, list(total_weight_purchased = weighted.mean(total_weight_purchased, weight, na.rm = TRUE), total_ghge_emissions = weighted.mean(ghg_emissions, weight, na.rm = TRUE)), by = c("lcfs_cat", "IMD_Decile")]
rm(imd_ghge)

# Calculate measures
imd_ghge_agg$percent_all_emissions <- NA # As percent of all emissions
imd_ghge_agg$percent_all_emissions[imd_ghge_agg$IMD_Decile == 1] <- (imd_ghge_agg$total_ghge_emissions[imd_ghge_agg$IMD_Decile == 1] / sum(imd_ghge_agg$total_ghge_emissions[lcfs_agg$IMD_Decile == 1])) * 100 # Do for just IMD=1
imd_ghge_agg$percent_all_emissions[imd_ghge_agg$IMD_Decile == 10] <- (imd_ghge_agg$total_ghge_emissions[imd_ghge_agg$IMD_Decile == 10] / sum(imd_ghge_agg$total_ghge_emissions[imd_ghge_agg$IMD_Decile == 10])) * 100 # Repeat for IMD=10

imd_ghge_agg$ghge_rate <- NA # As rate (ghge / total weight)
imd_ghge_agg$ghge_rate[imd_ghge_agg$IMD_Decile == 1] <- (imd_ghge_agg$total_ghge_emissions[imd_ghge_agg$IMD_Decile == 1] / imd_ghge_agg$total_weight_purchased[imd_ghge_agg$IMD_Decile == 1]) # Do for just IMD=1
imd_ghge_agg$ghge_rate[imd_ghge_agg$IMD_Decile == 10] <- (imd_ghge_agg$total_ghge_emissions[imd_ghge_agg$IMD_Decile == 10] / imd_ghge_agg$total_weight_purchased[imd_ghge_agg$IMD_Decile == 10]) # Repeat for IMD=10

# Save for outputting
write.csv(imd_ghge_agg, "../Outgoing/19082025/imd_decile_lcfs_ghge.csv")

# Get data ready to plot
imd_ghge_agg_wide <- dcast(imd_ghge_agg, lcfs_cat ~ IMD_Decile, value.var = c("percent_all_emissions", "ghge_rate", "ghge_rate")) # Shift to wide format
imd_ghge_agg_wide$abs_difference <- imd_ghge_agg_wide$ghge_rate_1 - imd_ghge_agg_wide$ghge_rate_10 # Calculate absolute difference in ghge rate
imd_ghge_agg_wide$rel_difference <- ((imd_ghge_agg_wide$ghge_rate_1 / imd_ghge_agg_wide$ghge_rate_10) - 1 ) * 100 # Calculate relative difference in ghge rate
imd_ghge_agg_wide$abs_pc_dif <- (imd_ghge_agg_wide$percent_all_emissions_1 - imd_ghge_agg_wide$percent_all_emissions_10) # Calculate absolute difference in ghge %


# Plot
ggplot(imd_ghge_agg_wide, aes(x = rel_difference, y = lcfs_cat)) +
  geom_bar(stat = "identity") +
  scale_x_continuous(breaks = seq(-15,20,5)) +
  xlab("Relative difference (%)") +
  ylab("Food category")

ggplot(imd_ghge_agg_wide, aes(x = abs_difference, y = lcfs_cat)) +
  geom_bar(stat = "identity") 

ggplot(imd_ghge_agg_wide, aes(x = abs_pc_dif, y = lcfs_cat)) +
  geom_bar(stat = "identity") 


# # LCFS categories by GHGE and IMD #
# # Terile version
# 
# # Load and clean
# imd_ghge <- fread("./Processed data/agg_ghge_imd_2022.csv") # Load
# imd_ghge <- imd_ghge[imd_ghge$lcfs_cat != "",] # Drop missing category
# imd_ghge <- imd_ghge[imd_ghge$ghge_tertile != "",] # Drop NAs
# 
# # Get total weight purchased for each person
# #imd_ghge <- imd_ghge[, list(total_items = sum(total_items , na.rm = TRUE), total_purchases = sum(total_purchases, na.rm = TRUE), total_weight_purchased = sum(all_total_items = sum(total_items , na.rm = TRUE), total_weight_purchased, na.rm = TRUE), ghg_emissions = sum(ghg_emissions, na.rm = TRUE)), by = c("ENTERPRISE_CUSTOMER_NUM", "ghge_tertile", "lcfs_cat", "IMD_Decile")] # Aggregate as some cases are split for whatever reason
# imd_ghge_tot <- imd_ghge[, list(all_weight_purchased = sum(total_weight_purchased, na.rm = TRUE), all_purchases = sum(total_purchases, na.rm = TRUE), all_ghg_emissions = sum(ghg_emissions, na.rm = TRUE)), by = "ENTERPRISE_CUSTOMER_NUM"] # Aggregate to get total overall for a person for all products (denominator)
# imd_ghge <- merge(imd_ghge, imd_ghge_tot, by = "ENTERPRISE_CUSTOMER_NUM", all.x = TRUE) # Join back on
# rm(imd_ghge_tot)
# 
# # Calculate percentage values
# imd_ghge$percent_purchases <- (imd_ghge$total_purchases / imd_ghge$all_purchases) * 100
# imd_ghge$percent_weight <- (imd_ghge$total_weight_purchased / imd_ghge$all_weight_purchased) * 100
# 
# # Join on sample weights
# imd_ghge <- merge(imd_ghge, sample_weights, by = "ENTERPRISE_CUSTOMER_NUM", all.x = TRUE) # Join on sample weights
# imd_ghge <- imd_ghge[!is.na(imd_ghge$weight),] # Drop if missing
# 
# # Aggregate to get weighted mean value
# imd_ghge_agg <- imd_ghge[, list(percent_weight = weighted.mean(percent_weight, weight, na.rm = TRUE), total_weight_purchased = weighted.mean(total_weight_purchased, weight, na.rm = TRUE)), by = c("lcfs_cat", "IMD_Decile", "ghge_tertile")]
# rm(imd_ghge)
# 
# # Get data ready to plot
# imd_ghge_agg_wide <- dcast(imd_ghge_agg, lcfs_cat + ghge_tertile ~ IMD_Decile, value.var = c("percent_weight", "total_weight_purchased")) # Shift to wide format
# imd_ghge_agg_wide$abs_difference <- imd_ghge_agg_wide$total_weight_purchased_1 - imd_ghge_agg_wide$total_weight_purchased_10 # Calculate absolute difference
# imd_ghge_agg_wide$rel_difference <- (imd_ghge_agg_wide$percent_weight_1 / imd_ghge_agg_wide$percent_weight_10) -1 # Calculate relative difference
# 
# # Plot
# #ggplot(imd_ghge_agg_wide, aes(x = rel_difference, y = lcfs_cat)) +
# #  geom_bar(stat = "identity") +
# #  facet_wrap(~ghge_tertile) +
# #  scale_x_continuous(breaks = seq(-1,3,0.5), labels = seq(0,4,0.5))
# 
# ggplot(imd_ghge_agg_wide, aes(x = rel_difference, y = lcfs_cat, group = ghge_tertile, color = ghge_tertile)) +
#   geom_point(stat = "identity", position = position_dodge(0.1)) +
#   scale_x_continuous(breaks = seq(-1,3,0.5), labels = seq(0,4,0.5))
# 
# ggplot(imd_ghge_agg_wide, aes(x = abs_difference, y = lcfs_cat, group = ghge_tertile, color = ghge_tertile)) +
#   geom_point(stat = "identity") 



# LCFS categories by land use and IMD #
# Ignoring teriles

# Load and clean
land <- fread("./Processed data/agg_land_imd_2022.csv") # Load
land <- land[land$lcfs_cat != "",] # Drop missing category

# Get total weight purchased for each person
land <- land[, list(total_items = sum(total_items , na.rm = TRUE), total_purchases = sum(total_purchases, na.rm = TRUE), total_weight_purchased = sum(all_total_items = sum(total_items , na.rm = TRUE), total_weight_purchased, na.rm = TRUE), land_use = sum(land_use, na.rm = TRUE)), by = c("ENTERPRISE_CUSTOMER_NUM", "lcfs_cat", "IMD_Decile")] # Aggregate as some cases are split for whatever reason

# Join on sample weights
land <- merge(land, sample_weights, by = "ENTERPRISE_CUSTOMER_NUM", all.x = TRUE) # Join on sample weights
land <- land[!is.na(land$weight),] # Drop if missing

# Aggregate to get weighted mean value (average total values)
land_agg <- land[, list(total_weight_purchased = weighted.mean(total_weight_purchased, weight, na.rm = TRUE), total_land_use = weighted.mean(land_use, weight, na.rm = TRUE)), by = c("lcfs_cat", "IMD_Decile")]
rm(imd_ghge)

# Calculate measures
land_agg$percent_all_emissions <- NA # As percent of all emissions
land_agg$percent_all_emissions[land_agg$IMD_Decile == 1] <- (land_agg$total_land_use[land_agg$IMD_Decile == 1] / sum(land_agg$total_land_use[lcfs_agg$IMD_Decile == 1])) * 100 # Do for just IMD=1
land_agg$percent_all_emissions[land_agg$IMD_Decile == 10] <- (land_agg$total_land_use[land_agg$IMD_Decile == 10] / sum(land_agg$total_land_use[land_agg$IMD_Decile == 10])) * 100 # Repeat for IMD=10

land_agg$land_rate <- NA # As rate (ghge / total weight)
land_agg$land_rate[land_agg$IMD_Decile == 1] <- (land_agg$total_land_use[land_agg$IMD_Decile == 1] / land_agg$total_weight_purchased[land_agg$IMD_Decile == 1]) # Do for just IMD=1
land_agg$land_rate[land_agg$IMD_Decile == 10] <- (land_agg$total_land_use[land_agg$IMD_Decile == 10] / land_agg$total_weight_purchased[land_agg$IMD_Decile == 10]) # Repeat for IMD=10

# Save for outputting
write.csv(land_agg, "../Outgoing/19082025/imd_decile_lcfs_landuse.csv")

# Get data ready to plot
land_agg_wide <- dcast(land_agg, lcfs_cat ~ IMD_Decile, value.var = c("percent_all_emissions", "land_rate")) # Shift to wide format
land_agg_wide$abs_difference <- land_agg_wide$land_rate_1 - land_agg_wide$land_rate_10 # Calculate absolute difference in ghge rate
land_agg_wide$rel_difference <- ((land_agg_wide$land_rate_1 / land_agg_wide$land_rate_10) - 1 ) * 100 # Calculate relative difference in ghge rate
land_agg_wide$abs_pc_dif <- (land_agg_wide$percent_all_emissions_1 - land_agg_wide$percent_all_emissions_10) # Calculate absolute difference in ghge %


# Plot
ggplot(land_agg_wide, aes(x = rel_difference, y = lcfs_cat)) +
  geom_bar(stat = "identity") +
  scale_x_continuous(breaks = seq(-30,15,5)) +
  xlab("Relative difference (%)") +
  ylab("Food category")

ggplot(land_agg_wide, aes(x = abs_difference, y = lcfs_cat)) +
  geom_bar(stat = "identity") 

ggplot(land_agg_wide, aes(x = abs_pc_dif, y = lcfs_cat)) +
  geom_bar(stat = "identity") 



# # LCFS categories by Land use and IMD #
# # Tertiles # 
# 
# # Load and clean
# imd_land <- fread("./Processed data/agg_land_imd_2022.csv") # Load
# imd_land <- imd_land[imd_land$lcfs_cat != "",] # Drop missing category
# imd_land <- imd_land[imd_land$land_tertile != "",] # Drop NAs
# 
# # Get total weight purchased for each person
# imd_land_tot <- imd_land[, list(all_weight_purchased = sum(total_weight_purchased, na.rm = TRUE), all_purchases = sum(total_purchases, na.rm = TRUE)), by = "ENTERPRISE_CUSTOMER_NUM"] # Aggregate
# imd_land <- merge(imd_land, imd_land_tot, by = "ENTERPRISE_CUSTOMER_NUM", all.x = TRUE) # Join back on
# rm(imd_land_tot)
# 
# # Calculate percentage values
# imd_land$percent_purchases <- (imd_land$total_purchases / imd_land$all_purchases) * 100
# imd_land$percent_weight <- (imd_land$total_weight_purchased / imd_land$all_weight_purchased) * 100
# 
# # Join on sample weights
# imd_land <- merge(imd_land, sample_weights, by = "ENTERPRISE_CUSTOMER_NUM", all.x = TRUE) # Join on sample weights
# imd_land <- imd_land[!is.na(imd_land$weight),] # Drop if missing
# 
# # Aggregate to get weighted mean value
# imd_land_agg <- imd_land[, list(percent_weight = weighted.mean(percent_weight, weight, na.rm = TRUE), total_weight_purchased = weighted.mean(total_weight_purchased, weight, na.rm = TRUE)), by = c("lcfs_cat", "IMD_Decile", "land_tertile")]
# rm(imd_land)
# 
# # Get data ready to plot
# imd_land_agg_wide <- dcast(imd_land_agg, lcfs_cat + land_tertile ~ IMD_Decile, value.var = c("percent_weight", "total_weight_purchased")) # Shift to wide format
# imd_land_agg_wide$abs_difference <- imd_land_agg_wide$total_weight_purchased_1 - imd_land_agg_wide$total_weight_purchased_10 # Calculate absolute difference
# imd_land_agg_wide$rel_difference <- (imd_land_agg_wide$percent_weight_1 / imd_land_agg_wide$percent_weight_10) - 1 # Calculate relative difference
# 
# # Plot
# ggplot(imd_land_agg_wide, aes(x = rel_difference, y = lcfs_cat)) +
#   geom_bar(stat = "identity") +
#   facet_wrap(~land_tertile) +
#   scale_x_continuous(breaks = seq(-1,3,0.5), labels = seq(0,4,0.5))
# 
# ggplot(imd_land_agg_wide, aes(x = rel_difference, y = lcfs_cat, group = land_tertile, color = land_tertile)) +
#   geom_point(stat = "identity", position = position_dodge(0.1)) +
#   scale_x_continuous(breaks = seq(-1,3,0.5), labels = seq(0,4,0.5))
# 
# ggplot(imd_land_agg_wide, aes(x = abs_difference, y = lcfs_cat, group = land_tertile, color = land_tertile)) +
#   geom_point(stat = "identity") 



# LCFS categories by water and IMD #
# Ignoring teriles

# Load and clean
water <- fread("./Processed data/agg_water_imd_2022.csv") # Load
water <- water[water$lcfs_cat != "",] # Drop missing category

# Get total weight purchased for each person
water <- water[, list(total_items = sum(total_items , na.rm = TRUE), total_purchases = sum(total_purchases, na.rm = TRUE), total_weight_purchased = sum(all_total_items = sum(total_items , na.rm = TRUE), total_weight_purchased, na.rm = TRUE), water_use = sum(water_use, na.rm = TRUE)), by = c("ENTERPRISE_CUSTOMER_NUM", "lcfs_cat", "IMD_Decile")] # Aggregate as some cases are split for whatever reason


# Join on sample weights
water <- merge(water, sample_weights, by = "ENTERPRISE_CUSTOMER_NUM", all.x = TRUE) # Join on sample weights
water <- water[!is.na(water$weight),] # Drop if missing

# Aggregate to get weighted mean value (average total values)
water_agg <- water[, list(total_weight_purchased = weighted.mean(total_weight_purchased, weight, na.rm = TRUE), total_water_use = weighted.mean(water_use, weight, na.rm = TRUE)), by = c("lcfs_cat", "IMD_Decile")]
rm(water)

# Calculate measures
water_agg$percent_all_emissions <- NA # As percent of all emissions
water_agg$percent_all_emissions[water_agg$IMD_Decile == 1] <- (water_agg$total_water_use[water_agg$IMD_Decile == 1] / sum(water_agg$total_water_use[water_agg$IMD_Decile == 1])) * 100 # Do for just IMD=1
water_agg$percent_all_emissions[water_agg$IMD_Decile == 10] <- (water_agg$total_water_use[water_agg$IMD_Decile == 10] / sum(water_agg$total_water_use[water_agg$IMD_Decile == 10])) * 100 # Repeat for IMD=10

water_agg$water_rate <- NA # As rate (ghge / total weight)
water_agg$water_rate[water_agg$IMD_Decile == 1] <- (water_agg$total_water_use[water_agg$IMD_Decile == 1] / water_agg$total_weight_purchased[water_agg$IMD_Decile == 1]) # Do for just IMD=1
water_agg$water_rate[water_agg$IMD_Decile == 10] <- (water_agg$total_water_use[water_agg$IMD_Decile == 10] / water_agg$total_weight_purchased[water_agg$IMD_Decile == 10]) # Repeat for IMD=10

# Save for outputting
write.csv(water_agg, "../Outgoing/19082025/imd_decile_lcfs_wateruse.csv")



# Get data ready to plot
water_agg_wide <- dcast(water_agg, lcfs_cat ~ IMD_Decile, value.var = c("percent_all_emissions", "water_rate")) # Shift to wide format
water_agg_wide$abs_difference <- water_agg_wide$water_rate_1 - water_agg_wide$water_rate_10 # Calculate absolute difference in ghge rate
water_agg_wide$rel_difference <- ((water_agg_wide$water_rate_1 / water_agg_wide$water_rate_10) - 1 ) * 100 # Calculate relative difference in ghge rate
water_agg_wide$abs_pc_dif <- (water_agg_wide$percent_all_emissions_1 - water_agg_wide$percent_all_emissions_10) # Calculate absolute difference in ghge %


# Plot
ggplot(water_agg_wide, aes(x = rel_difference, y = lcfs_cat)) +
  geom_bar(stat = "identity") +
  #scale_x_continuous(breaks = seq(-20,15,5)) +
  xlab("Relative difference (%)") +
  ylab("Food category")

ggplot(water_agg_wide, aes(x = abs_difference, y = lcfs_cat)) +
  geom_bar(stat = "identity") 

ggplot(water_agg_wide, aes(x = abs_pc_dif, y = lcfs_cat)) +
  geom_bar(stat = "identity") 




# Eatwell categories by IMD #

# Load and clean
imd_eatwell <- fread("./Processed data/agg_eatwell_imd_2022.csv") # Load
imd_eatwell <- imd_eatwell[imd_eatwell$Eatwell.segment != "",] # Drop missing category
imd_eatwell$Eatwell.segment[imd_eatwell$Eatwell.segment == "dairy"] <- "Dairy" # Recode as duplicate category

# Join on sample weights
imd_eatwell <- merge(imd_eatwell, sample_weights, by = "ENTERPRISE_CUSTOMER_NUM", all.x = TRUE) # Join on sample weights
imd_eatwell <- imd_eatwell[!is.na(imd_eatwell$weight),] # Drop if missing

# Aggregate to get weighted mean weight purchased by category
imd_eatwell_agg <- imd_eatwell[, list(total_weight_weighted = weighted.mean(total_weight_purchased, weight, na.rm = TRUE), total_weight_unweighted = mean(total_weight_purchased, na.rm = TRUE)), by = c("Eatwell.segment", "IMD_Decile")]
rm(imd_eatwell)

# Calculate percentage values - weighted data
imd_eatwell_agg$percent_weight[imd_eatwell_agg$IMD_Decile == 1] <- (imd_eatwell_agg$total_weight_weighted[imd_eatwell_agg$IMD_Decile == 1] / sum(imd_eatwell_agg$total_weight_weighted[imd_eatwell_agg$IMD_Decile == 1])) * 100 # Do for just IMD=1
imd_eatwell_agg$percent_weight[imd_eatwell_agg$IMD_Decile == 10] <- (imd_eatwell_agg$total_weight_weighted[imd_eatwell_agg$IMD_Decile == 10] / sum(imd_eatwell_agg$total_weight_weighted[imd_eatwell_agg$IMD_Decile == 10])) * 100 # Repeat for IMD=10

# Calculate percentage values - unweighted data
imd_eatwell_agg$percent_unweight[imd_eatwell_agg$IMD_Decile == 1] <- (imd_eatwell_agg$total_weight_unweighted[imd_eatwell_agg$IMD_Decile == 1] / sum(imd_eatwell_agg$total_weight_unweighted[imd_eatwell_agg$IMD_Decile == 1])) * 100 # Do for just IMD=1
imd_eatwell_agg$percent_unweight[imd_eatwell_agg$IMD_Decile == 10] <- (imd_eatwell_agg$total_weight_unweighted[imd_eatwell_agg$IMD_Decile == 10] / sum(imd_eatwell_agg$total_weight_unweighted[imd_eatwell_agg$IMD_Decile == 10])) * 100 # Repeat for IMD=10

# Print table
imd_eatwell_agg 

# Save for outputting
write.csv(imd_eatwell_agg, "../Outgoing/19082025/imd_decile_eatwell.csv")

# Plots:

# Pie chart
ggplot(imd_eatwell_agg, aes(x = "", y = percent_weight, fill = Eatwell.segment)) +
  geom_col(width = 1) +
  facet_wrap(~IMD_Decile) +
  coord_polar(theta = "y")

# Bar chart pf differences
imd_eatwell_agg_wide <- dcast(imd_eatwell_agg, Eatwell.segment ~ IMD_Decile, value.var = "percent_weight") # Shift to wide format
imd_eatwell_agg_wide$abs_difference <- (imd_eatwell_agg_wide$`1` - imd_eatwell_agg_wide$`10`) # Calculate absolute difference
imd_eatwell_agg_wide$rel_difference <- (imd_eatwell_agg_wide$`1` / imd_eatwell_agg_wide$`10`) - 1 # Calculate relative difference
#plot1 <- ggplot(imd_hfss_agg, aes(x = percent_weight, y = lcfs_cat, group = factor(IMD_Decile), fill = factor(IMD_Decile))) +
#  geom_bar(stat = "identity")
ggplot(imd_eatwell_agg_wide, aes(x = rel_difference * 100, y = Eatwell.segment)) +
  geom_bar(stat = "identity")

ggplot(imd_eatwell_agg_wide, aes(x = abs_difference, y = Eatwell.segment)) +
  geom_bar(stat = "identity") 


# Eatwell categories for all people #

# Load and clean
eatwell <- fread("./Processed data/agg_eatwell_all_2022.csv") # Load
eatwell <- eatwell[eatwell$Eatwell.segment != "",] # Drop missing category
eatwell$Eatwell.segment[eatwell$Eatwell.segment == "dairy"] <- "Dairy" # Recode as duplicate category

# Join on sample weights
eatwell <- merge(eatwell, sample_weights, by = "ENTERPRISE_CUSTOMER_NUM", all.x = TRUE) # Join on sample weights
eatwell <- eatwell[!is.na(eatwell$weight),] # Drop if missing

# Aggregate to get weighted mean weight purchased by category
eatwell_agg <- eatwell[, list(total_weight_weighted = weighted.mean(total_weight_purchased, weight, na.rm = TRUE), total_weight_unweighted = mean(total_weight_purchased, na.rm = TRUE)), by = c("Eatwell.segment")]
rm(eatwell)

# Calculate percentage values
eatwell_agg$percent_weight <- (eatwell_agg$total_weight_weighted / sum(eatwell_agg$total_weight_weighted)) * 100 # Weighted values
eatwell_agg$percent_unweight <- (eatwell_agg$total_weight_unweighted / sum(eatwell_agg$total_weight_unweighted)) * 100 # Unweighted values


# Print table
eatwell_agg 

# Save for outputting
write.csv(eatwell_agg, "../Outgoing/19082025/all_eatwell.csv")

# Plots:

# Pie chart
ggplot(eatwell_agg, aes(x = "", y = percent_weight, fill = Eatwell.segment)) +
  geom_col(width = 1) +
  coord_polar(theta = "y")

# Combine with above table
eatwell_agg$IMD_Decile <- "All"
table <- rbind(imd_eatwell_agg, eatwell_agg)


