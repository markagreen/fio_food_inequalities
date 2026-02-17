#############################
### Multi-level modelling ###
#############################

# Aim: Code for practising the multi-level modeling aspect of the analysis.

# Libraries
library(data.table)
library(lme4)
library(marginaleffects)
library(ggplot2)
library(viridisLite)
library(sf)

# Set seed to ensure random elements are replicable
set.seed(210)


## Load and tidy data ##

# Load files
all_consumers <- fread("./Processed data/all_2022.csv") # Aggregated food purchasing emissions data
setnames(all_consumers, old = "AGE_BAND_NAME", new = "AGE_BAND") # Rename
nrow(all_consumers) # Get sample size

# Subset regions
all_consumers <- all_consumers[all_consumers$CUSTOMER_REGION == "South East" | all_consumers$CUSTOMER_REGION == "Yorkshire and The Humber"] # Keep those regions with good coverage (as East/West Midlands has low spatial matching)
nrow(all_consumers) # Get sample size

# Load location data
locations <- fread("../../../_incoming-Fiofood/2025-01-30/HASH_OA_RGN.csv") # Customer residential location file
lkup <- fread("./Lookup files/Output_Area_to_Lower_Layer_Super_Output_Area_to_Middle_Layer_Super_Output_Area_to_Local_Authority_District__December_2020__Lookup_in_England_and_Wales.csv") # Output Area to LSOA lookup file
lkup <- lkup[, c("OA11CD", "LSOA11CD", "LAD20CD", "LAD20NM")] # Keep only columns required

# Join on customer information onto the purchasing data
locations <- merge(locations, lkup, by.x = "oa11", by.y = "OA11CD", all.x = TRUE) # Join on spatial identifiers to addresses
all_consumers <- merge(all_consumers, locations, by.x = "ENTERPRISE_CUSTOMER_NUM", by.y = "HASH_ENTERPRISE_CUSTOMER_ID", all.x = TRUE) # Join on spatial information to purchase data
rm(locations, lkup) # Tidy

# Join on additional geographical measures to be used in modelling
geodata_hrf <- fread("./Processed data/census_hhold_hrf.csv") # Load geographical data
xpred <- geodata_hrf[, list(IMD_Decile = max(IMD_Decile), RUC11CD = max(RUC11CD), median_distance_2021code = max(median_distance_2021code)), by = "LSOA11CD"] # Collapse data into LSOA values (use max but doesn't matter as all values will be the same)
all_consumers <- merge(all_consumers, xpred, by = "LSOA11CD", all.x = TRUE) # Join onto customer level data
rm(xpred) # Tidy

# Get missing records per enterprise ID
nrow(all_consumers) 
nrow(all_consumers[is.na(all_consumers$LSOA11CD)])  
nrow(all_consumers[is.na(all_consumers$IMD_Decile)]) 
nrow(all_consumers[is.na(all_consumers$AGE_BAND) | all_consumers$AGE_BAND == "" | all_consumers$AGE_BAND == "UNDER 18" | all_consumers$AGE_BAND == "UNKNOWN"]) 

# Drop people with missing records
all_consumers <- all_consumers[(!is.na(all_consumers$LSOA11CD)) & !is.na(all_consumers$IMD_Decile) & (!is.na(all_consumers$AGE_BAND) & all_consumers$AGE_BAND != "" & all_consumers$AGE_BAND != "UNDER 18" & all_consumers$AGE_BAND != "UNKNOWN"),] # Drop missing on three characteristics above
nrow(all_consumers) # analytical sample size 

# Recode household size variable
all_consumers$HOUSEHOLD_SIZE[all_consumers$HOUSEHOLD_SIZE > 4] <- 4 # 4 or more to match ONS data

# Create outcome variables
all_consumers$hfss_broad_weight_prop <- all_consumers$total_weight_purchased_hfss_broad / (all_consumers$total_weight_purchased_hfss_broad + all_consumers$total_weight_purchased_nonhfss_broad) # Proportion of purchases that are HFSS (by weight of purchases) - broad definition
all_consumers$hfss_broad_weight_prop[is.na(all_consumers$hfss_broad_weight_prop)] <- 0

all_consumers$hfss_narrow_weight_prop <- all_consumers$total_weight_purchased_hfss_narrow / (all_consumers$total_weight_purchased_hfss_narrow + all_consumers$total_weight_purchased_nonhfss_narrow) # Proportion of purchases that are HFSS (by weight of purchases) - narrow definition
all_consumers$hfss_narrow_weight_prop[is.na(all_consumers$hfss_narrow_weight_prop)] <- 0

all_consumers$ghq_rate <- all_consumers$ghg_emissions / all_consumers$total_weight_purchased # Sustainability metrics as a rate
all_consumers$land_rate <- all_consumers$land_use / all_consumers$total_weight_purchased
all_consumers$water_rate <- all_consumers$water_use / all_consumers$total_weight_purchased

# Drop people with low purchases
# quantile(all_consumers$total_energy, c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)) # Get percentiles (10% so deciles) for total energy purchased
all_consumers$energy_per_person <- all_consumers$total_energy / all_consumers$HOUSEHOLD_SIZE # Get energy per person
nrow(all_consumers) # Sample size 
all_consumers <- all_consumers[all_consumers$energy_per_person >= 161000 & !is.na(all_consumers$energy_per_person)] # Have purchased at least 1000 kcals per person per day over period (161 days)
nrow(all_consumers) # Sample size

# Save a list of customers to be used when creating dataset of aggregated food purchases later
hold <- all_consumers[, c("ENTERPRISE_CUSTOMER_NUM", "IMD_Decile")] # Subset variables needed
fwrite(hold, "./Processed data/all_ids_mlm.csv") # Save

# Create sample weights for any descriptive statistics elsewhere
# Aggregate loyalty card sample size per group in model
# Aggregate total population size per 

# Tidy
gc()


## Descriptive statistics ##

# Sample size
nrow(all_consumers)

# Number of people per LSOA
lsoa_tab <- data.frame(table(all_consumers$LSOA11CD)) # Save count of people per LSOA
summary(lsoa_tab) # Get summary statistics 
nrow(lsoa_tab[lsoa_tab$Freq < 10,]) # Number of LSOAs with less than 10 people
rm(lsoa_tab) # Tidy

# Key predictors
table(all_consumers$AGE_BAND)
table(all_consumers$HOUSEHOLD_SIZE)
table(all_consumers$IMD_Decile)
table(all_consumers$RUC11CD)
table(all_consumers$CUSTOMER_REGION)
summary(all_consumers$median_distance_2021code)

# Outcomes
summary(all_consumers$hfss_broad_weight_prop)
summary(all_consumers$hfss_narrow_weight_prop)
summary(all_consumers$ghg_emissions)
summary(all_consumers$ghq_rate)
summary(all_consumers$water_use)
summary(all_consumers$water_rate)
summary(all_consumers$land_use)
summary(all_consumers$land_rate)

# Other stats
tab1 <- all_consumers[, list(total_weight_purchased = mean(total_weight_purchased, na.rm = T)), by = "IMD_Decile"] # Mean weight purchased by IMD decile
tab2 <- all_consumers[, list(total_weight_purchased = mean(total_weight_purchased, na.rm = T)), by = c("IMD_Decile", "HOUSEHOLD_SIZE")] # Mean weight purchased by IMD decile and household size
cor(all_consumers$ghg_emissions, all_consumers$total_weight_purchased) # Correlation between emissions and weight purchased
rm(tab1, tab2) # Tidy


## MLM - lme4 ##

# Get poststratification data ready #

# Create predictive frame for geographical data based on the eventual multi-level model
geodata_hrf_long <- melt(data = geodata_hrf, # Convert to long format so have each household size option as one line
                         id.vars = c("LSOA21CD", "tab", "AGE_BAND", "LSOA11CD", "imd_rank", "IMD_Decile", "OAC_SubGroup", "RUC11CD", "RUC11", "LAD22CD", "population_density_km2", "stores", "median_distance_2011code", "median_distance_2021code"), # Keep these as columns
                         measure.vars = c("1 person in household", "2 people in household", "3 people in household", "4 or more people in household"), # Split out into rows
                         variable.name = "HOUSEHOLD_SIZE", # New variable name for household size
                         value.name = "population") # Capture the count of number of households
geodata_hrf_long[, HOUSEHOLD_SIZE := gsub(" person in household", "", HOUSEHOLD_SIZE)] # Remove text to match Sainsbury's variable
geodata_hrf_long[, HOUSEHOLD_SIZE := gsub(" people in household", "", HOUSEHOLD_SIZE)] # Repeat as people vs person
geodata_hrf_long[, HOUSEHOLD_SIZE := gsub(" or more", "", HOUSEHOLD_SIZE)] # Repeat again
geodata_hrf_long$HOUSEHOLD_SIZE <- as.numeric(geodata_hrf_long$HOUSEHOLD_SIZE) # Convert to numeric data type

geo_pred <- geodata_hrf_long[, c("LSOA11CD", "tab", "AGE_BAND", "HOUSEHOLD_SIZE", "IMD_Decile", "RUC11CD", "median_distance_2021code", "population")] # Subset variables required 
#geo_pred <- geo_pred[geo_pred$tab == "CT21_0329 South East" | geo_pred$tab == "CT21_0329 Yorkshire&The Humber"] # Subset only South East and Yorkshire
geo_pred$RUC11CD[geo_pred$RUC11CD == ""] <- NA # Set to NAs rather than own group

geo_pred[, CUSTOMER_REGION := gsub("CT21_0329 ", "", tab)] # Revise region variable 
geo_pred$CUSTOMER_REGION[geo_pred$CUSTOMER_REGION == "Yorkshire&The Humber"] <- "Yorkshire and The Humber" # Recode to match Sainsbury's
geo_pred$tab <- NULL # Delete variable
rm(geodata_hrf_long, geodata_hrf) # Tidy

# Aggretate counts for the creation of sample weights (for later descriptive statistics)
geo_pred_agg <- geo_pred[, list(population = sum(population, na.rm=T)), by = c("AGE_BAND", "HOUSEHOLD_SIZE", "IMD_Decile", "RUC11CD", "CUSTOMER_REGION")] # Get population count for all areas by defined strata groups
consumers_agg <- all_consumers[, list(sample_size = .N), by = c("AGE_BAND", "HOUSEHOLD_SIZE", "IMD_Decile", "RUC11CD", "CUSTOMER_REGION")] # Get counts of each strata within sample
consumers_agg <- merge(consumers_agg, geo_pred_agg, by = c("AGE_BAND", "HOUSEHOLD_SIZE", "IMD_Decile", "RUC11CD", "CUSTOMER_REGION"), all.x = TRUE) # Join together
consumers_agg$weight <- consumers_agg$population / consumers_agg$sample_size # Calculate weight
sample_weights <- merge(all_consumers, consumers_agg, by = c("AGE_BAND", "HOUSEHOLD_SIZE", "IMD_Decile", "RUC11CD", "CUSTOMER_REGION"), all.x = TRUE) # Join to main dataset
sample_weights <- sample_weights[, c("ENTERPRISE_CUSTOMER_NUM", "weight")] # Subset only those we need
fwrite(sample_weights, "./Processed data/sample_weights.csv") # Save
rm(sample_weights, geo_pred_agg, consumers_agg) # Tidy

# Tidy dataset into analysis ready format
analytical_sample <- all_consumers[, c("LSOA11CD", "ghg_emissions", "ghq_rate", "land_use", "land_rate", "water_use", "water_rate", "hfss_narrow_weight_prop", "hfss_broad_weight_prop", "AGE_BAND", "HOUSEHOLD_SIZE", "CUSTOMER_REGION", "IMD_Decile", "RUC11CD", "median_distance_2021code")] # Subset only variables needed in the analysis
analytical_sample$LSOA11CD <- as.factor(analytical_sample$LSOA11CD) # Set as factor
rm(hold) # Tidy




# GHG emissions overall #

# Model
mlm1 <- lmer(ghg_emissions ~ factor(AGE_BAND) + factor(HOUSEHOLD_SIZE) + factor(IMD_Decile) + factor(RUC11CD) + median_distance_2021code + (1 | LSOA11CD), data = analytical_sample) # Fit model 
summary(mlm1) # Glance at results
mlm1_sum <- data.frame(cbind(summary(mlm1)$coefficients, confint(mlm1, method = "Wald")[3:29,])) # Save model summary
mlm1_sum$term <- rownames(mlm1_sum) # Save name of variable (else not saved in the next step)
fwrite(mlm1_sum, "./Outputs/ghg_total_mlm_summary.csv")

# Make predictions
pred_lsoa <- predictions(model = mlm1, newdata = geo_pred, by = "LSOA11CD", wts = "population", allow.new.levels = TRUE) # LSOA total estimates
pred_imd <- predictions(model = mlm1, newdata = geo_pred, by = "IMD_Decile", wts = "population", allow.new.levels = TRUE) # Predict by IMD decile

# Compare to non-adjusted predictions
raw_imd <- all_consumers[, list(mean_ghg = mean(ghg_emissions, na.rm = TRUE)), by = "IMD_Decile"] # Get mean emissions per IMD decile
pred_imd <- merge(pred_imd, raw_imd, by = "IMD_Decile") # Create single table
pred_imd$dif <- pred_imd$estimate / pred_imd$mean_ghg # Relative difference in values
pred_imd # Print

# raw_lsoa <- all_consumers[, list(mean_ghg = mean(ghg_emissions, na.rm = TRUE)), by = "LSOA11CD"] # Get mean emissions per LSOA
# pred_lsoa <- merge(pred_lsoa, raw_lsoa, by = "LSOA11CD") # Create single table
# pred_lsoa$dif <- pred_lsoa$estimate - pred_lsoa$mean_ghg # Estimate change
# summary(pred_lsoa)

# Make nice plots

# Reshape IMD table for plotting purposes
pred_imd <- data.table(pred_imd)
fwrite(pred_imd, "./Outputs/imd_ghg_total_model.csv") # Save
pred_imd_long <- melt(pred_imd, id.vars = "IMD_Decile", measure.vars = c("estimate", "mean_ghg"), variable.name = "type", value.name = "value")
pred_imd_long <- cbind(pred_imd_long, pred_imd[, c("conf.low", "conf.high")])
pred_imd_long[11:20,4:5] <- NA

ggplot(pred_imd_long, aes(x = factor(IMD_Decile), y = value, group = type, color = type)) + 
  geom_errorbar(aes(x = factor(IMD_Decile), ymin = conf.low, ymax = conf.high, group = type, color = type), width = 0, position = position_dodge(width = 0.2)) +
  geom_point(size = 1.5, position = position_dodge(width = 0.2)) +
  ylim(0, max(pred_imd_long$value)) +
  labs(color = "Model", x = "Deprivation decile", y = "Greenhouse gas emissions (kg CO2 equiv.)", caption = "Decile 1 = most deprived, decile 10 = least deprived") +
  scale_colour_viridis_d(option = "turbo", begin = 0.1, end = 0.9, labels = c("estimate" = "Model", "mean_ghg" = "Raw")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Map data

# Tidy data
pred_lsoa <- data.table(pred_lsoa) # Get into format required
fwrite(pred_lsoa, "./Outputs/lsoa_ghg_total_model.csv") # Save
lsoa_sp <- read_sf("../../../_incoming-Fiofood/2024-08-14/LSOA_Dec_2011_Boundaries_Generalised_Clipped_BGC_EW_V3_-335161623626682850.geojson") # Load spatial boundaries for LSOAs
yorks_sp <- merge(lsoa_sp, pred_lsoa, by = "LSOA11CD", all.y = TRUE) # Join on estimates to LSOA spatial data
yorks_sp$lad_name <- gsub("\\s[0-9].*", "", yorks_sp$LSOA11NM) # Get Local Authority name

# Plot map for England
ggplot(yorks_sp) +
  geom_sf(aes(fill = estimate), color = NA) + # Plot estimated value
  scale_fill_viridis_c() + # Make colour blind friendly
  labs(fill = "Emissions", caption = "Mean annual household emissions per kg") # Add labels

# Plot map for one place
ggplot(yorks_sp[yorks_sp$lad_name == "Liverpool",]) + # Replace name to check other places
  geom_sf(aes(fill = estimate), color = NA) +
  scale_fill_viridis_c() +
  labs(fill = "Emissions", caption = "Mean annual household emissions per kg")

# Get estimates for parliamentary constituencies
oapc_lkup <- fread("N:/_incoming-Fiofood/2024-08-15/Output_area_(2011)_to_future_Parliamentary_Constituencies_Lookup_in_England_and_Wales.csv") # Load OA to PC lookup
oalsoa_lkup <- fread("./Lookup files/Output_Area_to_Lower_Layer_Super_Output_Area_to_Middle_Layer_Super_Output_Area_to_Local_Authority_District__December_2020__Lookup_in_England_and_Wales.csv") # Load OA to LSOA lookup
oapc_lkup <- merge(oapc_lkup, oalsoa_lkup, by = "OA11CD", all.x = TRUE) # Join together
lsoapc_lkup <- oapc_lkup[!duplicated(oapc_lkup$LSOA11CD)] # Get rid of duplicates
pred_lsoa <- merge(pred_lsoa, lsoapc_lkup, by = "LSOA11CD", all.x = TRUE) # Join lookup onto LSOA estimates
pred_lsoa <- data.table(pred_lsoa)
pred_pc <- pred_lsoa[, list(estimate = median(estimate, na.rm = TRUE)), by = c("PCON25CD", "PCON25NM")] # Get constituency estimates
fwrite(pred_pc, "./Outputs/constituency_estimates_ghg_emissions.csv") # Save
rm(oapc_lkup, oalsoa_lkup, lsoapc_lkup) # Tidy

# Plot this
pc_sp <- read_sf("../../../_incoming-Fiofood/2024-08-15/Westminster_Parliamentary_Constituencies_July_2024_Boundaries_UK_BSC_5645385384493466092.geojson") # Load spatial boundaries for LSOAs
yorkspc_sp <- merge(pc_sp, pred_pc, by.x = "PCON24CD", by.y = "PCON25CD", all.y = TRUE) # Join on estimates to LSOA spatial data
plot_pc <- ggplot(yorkspc_sp) +
  geom_sf(aes(fill = estimate), color = NA) +
  scale_fill_viridis_c() +
  labs(fill = "Greenhouse gas emissions", caption = "Mean annual household emissions per kg")
plot_pc
ggsave(plot = plot_pc, filename = "./Outputs/constituency_map.jpeg")
ggsave(plot = plot_pc, filename = "./Outputs/constituency_map_highres.jpeg", dpi = 1000)


# HFSS broad (by weight) #

# Model
mlm2 <- lmer(hfss_broad_weight_prop ~ factor(AGE_BAND) + factor(HOUSEHOLD_SIZE) + factor(IMD_Decile) + factor(RUC11CD) + median_distance_2021code + (1 | LSOA11CD), data = analytical_sample) # Fit model (~20 seconds to fit)
summary(mlm2) # Glance at results
mlm2_sum <- data.frame(cbind(summary(mlm2)$coefficients, confint(mlm2, method = "Wald")[3:29,])) # Save model summary
mlm2_sum$term <- rownames(mlm2_sum) # Save name of variable (else not saved in the next step)
fwrite(mlm2_sum, "./Outputs/hfss_broad_mlm_summary.csv")

# Make predictions
pred2_lsoa <- predictions(model = mlm2, newdata = geo_pred, by = "LSOA11CD", wts = "population", allow.new.levels = TRUE) # LSOA total estimates
fwrite(pred2_lsoa, "./Outputs/lsoa_hfss_broad_model.csv") # Save
pred2_imd <- predictions(model = mlm2, newdata = geo_pred, by = "IMD_Decile", wts = "population", allow.new.levels = TRUE) # Predict by IMD decile

# Compare to non-adjusted predictions
raw2_imd <- all_consumers[, list(hfss_broad_weight_prop = mean(hfss_broad_weight_prop, na.rm = TRUE)), by = "IMD_Decile"] # Get mean emissions per IMD decile
pred2_imd <- merge(pred2_imd, raw2_imd, by = "IMD_Decile") # Create single table
pred2_imd # Print

# raw2_lsoa <- all_consumers[, list(hfss_weight_prop = mean(hfss_weight_prop, na.rm = TRUE)), by = "LSOA11CD"] # Get mean emissions per LSOA
# pred2_lsoa <- merge(pred2_lsoa, raw2_lsoa, by = "LSOA11CD") # Create single table
# pred2_lsoa$dif <- pred2_lsoa$estimate - pred2_lsoa$hfss_weight_prop # Estimate change
# summary(pred2_lsoa)


# Make nice plots

# Reshape IMD table for plotting purposes
pred2_imd <- data.table(pred2_imd)
fwrite(pred2_imd, "./Outputs/imd_hfss_broad_model.csv") # Save
pred2_imd_long <- melt(pred2_imd, id.vars = "IMD_Decile", measure.vars = c("estimate", "hfss_broad_weight_prop"), variable.name = "type", value.name = "value")
pred2_imd_long <- cbind(pred2_imd_long, pred2_imd[, c("conf.low", "conf.high")])
pred2_imd_long[11:20,4:5] <- NA

ggplot(pred2_imd_long, aes(x = factor(IMD_Decile), y = value, group = type, color = type)) + 
  geom_errorbar(aes(x = factor(IMD_Decile), ymin = conf.low, ymax = conf.high, group = type, color = type), width = 0, position = position_dodge(width = 0.2)) +
  geom_point(size = 1.5, position = position_dodge(width = 0.2)) +
  ylim(0.085, max(pred2_imd_long$conf.high)) +
  labs(color = "Model", x = "Deprivation decile", y = "Proportion of purchases that are HFSS (by g)", caption = "Decile 1 = most deprived, decile 10 = least deprived") +
  scale_colour_viridis_d(option = "turbo", begin = 0.1, end = 0.9, labels = c("estimate" = "Model", "hfss_broad_weight_prop" = "Raw")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Plot differences in values between HFSS and GHQ across LSOAs
lsoa_all <- merge(pred_lsoa, pred2_lsoa, by = "LSOA11CD", all.x = TRUE) # Combine estimates
qplot(lsoa_all$estimate.y, lsoa_all$estimate.x) + # Plot
  labs(x = "Proportion of purchases as HFSS", y = "Greenhouse gas emissions (CO2 per kg)")
cor(lsoa_all$estimate.y[!is.na(lsoa_all$estimate.y)], lsoa_all$estimate.x[!is.na(lsoa_all$estimate.y)]) # Correaltion between them

# Create map of both data together

# Tidy data
lsoa_sp <- read_sf("../../../_incoming-Fiofood/2024-08-14/LSOA_Dec_2011_Boundaries_Generalised_Clipped_BGC_EW_V3_-335161623626682850.geojson") # Load spatial boundaries for LSOAs
yorks_sp <- merge(lsoa_sp, lsoa_all, by = "LSOA11CD", all.y = TRUE) # Join on estimates to LSOA spatial data


# Plot map for England
ggplot(yorks_sp) +
  geom_sf(aes(fill = estimate.x), color = NA) +
  scale_fill_viridis_c() +
  labs(fill = "Emissions", caption = "Mean annual household emissions per kg")

ggplot(yorks_sp) +
  geom_sf(aes(fill = estimate.y), color = NA) +
  scale_fill_viridis_c() +
  labs(fill = "Proportion", caption = "Proportion of sales that were HFSS (by g)")

# Plot map for one place
ggplot(yorks_sp[yorks_sp$LAD20NM == "Liverpool",]) +
  geom_sf(aes(fill = estimate.y), color = NA) +
  scale_fill_viridis_c() +
  labs(fill = "Proportion", caption = "Proportion of sales that were HFSS (by g)")

# Plot both together
library(biscale)
library(cowplot)
data <- bi_class(yorks_sp, x = estimate.y, y = estimate.x, style = "quantile", dim = 3) # Define bivariate classes for mapping
legend <- bi_legend(pal = "DkCyan", dim = 3, xlab = "Higher HFSS", ylab = "Higher GHQ", size = 8) # Store legend seperately
map <- ggplot() + # Make map
  geom_sf(data, mapping = aes(fill = bi_class), color = NA, show.legend = FALSE) +
  bi_scale_fill(pal = "DkCyan", dim = 3) + # or GrPink
  bi_theme()
ggdraw() + # Put together
  draw_plot(map, 0, 0, 1, 1) +
  draw_plot(legend, 0.05, 0.075, 0.2, 0.2) # First bit controls location (position from left (1 = right) * position from bottom (1 = top)), second bit controls size (so start plotting at 0.05 from left x0.075 from bottom, to size 0.2*0.2 i think)

# For a city
map <- ggplot() + # Make map
  geom_sf(data[data$LAD20NM == "Liverpool",], mapping = aes(fill = bi_class), color = NA, show.legend = FALSE) +
  bi_scale_fill(pal = "DkCyan", dim = 3) + # or GrPink
  bi_theme()
ggdraw() + # Put together
  draw_plot(map, 0, 0, 1, 1) +
  draw_plot(legend, 0.05, 0.075, 0.2, 0.2)
map # Print

map <- ggplot() + # Make map
  geom_sf(data[data$lad_name == "Kingston upon Hull",], mapping = aes(fill = bi_class), color = NA, show.legend = FALSE) +
  bi_scale_fill(pal = "DkCyan", dim = 3) + # or GrPink
  bi_theme()
ggdraw() + # Put together
  draw_plot(map, 0, 0, 1, 1) +
  draw_plot(legend, 0.05, 0.8, 0.2, 0.2)


# HFSS narrow #

# Model
mlm2n <- lmer(hfss_narrow_weight_prop ~ factor(AGE_BAND) + factor(HOUSEHOLD_SIZE) + factor(IMD_Decile) + factor(RUC11CD) + median_distance_2021code + (1 | LSOA11CD), data = analytical_sample) # Fit model (~20 seconds to fit)
summary(mlm2n) # Glance at results
mlm2n_sum <- data.frame(cbind(summary(mlm2n)$coefficients, confint(mlm2n, method = "Wald")[3:29,])) # Save model summary
mlm2n_sum$term <- rownames(mlm2n_sum) # Save name of variable (else not saved in the next step)
fwrite(mlm2n_sum, "./Outputs/hfss_narrow_mlm_summary.csv")

# Make predictions
pred2n_lsoa <- predictions(model = mlm2n, newdata = geo_pred, by = "LSOA11CD", wts = "population", allow.new.levels = TRUE) # LSOA total estimates
fwrite(pred2n_lsoa, "./Outputs/lsoa_hfss_narrow_model.csv") # Save
pred2n_imd <- predictions(model = mlm2n, newdata = geo_pred, by = "IMD_Decile", wts = "population", allow.new.levels = TRUE) # Predict by IMD decile

# Compare to non-adjusted predictions
raw2n_imd <- all_consumers[, list(hfss_broad_weight_prop = mean(hfss_narrow_weight_prop, na.rm = TRUE)), by = "IMD_Decile"] # Get mean emissions per IMD decile
pred2n_imd <- merge(pred2n_imd, raw2n_imd, by = "IMD_Decile") # Create single table
pred2n_imd # Print
pred2n_imd <- data.table(pred2n_imd)
fwrite(pred2n_imd, "./Outputs/imd_hfss_narrow_model.csv") # Save

# GHQ rate #

# Model
mlm3 <- lmer(ghq_rate ~ factor(AGE_BAND) + factor(HOUSEHOLD_SIZE) + factor(IMD_Decile) + factor(RUC11CD) + median_distance_2021code + (1 | LSOA11CD), data = analytical_sample) # Fit model 
summary(mlm3) # Glance at results
mlm3_sum <- data.frame(cbind(summary(mlm3)$coefficients, confint(mlm3, method = "Wald")[3:29,])) # Save model summary
mlm3_sum$term <- rownames(mlm3_sum) # Save name of variable (else not saved in the next step)
fwrite(mlm3_sum, "./Outputs/ghg_rate_mlm_summary.csv") # Save


# Make predictions
pred3_lsoa <- predictions(model = mlm3, newdata = geo_pred, by = "LSOA11CD", wts = "population", allow.new.levels = TRUE) # LSOA total estimates
fwrite(pred3_lsoa, "./Outputs/lsoa_ghg_rate_model.csv") # Save
pred3_imd <- predictions(model = mlm3, newdata = geo_pred, by = "IMD_Decile", wts = "population", allow.new.levels = TRUE) # Predict by IMD decile

# Compare to non-adjusted predictions
raw3_imd <- all_consumers[, list(ghq_rate = mean(ghq_rate, na.rm = TRUE)), by = "IMD_Decile"] # Get mean emissions per IMD decile
pred3_imd <- merge(pred3_imd, raw3_imd, by = "IMD_Decile") # Create single table
pred3_imd # Print


# Make nice plots


# Reshape IMD table for plotting purposes
pred3_imd <- data.table(pred3_imd)
fwrite(pred3_imd, "./Outputs/imd_ghg_rate_model.csv") # Save
pred3_imd_long <- melt(pred3_imd, id.vars = "IMD_Decile", measure.vars = c("estimate", "ghq_rate"), variable.name = "type", value.name = "value")
pred3_imd_long <- cbind(pred3_imd_long, pred3_imd[, c("conf.low", "conf.high")])
pred3_imd_long[11:20,4:5] <- NA

ggplot(pred3_imd_long, aes(x = factor(IMD_Decile), y = value, group = type, color = type)) + 
  geom_errorbar(aes(x = factor(IMD_Decile), ymin = conf.low, ymax = conf.high, group = type, color = type), width = 0, position = position_dodge(width = 0.2)) +
  geom_point(size = 1.5, position = position_dodge(width = 0.2)) +
  ylim(0.004, max(pred3_imd_long$conf.high)) +
  labs(color = "Model", x = "Deprivation decile", y = "Greenhouse gas emissions rate (per weight)", caption = "Decile 1 = most deprived, decile 10 = least deprived") +
  scale_colour_viridis_d(option = "turbo", begin = 0.1, end = 0.9, labels = c("estimate" = "Model", "ghq_rate" = "Raw")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Land use #

# Model
mlm4 <- lmer(land_use ~ factor(AGE_BAND) + factor(HOUSEHOLD_SIZE) + factor(IMD_Decile) + factor(RUC11CD) + median_distance_2021code + (1 | LSOA11CD), data = analytical_sample) # Fit model 
summary(mlm4) # Glance at results
mlm4_sum <- data.frame(cbind(summary(mlm4)$coefficients, confint(mlm4, method = "Wald")[3:29,])) # Save model summary
mlm4_sum$term <- rownames(mlm4_sum) # Save name of variable (else not saved in the next step)
fwrite(mlm4_sum, "./Outputs/land_use_total_mlm_summary.csv") # Save

# Make predictions
pred4_lsoa <- predictions(model = mlm4, newdata = geo_pred, by = "LSOA11CD", wts = "population", allow.new.levels = TRUE) # LSOA total estimates
fwrite(pred4_lsoa, "./Outputs/lsoa_land_use_model.csv") # Save
pred4_imd <- predictions(model = mlm4, newdata = geo_pred, by = "IMD_Decile", wts = "population", allow.new.levels = TRUE) # Predict by IMD decile

# Compare to non-adjusted predictions
raw4_imd <- all_consumers[, list(land_use = mean(land_use, na.rm = TRUE)), by = "IMD_Decile"] # Get mean emissions per IMD decile
pred4_imd <- merge(pred4_imd, raw4_imd, by = "IMD_Decile") # Create single table
pred4_imd # Print

# Make nice plots

# Reshape IMD table for plotting purposes
pred4_imd <- data.table(pred4_imd)
fwrite(pred4_imd, "./Outputs/imd_land_use_model.csv") # Save
pred4_imd_long <- melt(pred4_imd, id.vars = "IMD_Decile", measure.vars = c("estimate", "land_use"), variable.name = "type", value.name = "value")
pred4_imd_long <- cbind(pred4_imd_long, pred4_imd[, c("conf.low", "conf.high")])
pred4_imd_long[11:20,4:5] <- NA

ggplot(pred4_imd_long, aes(x = factor(IMD_Decile), y = value, group = type, color = type)) + 
  geom_errorbar(aes(x = factor(IMD_Decile), ymin = conf.low, ymax = conf.high, group = type, color = type), width = 0, position = position_dodge(width = 0.2)) +
  geom_point(size = 1.5, position = position_dodge(width = 0.2)) +
  ylim(0, max(pred4_imd_long$conf.high)) +
  labs(color = "Model", x = "Deprivation decile", y = "Land use (m2 year per kg)", caption = "Decile 1 = most deprived, decile 10 = least deprived") +
  scale_colour_viridis_d(option = "turbo", begin = 0.1, end = 0.9, labels = c("estimate" = "Model", "land_use" = "Raw")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Land use rate #

# Model
mlm4r <- lmer(land_rate ~ factor(AGE_BAND) + factor(HOUSEHOLD_SIZE) + factor(IMD_Decile) + factor(RUC11CD) + median_distance_2021code + (1 | LSOA11CD), data = analytical_sample) # Fit model 
summary(mlm4r) # Glance at results
mlm4r_sum <- data.frame(cbind(summary(mlm4r)$coefficients, confint(mlm4r, method = "Wald")[3:29,])) # Save model summary
mlm4r_sum$term <- rownames(mlm4r_sum) # Save name of variable (else not saved in the next step)
fwrite(mlm4r_sum, "./Outputs/land_use_rate_mlm_summary.csv") # Save

# Make predictions
pred4r_lsoa <- predictions(model = mlm4r, newdata = geo_pred, by = "LSOA11CD", wts = "population", allow.new.levels = TRUE) # LSOA total estimates
fwrite(pred4r_lsoa, "./Outputs/lsoa_land_rate_model.csv") # Save
pred4r_imd <- predictions(model = mlm4r, newdata = geo_pred, by = "IMD_Decile", wts = "population", allow.new.levels = TRUE) # Predict by IMD decile

# Compare to non-adjusted predictions
raw4r_imd <- all_consumers[, list(land_rate = mean(land_rate, na.rm = TRUE)), by = "IMD_Decile"] # Get mean emissions per IMD decile
pred4r_imd <- merge(pred4r_imd, raw4r_imd, by = "IMD_Decile") # Create single table
pred4r_imd # Print


# Make nice plots

# Reshape IMD table for plotting purposes
pred4r_imd <- data.table(pred4r_imd)
fwrite(pred4r_imd, "./Outputs/imd_land_rate_model.csv") # Save
pred4r_imd_long <- melt(pred4r_imd, id.vars = "IMD_Decile", measure.vars = c("estimate", "land_rate"), variable.name = "type", value.name = "value")
pred4r_imd_long <- cbind(pred4r_imd_long, pred4r_imd[, c("conf.low", "conf.high")])
pred4r_imd_long[11:20,4:5] <- NA

ggplot(pred4r_imd_long, aes(x = factor(IMD_Decile), y = value, group = type, color = type)) + 
  geom_errorbar(aes(x = factor(IMD_Decile), ymin = conf.low, ymax = conf.high, group = type, color = type), width = 0, position = position_dodge(width = 0.2)) +
  geom_point(size = 1.5, position = position_dodge(width = 0.2)) +
  ylim(0.5, max(pred4_imd_long$conf.high)) +
  labs(color = "Model", x = "Deprivation decile", y = "Land use (m2 year per kg) per weight", caption = "Decile 1 = most deprived, decile 10 = least deprived") +
  scale_colour_viridis_d(option = "turbo", begin = 0.1, end = 0.9, labels = c("estimate" = "Model", "land_rate" = "Raw")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Water use #

# Model
mlm5 <- lmer(water_use ~ factor(AGE_BAND) + factor(HOUSEHOLD_SIZE) + factor(IMD_Decile) + factor(RUC11CD) + median_distance_2021code + (1 | LSOA11CD), data = analytical_sample) # Fit model 
summary(mlm5) # Glance at results
mlm5_sum <- data.frame(cbind(summary(mlm5)$coefficients, confint(mlm5, method = "Wald")[3:29,])) # Save model summary
mlm5_sum$term <- rownames(mlm5_sum) # Save name of variable (else not saved in the next step)
fwrite(mlm5_sum, "./Outputs/water_use_total_mlm_summary.csv") # Save

# Make predictions
pred5_lsoa <- predictions(model = mlm5, newdata = geo_pred, by = "LSOA11CD", wts = "population", allow.new.levels = TRUE) # LSOA total estimates
fwrite(pred5_lsoa, "./Outputs/lsoa_water_use_model.csv") # Save
pred5_imd <- predictions(model = mlm5, newdata = geo_pred, by = "IMD_Decile", wts = "population", allow.new.levels = TRUE) # Predict by IMD decile

# Compare to non-adjusted predictions
raw5_imd <- all_consumers[, list(water_use = mean(water_use, na.rm = TRUE)), by = "IMD_Decile"] # Get mean emissions per IMD decile
pred5_imd <- merge(pred5_imd, raw5_imd, by = "IMD_Decile") # Create single table
pred5_imd # Print


# Make nice plots

# Reshape IMD table for plotting purposes
pred5_imd <- data.table(pred5_imd)
fwrite(pred5_imd, "./Outputs/imd_water_use_model.csv") # Save
pred5_imd_long <- melt(pred5_imd, id.vars = "IMD_Decile", measure.vars = c("estimate", "water_use"), variable.name = "type", value.name = "value")
pred5_imd_long <- cbind(pred5_imd_long, pred5_imd[, c("conf.low", "conf.high")])
pred5_imd_long[11:20,4:5] <- NA

ggplot(pred5_imd_long, aes(x = factor(IMD_Decile), y = value, group = type, color = type)) + 
  geom_errorbar(aes(x = factor(IMD_Decile), ymin = conf.low, ymax = conf.high, group = type, color = type), width = 0, position = position_dodge(width = 0.2)) +
  geom_point(size = 1.5, position = position_dodge(width = 0.2)) +
  ylim(0, max(pred5_imd_long$conf.high)) +
  labs(color = "Model", x = "Deprivation decile", y = "Water use (L/kg)", caption = "Decile 1 = most deprived, decile 10 = least deprived") +
  scale_colour_viridis_d(option = "turbo", begin = 0.1, end = 0.9, labels = c("estimate" = "Model", "water_use" = "Raw")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#L/kg


# Water use rate #

# Model
mlm5r <- lmer(water_rate ~ factor(AGE_BAND) + factor(HOUSEHOLD_SIZE) + factor(IMD_Decile) + factor(RUC11CD) + median_distance_2021code + (1 | LSOA11CD), data = analytical_sample) # Fit model 
summary(mlm5r) # Glance at results
mlm5r_sum <- data.frame(cbind(summary(mlm5r)$coefficients, confint(mlm5r, method = "Wald")[3:29,])) # Save model summary
mlm5r_sum$term <- rownames(mlm5r_sum) # Save name of variable (else not saved in the next step)
fwrite(mlm5r_sum, "./Outputs/water_use_rate_mlm_summary.csv") # Save

# Make predictions
pred5r_lsoa <- predictions(model = mlm5r, newdata = geo_pred, by = "LSOA11CD", wts = "population", allow.new.levels = TRUE) # LSOA total estimates
fwrite(pred5r_lsoa, "./Outputs/lsoa_water_rate_model.csv") # Save
pred5r_imd <- predictions(model = mlm5r, newdata = geo_pred, by = "IMD_Decile", wts = "population", allow.new.levels = TRUE) # Predict by IMD decile

# Compare to non-adjusted predictions
raw5r_imd <- all_consumers[, list(water_rate = mean(water_rate, na.rm = TRUE)), by = "IMD_Decile"] # Get mean emissions per IMD decile
pred5r_imd <- merge(pred5r_imd, raw5r_imd, by = "IMD_Decile") # Create single table
pred5r_imd # Print


# Make nice plots

# Reshape IMD table for plotting purposes
pred5r_imd <- data.table(pred5r_imd)
fwrite(pred5r_imd, "./Outputs/imd_water_rate_model.csv") # Save
pred5r_imd_long <- melt(pred5r_imd, id.vars = "IMD_Decile", measure.vars = c("estimate", "water_rate"), variable.name = "type", value.name = "value")
pred5r_imd_long <- cbind(pred5r_imd_long, pred5r_imd[, c("conf.low", "conf.high")])
pred5r_imd_long[11:20,4:5] <- NA

ggplot(pred5r_imd_long, aes(x = factor(IMD_Decile), y = value, group = type, color = type)) + 
  geom_errorbar(aes(x = factor(IMD_Decile), ymin = conf.low, ymax = conf.high, group = type, color = type), width = 0, position = position_dodge(width = 0.2)) +
  geom_point(size = 1.5, position = position_dodge(width = 0.2)) +
  ylim(0.0105, max(pred5_imd_long$conf.high)) +
  labs(color = "Model", x = "Deprivation decile", y = "Water use (L/kg) per weight (g)", caption = "Decile 1 = most deprived, decile 10 = least deprived") +
  scale_colour_viridis_d(option = "turbo", begin = 0.1, end = 0.9, labels = c("estimate" = "Model", "water_rate" = "Raw")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#L/kg


## End ##


