##################################
### Make final plots for paper ###
##### Final version of paper #####
##################################

# Aim: To create the plots used in the paper. 

# Libraries
library(ggplot2)
library(patchwork)
library(readxl)
library(data.table)


### Main body of the paper ###


## Figure 1 ##

# HFSS broad definition
hfss <- read.csv("../laser output feb 26/05022026/05022026/imd_hfss_broad_model.csv") # Load estimates for high in fat salt and/or sugar model (broad definition)
fig1a <- ggplot(hfss, aes(x = factor(IMD_Decile), y = (estimate*100))) + # Make plot. Define what to plot (model estimates converted to a percentage)
  geom_errorbar(aes(x = factor(IMD_Decile), ymin = (conf.low*100), ymax = (conf.high*100)), width = 0, position = position_dodge(width = 0.2)) + # Add error bars
  geom_point(size = 1.5, position = position_dodge(width = 0.2)) + # Plot points
  #ylim(0, max(pred_imd$value)) + # If want plot to start at 0
  labs(x = "Deprivation tenths", y = "HFSS purchases (%)") + # Label axes
  theme(text = element_text(size = 14)) # Adjust size of text
#fig1a # Print

# GHGE
ghge <- read.csv("../laser output feb 26/05022026/05022026/imd_ghg_total_model.csv") # Load estimates of greenhouse gas emissions (total) model
fig1b <- ggplot(ghge, aes(x = factor(IMD_Decile), y = estimate)) + # Make plot. Define what to plot.
  geom_errorbar(aes(x = factor(IMD_Decile), ymin = conf.low, ymax = conf.high), width = 0, position = position_dodge(width = 0.2)) + # Add error bars
  geom_point(size = 1.5, position = position_dodge(width = 0.2)) + # Plot points
  #ylim(0, max(pred_imd$value)) + # If want plot to start at 0
  labs(x = "Deprivation tenths", y = "Greenhouse gas emissions \n(kg CO2 equiv. total)") + # Label axes
  theme(text = element_text(size = 14)) # Adjust size of text
#fig1b # Print

# Land use
landuse <- read.csv("../laser output feb 26/05022026/05022026/imd_land_use_model.csv") # Load estimates of greenhouse gas emissions (total) model
fig1c <- ggplot(landuse, aes(x = factor(IMD_Decile), y = estimate)) + # Make plot. Define what to plot.
  geom_errorbar(aes(x = factor(IMD_Decile), ymin = conf.low, ymax = conf.high), width = 0, position = position_dodge(width = 0.2)) + # Add error bars
  geom_point(size = 1.5, position = position_dodge(width = 0.2)) + # Plot points
  #ylim(0, max(pred_imd$value)) + # If want plot to start at 0
  labs(x = "Deprivation tenths", y = "Land use (m2.year)") + # Label axes
  theme(text = element_text(size = 14)) # Adjust size of text
#fig1c # Print

# Water use
wateruse <- read.csv("../laser output feb 26/05022026/05022026/imd_water_use_model.csv") # Load estimates of greenhouse gas emissions (total) model
fig1d <- ggplot(wateruse, aes(x = factor(IMD_Decile), y = estimate)) + # Make plot. Define what to plot.
  geom_errorbar(aes(x = factor(IMD_Decile), ymin = conf.low, ymax = conf.high), width = 0, position = position_dodge(width = 0.2)) + # Add error bars
  geom_point(size = 1.5, position = position_dodge(width = 0.2)) + # Plot points
  #ylim(0, max(pred_imd$value)) + # If want plot to start at 0
  labs(x = "Deprivation tenths", y = "Water use (kL)") + # Label axes
  theme(text = element_text(size = 14)) # Adjust size of text
#fig1d # Print

# Combine into final plot
figure1 <- fig1a + fig1b + fig1c + fig1d + plot_annotation(tag_levels = 'A', caption = "HFSS = High in fat, salt and/or sugar. Deprivation tenths: 1 = most deprived 10% of areas, 10 = least deprived 10% of areas")
figure1

ggsave(plot = figure1, filename = "./Plots/figure1.jpeg", dpi = 300)


## Figure 2 ##

# GHGE
ghge_rate <- read.csv("../laser output feb 26/05022026/05022026/imd_ghg_rate_model.csv") # Load estimates of greenhouse gas emissions (rate) model
fig2a <- ggplot(ghge_rate, aes(x = factor(IMD_Decile), y = (estimate*1000))) + # Make plot. Define what to plot.
  geom_errorbar(aes(x = factor(IMD_Decile), ymin = (conf.low*1000), ymax = (conf.high*1000)), width = 0, position = position_dodge(width = 0.2)) + # Add error bars
  geom_point(size = 1.5, position = position_dodge(width = 0.2)) + # Plot points
  #ylim(0, max(pred_imd$value)) + # If want plot to start at 0
  labs(x = "Deprivation tenths", y = "Greenhouse gas emissions (kg CO2 equiv. total) \nper 1000 kg purchased") + # Label axes
  theme(text = element_text(size = 14)) # Adjust size of text
#fig2a # Print

# Land use
landuse_rate <- read.csv("../laser output feb 26/05022026/05022026/imd_land_rate_model.csv") # Load estimates of greenhouse gas emissions (rate) model
fig2b <- ggplot(landuse_rate, aes(x = factor(IMD_Decile), y = (estimate*1000))) + # Make plot. Define what to plot.
  geom_errorbar(aes(x = factor(IMD_Decile), ymin = (conf.low*1000), ymax = (conf.high*1000)), width = 0, position = position_dodge(width = 0.2)) + # Add error bars
  geom_point(size = 1.5, position = position_dodge(width = 0.2)) + # Plot points
  scale_y_continuous(breaks = seq(535, 555, by = 5)) + # Define number of x-axis tick points
  labs(x = "Deprivation tenths", y = "Land use (m2.year) \nper 1000 kg purchased") + # Label axes
  theme(text = element_text(size = 14)) # Adjust size of text
fig2b # Print

# Water use
wateruse_rate <- read.csv("../laser output feb 26/05022026/05022026/imd_water_rate_model.csv") # Load estimates of greenhouse gas emissions (rate) model
fig2c <- ggplot(wateruse_rate, aes(x = factor(IMD_Decile), y = (estimate*1000))) + # Make plot. Define what to plot.
  geom_errorbar(aes(x = factor(IMD_Decile), ymin = (conf.low*1000), ymax = (conf.high*1000)), width = 0, position = position_dodge(width = 0.2)) + # Add error bars
  geom_point(size = 1.5, position = position_dodge(width = 0.2)) + # Plot points
  #ylim(0, max(pred_imd$value)) + # If want plot to start at 0
  labs(x = "Deprivation tenths", y = "Water use (kL) \nper 1000 kg purchased") + # Label axes
  theme(text = element_text(size = 14)) # Adjust size of text
#fig2c # Print

# Combine into final plot
figure2 <- fig2a + fig2b / fig2c + plot_annotation(tag_levels = 'A', caption = "Deprivation tenths: 1 = most deprived 10% of areas, 10 = least deprived 10% of areas")
figure2

ggsave(plot = figure2, filename = "./Plots/figure2.jpeg", dpi = 300)



### Appendices ###

## Narrow HFSS plot ##

# HFSS broad definition
hfss_n <- read.csv("../laser output feb 26/05022026/05022026/imd_hfss_narrow_model.csv") # Load estimates for high in fat salt and/or sugar model (broad definition)
fig1a_n <- ggplot(hfss_n, aes(x = factor(IMD_Decile), y = (estimate*100))) + # Make plot. Define what to plot (model estimates converted to a percentage)
  geom_errorbar(aes(x = factor(IMD_Decile), ymin = (conf.low*100), ymax = (conf.high*100)), width = 0, position = position_dodge(width = 0.2)) + # Add error bars
  geom_point(size = 1.5, position = position_dodge(width = 0.2)) + # Plot points
  #ylim(0, max(pred_imd$value)) + # If want plot to start at 0
  labs(x = "Deprivation tenths", y = "HFSS purchases (%)") + # Label axes
  theme(text = element_text(size = 14)) # Adjust size of text
fig1a_n # Print
ggsave(plot = fig1a_n, filename = "../Paper 3/Plots/appendix_figure_1.jpeg", dpi = 300) # Save


## Food categories ##

# Greenhouse gas emissions #

# Load 
imd_ghge_agg <- fread("../laser output feb 26/05022026/05022026/imd_decile_lcfs_ghge.csv")

# Get data ready to plot
imd_ghge_agg_wide <- dcast(imd_ghge_agg, lcfs_cat ~ IMD_Decile, value.var = c("percent_all_emissions", "ghge_rate", "ghge_rate")) # Shift to wide format
imd_ghge_agg_wide$abs_difference <- imd_ghge_agg_wide$ghge_rate_1 - imd_ghge_agg_wide$ghge_rate_10 # Calculate absolute difference in ghge rate
imd_ghge_agg_wide$rel_difference <- ((imd_ghge_agg_wide$ghge_rate_1 / imd_ghge_agg_wide$ghge_rate_10) - 1 ) * 100 # Calculate relative difference in ghge rate
imd_ghge_agg_wide$abs_pc_dif <- (imd_ghge_agg_wide$percent_all_emissions_1 - imd_ghge_agg_wide$percent_all_emissions_10) # Calculate absolute difference in ghge %


# Plot
plot <- ggplot(imd_ghge_agg_wide, aes(x = rel_difference, y = lcfs_cat)) +
  geom_bar(stat = "identity") +
  scale_x_continuous(breaks = seq(-40,50,10)) +
  coord_cartesian(xlim = c(-40, 50), expand = FALSE) +
  xlab("Relative difference (%)") +
  ylab("Food category")
plot # Print
ggsave(plot = plot, filename = "../Paper 3/Plots/appendix_figure_2.jpeg", dpi = 300) # Save


# Land use #

# Load 
land_agg <- fread("../laser output feb 26/05022026/05022026/imd_decile_lcfs_landuse.csv")

# Get data ready to plot
land_agg_wide <- dcast(land_agg, lcfs_cat ~ IMD_Decile, value.var = c("percent_all_emissions", "land_rate")) # Shift to wide format
land_agg_wide$abs_difference <- land_agg_wide$land_rate_1 - land_agg_wide$land_rate_10 # Calculate absolute difference in ghge rate
land_agg_wide$rel_difference <- ((land_agg_wide$land_rate_1 / land_agg_wide$land_rate_10) - 1 ) * 100 # Calculate relative difference in ghge rate
land_agg_wide$abs_pc_dif <- (land_agg_wide$percent_all_emissions_1 - land_agg_wide$percent_all_emissions_10) # Calculate absolute difference in ghge %


# Plot
plot <- ggplot(land_agg_wide, aes(x = rel_difference, y = lcfs_cat)) +
  geom_bar(stat = "identity") +
  scale_x_continuous(breaks = seq(-40,50,10)) +
  coord_cartesian(xlim = c(-40, 50), expand = FALSE) +
  xlab("Relative difference (%)") +
  ylab("Food category")
plot # Print
ggsave(plot = plot, filename = "../Paper 3/Plots/appendix_figure_3.jpeg", dpi = 300) # Save


# Water use #

# Load 
water_agg <- fread("../laser output feb 26/05022026/05022026/imd_decile_lcfs_wateruse.csv")

# Get data ready to plot
water_agg_wide <- dcast(water_agg, lcfs_cat ~ IMD_Decile, value.var = c("percent_all_emissions", "water_rate")) # Shift to wide format
water_agg_wide$abs_difference <- water_agg_wide$water_rate_1 - water_agg_wide$water_rate_10 # Calculate absolute difference in ghge rate
water_agg_wide$rel_difference <- ((water_agg_wide$water_rate_1 / water_agg_wide$water_rate_10) - 1 ) * 100 # Calculate relative difference in ghge rate
water_agg_wide$abs_pc_dif <- (water_agg_wide$percent_all_emissions_1 - water_agg_wide$percent_all_emissions_10) # Calculate absolute difference in ghge %

water_agg_wide$rel_difference[water_agg_wide$lcfs_cat_recode == "Mineral or spring waters"] <- 0

# Plot
plot <- ggplot(water_agg_wide, aes(x = rel_difference, y = lcfs_cat)) +
  geom_bar(stat = "identity") +
  scale_x_continuous(breaks = seq(-40,50,10)) +
  coord_cartesian(xlim = c(-40, 50), expand = FALSE) +
  xlab("Relative difference (%)") +
  ylab("Food category")
plot # Print
ggsave(plot = plot, filename = "../Paper 3/Plots/appendix_figure_4.jpeg", dpi = 300) # Save



# Tidy
rm(list = ls()) # Delete all objects in memory
gc()


