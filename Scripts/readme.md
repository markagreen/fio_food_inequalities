# R Scripts
The folder contains the following files:

1. clean_consumer_data.R - loads the retailer data in chunks (as too large to load into memory), cleans and processes the data to estimate outcome measures based on food purchases, and aggregates to household level data.
2. aggregate_food_categories_imd.R - similar to above but creates aggregated estimates of outcome varaibles for living and food costs survey food categories by deprivation tenth (weighted for sample characteristics).
3. aggregate_eatwell_categories.R - similar to above but produces estimates of outcomes by eatwell category for deprivation tenths (results not presented in paper)
4. clean_geodata.R - tidies all the geographical small area data to be used in the post-stratification step.
5. mlm.R - the primary analysis script used to replicate all of the results presented in the main body of the paper. Loads the processed consumer data, cleans them into analysis ready format, generates descriptive statistics, runs the multi-level regression model and produces post-stratified estimates using census data.
6. inequalities_foods.R - generates descriptive data for the plots in the appendices on differences in food category dynamics between most and least deprived areas.
7. census_table1.R - generates the census statistics used in Table 1 of the paper.
8. plots_for_paper_current.R - produces all of the figures presented in the main body of the paper and appendices.
