analysis_dir <- here::here("02_analysis")

# ANOVA: run ANOVA and write out table of results
#        run for each variable, once with each season as the intercept
# 
# Outputs: 
# anova_output.csv -- table with parameter estimates, sterr, etc for each run
# seasonal_change_estimates.csv -- table with estimated change for each variable
# seasonal_change_p_values.csv -- table with adjusted p value of estimated change for each variable
# seasonal_change_summary.csv -- table with estimated change only for p < 0.05
# anova_pred_table.csv -- table with predicted means for each season
print(paste("-----", "01-1_nutrient_seasonal_variability.R", "-----"))
source(file.path(analysis_dir, "01-1_nutrient_seasonal_variability.R"))



# Dilution correlations: GLM testing for correlations between nutrients and conductivity
#
# Outputs: 
# dilution_ci_table.csv -- table with confidence intervals on parameters for each model
print(paste("-----", "01-2_conductivity_correlations.R", "-----"))
source(file.path(analysis_dir, "01-2_conductivity_correlations.R"))

# Spatial comparisons
# 
# Outputs: 
# "03_outputs/season_region_interaction.png"
# "03_outputs/season_region_hysteresis.png"
# "03_outputs/region_preds.csv"
# "03_outputs/region_estimates.png"
print(paste("-----", "01-3_nutrient_spatial_variability.R", "-----"))
source(file.path(analysis_dir, "01-3_nutrient_spatial_variability.R"))


# Fit models relating nutrients to chl-a in pelagic environment
# 
# Outputs: 
# chl_open_top_10_models_total.png -- variable importance figure for total
# chl_predictions_open_total.png -- predictions for model with top predictors
# chl_open_top_10_models_dissolved.png -- variable importance figure for dissolved
# chl_predictions_open_dissolved.png -- predictions for model with top predictors
print(paste("-----", "02-1_chl_dissolved.R", "-----"))
source(file.path(analysis_dir, "02-1_chl_dissolved.R"))
print(paste("-----", "02-2_chl_total.R", "-----"))
source(file.path(analysis_dir, "02-2_chl_total.R"))

# Get figure of nutrient effects by season
# 
# Output: 
# nutrient_interaction_effects.png -- figure of nutrient interaction effects by season
# print(paste("-----", "02-3_interaction_preds.R", "-----"))
# source(file.path(analysis_dir, "02-3_interaction_preds.R"))

# Fit model relating nutrient to chl-a in floodplain
# 
# Outputs: 
# nutrient_effects_tbl.csv -- table of effect sizes for nutrients
# nutrient_pval_tbl.csv -- table of p-values for nutrients
# chl_fpl_top_10_models.png -- variable importance figure for chla
# print(paste("-----", "03-1_chl_floodplain_selection.R", "-----"))
# source(file.path(analysis_dir, "03-1_chl_floodplain_selection.R"))
# print(paste("-----", "03-2_nutrient_floodplain_effects.R", "-----"))
# source(file.path(analysis_dir, "03-2_nutrient_floodplain_effects.R"))





