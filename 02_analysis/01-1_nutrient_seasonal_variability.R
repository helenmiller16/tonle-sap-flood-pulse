library(data.table)
library(ggplot2)
library(patchwork)
library(lme4)
data_dir <- "00_data"
output_dir <- "03_outputs"
source(here::here("03_figures/themes.R"))

# setup -----

data <- fread(here::here(data_dir, "combined-data.csv"))
data[region == "", region := NA]
# set levels to control intercept

data[, dataset := factor(dataset, levels = c("burnett", 
                                             "holtgrieve", 
                                             "miller", 
                                             "heu", 
                                             "loken", 
                                             "yoshikawa"))]
data[, region := factor(region, levels = c("North",
                                           "Central",
                                           "South"))]
data[, season := factor(season, levels = c("low", 
                                           "rising", 
                                           "high", 
                                           "falling"))]
data <- data[env == "pelagic" & depth == "surface"]

ctl <- glm.control(epsilon = 1e-4)
delta <- 0.0001 # delta to add for 0s in response



# fit models -----
# variables to fit
Y <- c("DIP", 
       "DIN", 
       "TP", 
       "TN", 
       "NO3", 
       "DSi", 
       "DIN.DIP", 
       "TN.TP", 
       "chla", 
       "DO", 
       "cond")
df <- data.frame(
  y = character(), 
  variable = character(), 
  estimate = numeric(), 
  sterr = numeric(), 
  t = numeric(), 
  p = numeric(), 
  lower_2.5 = numeric(), 
  upper_97.5 = numeric()
)

aov_df <- data.frame(
  y = character(), 
  x = character(),
  f_df_1 = numeric(), 
  f_df_2 = numeric(),
  F_value = numeric(), 
  p_value = numeric()
)

coef_dict <- c(
  "datasetholtgrieve" = "Holtgrieve", 
  "datasetmiller" = "Miller", 
  "datasetyoshikawa" = "Yoshikawa", 
  "datasetloken" = "Loken", 
  "datasetheu" = "Heu", 
  "regionCentral" = "Central", 
  "regionSouth" = "South"
)
coef_df <- data.frame(coef = coef_dict)
for (y in Y) {
  
  # Get anova result (is there any difference overall?)
  formula_null <- formula(paste(y, "+delta ~ dataset + region"))
  formula_dataset <- formula(paste(y, "+delta ~ season + region"))
  formula_region <- formula(paste(y, "+delta ~ season + dataset"))
  formula <- formula(paste(y, "+delta ~ season + dataset + region"))
  
  null_model <- glm(formula_null,  
                    family = Gamma(link="log"), 
                    data= data, 
                    na.action = na.omit)
  season_model <- glm(formula,  
                      family = Gamma(link="log"), 
                      data= data, 
                      na.action = na.omit)
  dataset_model <- glm(formula_dataset, 
                       family = Gamma(link="log"), 
                       data= data, 
                       na.action = na.omit)
  
  region_model <- glm(formula_dataset, 
                       family = Gamma(link="log"), 
                       data= data, 
                       na.action = na.omit)
  
  anova_result <- anova(null_model, season_model)
  anova_dataset <- anova(dataset_model, season_model)
  anova_region <- anova(region_model, season_model)
  anova_p <- anova_result$`Pr(>F)`[2]
  anova_f <- anova_result$F[2]
  f_df_1 <- anova_result$`Resid. Df`[2]
  f_df_2 <- anova_result$Df[2]
  aov_df <- rbind(aov_df, 
                  data.frame(
                    y = y, 
                    x = "season", 
                    f_df_1 = f_df_1, 
                    f_df_2 = f_df_2,
                    f_value = anova_f, 
                    p_value = anova_p
                  ))
  aov_df <- rbind(aov_df, 
                  data.frame(
                    y = y, 
                    x = "dataset", 
                    f_df_1 = anova_dataset$`Resid. Df`[2],
                    f_df_2 = anova_dataset$Df[2],
                    f_value = anova_dataset$F[2],
                    p_value = anova_dataset$`Pr(>F)`[2]
                  ))
  aov_df <- rbind(aov_df, 
                  data.frame(
                    y = y, 
                    x = "region", 
                    f_df_1 = anova_region$`Resid. Df`[2],
                    f_df_2 = anova_region$Df[2],
                    f_value = anova_region$F[2],
                    p_value = anova_region$`Pr(>F)`[2]
                  ))
  
  
  # Get and save effect size for region and dataset
  
  all_coef <- coef(season_model)
  all_coef_ci <- confint(season_model)
  coef_df[[paste0(y, "_est")]] <- exp(all_coef)[rownames(coef_df)]
  coef_df[[paste0(y, "_lwr")]] <- exp(all_coef_ci)[, 1][rownames(coef_df)]
  coef_df[[paste0(y, "_upr")]] <- exp(all_coef_ci)[, 2][rownames(coef_df)]
  
  # set factor levels to control what we compare to
  d_copy <- copy(data)
  seasons <- levels(data$season)
  for (s in seasons) {
    # set factor level
    d_copy[, season := factor(season, 
                              levels = c(s, seasons[seasons != s]))]
    
    formula <- formula(paste(y, "+delta ~ season + dataset + region"))
    model <- glm(formula,  
                 family = Gamma(link="log"), 
                 data= d_copy, 
                 na.action = na.omit)
    
    coef <- coef(summary(model))
    confint <- confint(model)[rownames(coef), ]
    
    d <- data.frame(
      y = y,
      from = s,
      variable = rownames(coef), 
      estimate = coef[,'Estimate'], 
      sterr = coef[,'Std. Error'], 
      t = coef[,'t value'], 
      p = coef[,'Pr(>|t|)'], 
      lower_2.5 = confint[,'2.5 %'],
      upper_97.5 = confint[, '97.5 %']
    )
    df <- rbind(df, d)
  }
  
}
fwrite(df, here::here(output_dir, "anova_output.csv"))
fwrite(coef_df, here::here(output_dir, "dataset_season_estimates.csv"))
fwrite(aov_df, here::here(output_dir, "predictor_anova.csv"))
# make table of estimates and p-values for 
# difference into that season
setDT(df)
table <- df[grepl("season", variable)]
table[, variable := sub("season", "", variable)]
setnames(table, "variable", "to")
# only include season changing
table <- table[
  (from == "low" & to == "rising") |
    (from == "rising" & to == "high") |
    (from == "high" & to == "falling") |
    (from == "falling" & to == "low")
]
table[, p_adj := p.adjust(p, method = "bonferroni")]
fwrite(table, here::here(output_dir, "seasonal_change_estimates.csv"))

# 
# estimates <- dcast(table, y ~ to, value.var = "estimate") |> 
#   as.data.table()
# y <- estimates$y
# estimates$y <- NULL
# estimates <- as.matrix(estimates)
# rownames(estimates) <- y
# write.csv(estimates, here::here(output_dir, "seasonal_change_estimates.csv"))
# 
# p_values <- dcast(table, y ~ to, value.var = "p_adj")
# y <- p_values$y
# p_values$y <- NULL
# p_values <- as.matrix(p_values)
# rownames(p_values) <- y
# write.csv(p_values, here::here(output_dir, "seasonal_change_p_values.csv"))
# 
# 
# 
