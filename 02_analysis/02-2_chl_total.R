# Setup ----- 
library(data.table)
library(ggplot2)
library(patchwork)
library(kableExtra)
library(lme4)
library(MuMIn)

data_dir <- "00_data"
figures_dir <- "03_figures"
output_dir <- "03_outputs"
data <- fread(here::here(data_dir, "combined-data.csv"))
source(here::here(figures_dir, "themes.R"))
data <- data[region != ""]
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

data_filtered <- data[env == "pelagic" & depth == "surface", .(
  TN, 
  TP, 
  chla, 
  DSi, 
  TN.TP, 
  region, 
  season, 
  dataset
)]
  
data_filtered <- data_filtered[complete.cases(data_filtered)]

names_dict <- c(
  "scale(TN)" = "TN", 
  "scale(TP)" = "TP", 
  "scale(DSi)" = "DSi", 
  "scale(TN.TP)" = "N/P", 
  "region" = "region", 
  "dataset" = "dataset", 
  "season" = "season", 
  "scale(TN):season" = "TN * season", 
  "scale(TP):season" = "TP * season",
  "scale(DSi):season" = "DSi * season"
  
)


# Model selection -----
model_full <- 
  glm(chla ~ 
          scale(TN) + 
          scale(TP) + 
          scale(TN.TP) +
          scale(DSi) +
          season:(scale(TN) + scale(TP) + scale(DSi)) +
          dataset + 
          region +
          season, 
        family = Gamma(link = "log"), 
        data = data_filtered, 
        na.action = na.fail)


# Fit every possible model and get AICc and weight 
# for each potential model
t <- dredge(model_full)

# Prep plot -----
# Plot by weighted power of each variable

# For each row, if notnull, assign value of weight
weighted <- ifelse(is.na(t), 0, 1) * t$weight
# Then sum up each column
weighted_counts <- apply(weighted, 2, sum)

# Make into a df for ggplot
weighted_counts <- data.frame(predictor = names_dict, 
                              weight = weighted_counts[names(names_dict)])

# dot plot: x is model number, y is parameter
present <- as.data.table(!is.na(t))[1:10, names(names_dict), with = FALSE]
present[, model := factor(rownames(present), levels = rownames(present))]
present[, delta := t$delta[1:10]]
present <- melt(present, c("model", "delta"))
present[, variable := names_dict[variable]]
#present <- present[value == TRUE]

# Best fit -----
model_best <- glm(chla ~ 
                    TP + 
                    season +
                    region +
                    TN + 
                    TP * season , 
                  family = Gamma(link = "log"), 
                  data = data)
summary(model_best)
confint(model_best)
# Pseudo R2 (percent deviance explained)
1- model_best$deviance/model_best$null.deviance
# R2 (percent variance explained)
1-sum( (model_best$y - model_best$fitted.values)^2 )/
  sum( (model_best$y - mean(model_best$y)) ^2)
## predictions -----
new_data <- expand.grid(
  TP = seq(min(data$TP, na.rm = TRUE), max(data$TP, na.rm = TRUE),length = 101),
  season = c("low", "rising", "high", "falling"), 
  region = c("North", "Central", "South"), 
  TN = mean(data$TN, na.rm = TRUE)
)
predictions <- predict(model_best, new_data, type = "response")
predictions <- cbind(new_data, predictions = predictions)
setDT(predictions)



# CI on predictions ----- 
set.seed(42)
boots <- 1000

est <- matrix(NA, nrow(new_data), ncol = boots)
for (i in 1:boots) {
  ynew <- unlist(simulate(model_best))
  ymod <- update(model_best, ynew ~ ., data = model_best$model)
  est[, i] <- predict(ymod, newdata = new_data, type = "response")
}
# 95% CI
predictions$q1 <- apply(est, 1, quantile, probs = 0.025)
predictions$q2 <- apply(est, 1, quantile, probs = 0.975)

# Write summary stats -----
model_fit <- data.table(covariate = names(coef(model_best)), 
                        coef = (coef(model_best)), 
                        (confint(model_best)))
model_fit <- rbind(model_fit, data.table(covariate = c('R2_dev', 'R2_var'), 
                                         coef = c(1- model_best$deviance/model_best$null.deviance, 
                                                  1 - sum( (model_best$y - model_best$fitted.values)^2 )/
                                                    sum( (model_best$y - mean(model_best$y))^2 ))
), fill = TRUE)

fwrite(predictions, here::here(output_dir, "chla_models/predictions_total.csv"), row.names = FALSE)
fwrite(model_fit, here::here(output_dir, "chla_models/model_fit_total.csv"), row.names = FALSE)
fwrite(present, here::here(output_dir, "chla_models/present_total.csv"), row.names = FALSE)
fwrite(weighted_counts, here::here(output_dir, "chla_models/variable_importance_total.csv"), row.names = FALSE)

