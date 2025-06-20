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

data_filtered <- data[env == "pelagic", .(
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


# Fit interaction terms


data_d <- data[complete.cases(data[, .(chla, DIP, DIN, season, dataset, region)]), 
               .(chla, DIP, DIN, season, dataset, region)]
data_t <- data[complete.cases(data[, .(chla, TP, TN, season, dataset, region)]), 
               .(chla, TP, TN, season, dataset, region)]
model_total <- glm(chla ~ 
                     TP:season + 
                     TN:season + 
                     dataset +
                     region , 
                   family = Gamma(link = "log"), 
                   data = data_t)
model_dissolved <- glm(chla ~ 
                         DIP:season + 
                         DIN:season +
                         dataset + 
                         region , 
                       family = Gamma(link = "log"), 
                       data = data_d)

newdata_t <- expand.grid(
  TP = seq(min(data$TP, na.rm = TRUE), max(data$TP, na.rm = TRUE),length = 101),
  TN = seq(min(data$TN, na.rm = TRUE), max(data$TN, na.rm = TRUE), length = 101),
  season = c("low", "rising", "high", "falling"), 
  dataset = c("burnett"), 
  region = c("North")
)
newdata_d <- expand.grid(
  DIP = seq(min(data$DIP, na.rm = TRUE), max(data$DIP, na.rm = TRUE),length = 101),
  DIN = seq(min(data$DIN, na.rm = TRUE), max(data$DIN, na.rm = TRUE), length = 101),
  season = c("low", "rising", "high", "falling"), 
  dataset = c("burnett"),
  region = c("North")
)

preds_t <- cbind(
  newdata_t, 
  predictions = predict(model_total, newdata_t, type = "response")
)
preds_t$type <- "total"
setDT(preds_t)
setnames(preds_t, c("TN", "TP"), c("N", "P"))


preds_d <- cbind(
  newdata_d, 
  predictions = predict(model_dissolved, newdata_d, type = "response")
)
preds_d$type <- "dissolved"
setDT(preds_d)
setnames(preds_d, c("DIN", "DIP"), c("N", "P"))
preds <- rbind(preds_t, preds_d)


# confidence interval on predictions
# this takes a while
set.seed(42)
boots <- 1000

est_d <- matrix(NA, nrow(newdata_d), ncol = boots)
for (i in 1:boots) {
  ynew <- unlist(simulate(model_dissolved))
  ymod <- update(model_dissolved, ynew ~ .)
  est_d[, i] <- predict(ymod, newdata = newdata_d, type = "response")
}

# 95% CI
q1_d <- apply(est_d, 1, quantile, probs = 0.025)
q2_d <- apply(est_d, 1, quantile, probs = 0.975)

# total
est_t <- matrix(NA, nrow(newdata_t), ncol = boots)
for (i in 1:boots) {
  ynew <- unlist(simulate(model_total))
  ymod <- update(model_total, ynew ~ .)
  est_t[, i] <- predict(ymod, newdata = newdata_t, type = "response")
}

# 95% CI
q1_t <- apply(est_t, 1, quantile, probs = 0.025)
q2_t <- apply(est_t, 1, quantile, probs = 0.975)

preds$q1 <- c(q1_t, q1_d)
preds$q2 <- c(q2_t, q2_d)


# Make plots

mid_tn <- median(seq(min(data$TN, na.rm = TRUE), max(data$TN, na.rm = TRUE), length = 101))
mid_tp <- median(seq(min(data$TP, na.rm = TRUE), max(data$TP, na.rm = TRUE), length = 101))
mid_din <- median(seq(min(data$DIN, na.rm = TRUE), max(data$DIN, na.rm = TRUE), length = 101))
mid_dip <- median(seq(min(data$DIP, na.rm = TRUE), max(data$DIP, na.rm = TRUE), length = 101))

pred_to_plot <- preds[N %in% c(mid_tn, mid_din)]
pred_to_plot[, var := ifelse(type == "total", "TP", "DIP")]
pred_to_plot[, x := P]


p <- preds[P %in% c(mid_tp, mid_dip)]
p[, var := ifelse(type == "total", "TN", "DIN")]
p[, x := N]

pred_to_plot <- rbind(pred_to_plot, p)
# also include slope and p-values
coef_t <- summary(model_total)$coefficients
coef_d <- summary(model_dissolved)$coefficients
pval_t <- coef_t[, 'Pr(>|t|)']
pval_d <- coef_d[, 'Pr(>|t|)']
est_t <- coef_t[, 'Estimate']
est_d <- coef_d[, 'Estimate']
est_df <- data.frame(
  p_value = c(pval_t, pval_d), 
  estimate = c(est_t, est_d), 
  var = names(c(pval_t, pval_d)),
  type = rep(c("total", "dissolved"), each = 12))
est_df <- est_df[grepl("season", est_df$var), ]
est_df$season <- gsub("(TP)|(TN)|(DIN)|(DIP)|(:)|season", "", est_df$var)
est_df$var <- gsub("(season)|:|(low)|(high)|(falling)|(rising)", "", est_df$var)
est_df$season <- factor(est_df$season, levels = levels(preds$season))



plot_var <- function(v, 
                     include_facet_labels = FALSE, 
                     include_y_lab = FALSE, 
                     pos_x = 0.05, 
                     pos_y = 0.95) {
  
  plot_data <- pred_to_plot[var == v]
  plot_ann <- est_df[est_df$var == v,]
  plot_ann$x <- min(plot_data$x) + pos_x * diff(range(plot_data$x))
  plot_ann$y <- min(plot_data$q1) + pos_y * diff(range(c(plot_data$q1, plot_data$q2)))
  plot_ann$text <- paste0("log(Chl-a) = ", round(plot_ann$estimate, 3), "*", plot_ann$var, " + ...\np=", round(plot_ann$p_value, 3))
  
  ylab <- ifelse(include_y_lab, "chl-a", "")
  p <- ggplot(plot_data) + 
    geom_ribbon(aes(x = x, ymin = q1, ymax = q2), 
                alpha = 0.2) + 
    geom_line(aes(x = x, y = predictions)) + 
    facet_grid(cols = vars(season)) +
    labs(x = "", y = v)+
    scale_y_log10(expand = c(0.1, 0.1)) +
    tsl_theme +
    theme(panel.grid = element_blank(), 
          axis.text.x = element_blank(),
          axis.text.y = element_blank())
  
  if (!include_facet_labels) {
    p <- p + theme(strip.text.x = element_blank())
  }
  
  # add estimate and p-value
  
  
  p + geom_text(data = plot_ann, 
                mapping = aes(x, y, label = text, hjust = 0, vjust = 1), cex = 2)
}

library(patchwork)
library(grid)
plot <- (plot_var("DIN", TRUE) + ggtitle("Effect of nutrients on Chl-a by season")) / plot_var("TN") / plot_var("DIP") / plot_var("TP") 


# Create a text grob to act as a y-axis label
y_label <- wrap_elements(textGrob("Chl-a as a function of...", rot = 90, gp = gpar(fontsize = 12)))

# Combine the label and the plots using patchwork
combined_plot <- y_label + (plot)+ 
  plot_layout(widths = c(1, 10))  # Adjust the width for the label and plots
png(here::here(output_dir, "nutrient_interaction_effects.png"), width = 6, height = 6, units = "in", res = 240)
print(combined_plot)
dev.off()
