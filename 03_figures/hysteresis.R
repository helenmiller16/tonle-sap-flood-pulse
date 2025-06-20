library(data.table)
library(lme4)
source(here::here("03_figures/themes.R"))
data_dir <- "00_data"
output_dir <- "03_outputs"


data <- fread(here::here(data_dir, "combined-data.csv"))

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

anova <- fread(here::here(output_dir, "anova_output.csv"))
anova <- anova[from == "low"]
anova <- anova[!duplicated(anova)]

effects_tbl <- fread(here::here(output_dir, "seasonal_change_estimates.csv"))

#p_vals <- fread(here::here(output_dir, "seasonal_change_p_values.csv"))
#names(p_vals)[1] <- "var"


get_anova_preds <- function(var) {
  anova_preds <- data.table(
    var = character(), 
    season = character(), 
    pred = numeric(), 
    lower = numeric(), 
    upper = numeric()
  )
  for (v in var) {
    a <- anova[y %in% v]
    # intercept is burnet, central, low. 
    # Show predictions for burnet, central for all seasons
    int = a[variable == "(Intercept)", estimate]
    int_lwr = a[variable == "(Intercept)", lower_2.5]
    int_upr = a[variable == "(Intercept)", upper_97.5]
    df <- data.table(
      var = v,
      season = c("low", "rising", "high", "falling"), 
      pred = c(int, 
               int + a[variable == "seasonrising", estimate], 
               int + a[variable == "seasonhigh", estimate], 
               int + a[variable == "seasonfalling", estimate])
    )
    
    df$lower <- c(a[variable == "(Intercept)", lower_2.5], 
                  int + a[variable == "seasonrising", lower_2.5],
                  int + a[variable == "seasonhigh", lower_2.5], 
                  int + a[variable == "seasonfalling", lower_2.5])
    
    df$upper <- c(a[variable == "(Intercept)", upper_97.5], 
                  int + a[variable == "seasonrising", upper_97.5],
                  int + a[variable == "seasonhigh", upper_97.5], 
                  int + a[variable == "seasonfalling", upper_97.5])
    df[, `:=`(pred = exp(pred), 
              lower = exp(lower), 
              upper = exp(upper))]
    anova_preds <- rbind(anova_preds, df)
  }
  anova_preds
}

get_anova_preds_sim <- function(variable) {
  delta <- 0.0001
  anova_preds <- data.table(
    var = character(), 
    season = character(), 
    pred = numeric(), 
    lower = numeric(), 
    upper = numeric()
  )
  
  for (v in variable) {
    
    
    d <- data[env == "pelagic", c(v, "season", "dataset", "region"), with = FALSE]
    d <- d[complete.cases(d)]
    model <- glm(formula(paste(v, "+delta ~ season + dataset + region")), 
                 family = Gamma(link="log"), 
                 data= d, 
                 na.action = na.omit)
    
    
    newdata <- expand.grid(
      season = c("low", "rising", "high", "falling"), 
      region = c("North"), 
      dataset = "lake"
    )
    
    p <- cbind(
      newdata, 
      predictions = predict(model, newdata, type = "response")
    )
    
    # confidence interval on predictions
    # this takes a while
    set.seed(42)
    boots <- 1000
    
    est <- matrix(NA, nrow(newdata), ncol = boots)
    print("begin")
    for (i in 1:boots) {
      ynew <- unlist(simulate(model))
      ymod <- update(model, ynew ~ .)
      est[, i] <- predict(ymod, newdata = newdata, type = "response")
    }
    
    # 95% CI
    q1 <- apply(est, 1, quantile, probs = 0.025)
    q2 <- apply(est, 1, quantile, probs = 0.975)
    
    df <- data.table(
      var = v,
      season = p$season, 
      pred = p$predictions
    )
    df$lower <- q1
    df$upper <- q2
    
    anova_preds <- rbind(anova_preds, df)
  }
  anova_preds
}

anova_preds <- get_anova_preds(unique(anova$y))



plot_seasonal <- function(variable, units = "μM", hline = c(), xlab = "") {
  d <- anova_preds[var == variable]
  d$x <- c(1,2,3,2)
  d <- rbind(d, d[1,])
  d[, color := season_colors[season]]
  
  e <- effects_tbl[y == variable]
  
  # points <- copy(data)
  # points[, x := ifelse(season == "low", 1, 
  #                    ifelse(season %in% c("rising", "falling"), 2, 
  #                           3))]
  # points[, color := season_colors[season]]
  
  
  arrow <- arrow(type = "closed", angle = 15, length = unit(0.15, "inches"))
  plot <- ggplot(d) 
  x_exp <- c(.1, .1)
  if (length(hline) == 1) {
    plot <- plot + geom_hline(data = data.frame(y = hline), 
                              aes(yintercept = y))
  }
  if (length(hline) == 2) {
    plot <- plot + geom_ribbon(data = data.frame(x = c(.8, 3.2), 
                                                 ymin = hline[1], 
                                                 ymax = hline[2]), 
                               aes(ymin = ymin, ymax = ymax, x = x), 
                               #fill = "#477f47", 
                               alpha = 0.2)
    x_exp <- c(0, 0)
  }
  plot + 
    # geom_violin(data = points,
    #            mapping = aes(x = x, y = .data[[variable]],
    #                          color = season))+
    # data_colors_dark +
    
    
    # error bars
    geom_errorbar(data = d[1:4, ], 
                  mapping = aes(x =x, 
                                ymin = lower, 
                                ymax = upper, 
                                color = season), 
                  width=0, 
                  linewidth = 1,
                  alpha = 0.5) +
    
    
    # high water path
    geom_path(data = d[c(2,3),],
              aes(x = x, y = pred),
              arrow = arrow,
              linewidth = ifelse(e[to == "high", p_adj] < 0.05, 2, .5),
              color = dark_colors[3]) +
    
    # Rising water
    geom_point(data = d[2, ], 
               aes(x = x, y = pred, color = "rising", fill = "rising"), 
               size = 4, 
               shape = 21) +
    geom_path(data = d[c(1,2),], 
              aes(x = x, y = pred), 
              arrow = arrow, 
              linewidth = ifelse(e[to == "rising", p_adj] < 0.05, 2, .5),
              color = dark_colors[2]) + 
    
    # low water
    geom_point(data = d[1, ], 
               aes(x = x, y = pred, color = "low", fill = "low"), 
               size = 4,  
               shape = 21) +
    geom_path(data = d[c(4,1),], aes(x = x, y = pred), arrow = arrow, 
              linewidth = ifelse(e[to == "low", p_adj] < 0.05, 2, .5),
              color = dark_colors[1]) + 
    
    
    # falling water
    geom_point(data = d[4, ], 
               aes(x = x, y = pred, color = "falling", fill = "falling"), 
               size = 4,  
               shape = 21) +
    geom_path(data = d[c(3,4),], aes(x = x, y = pred), arrow = arrow,
              linewidth = ifelse(e[to == "falling", p_adj] < 0.05, 2, .5),
              color = dark_colors[4]) +
    
    # high water
    geom_point(data = d[3, ], 
               aes(x = x, y = pred, color = "high", fill = "high"), 
               size = 4,  
               shape = 21,) +
    
    # only draw arrow head and not line
    geom_path(data = d[c(2,3),], aes(x = c(2.99, 3), 
                                     y = c(d[2,]$"pred" + .99*(d[3,]$"pred"-d[2,]$"pred"), 
                                           d[3,]$"pred")), 
              arrow = arrow,
              linewidth = ifelse(e[to == "high", p_adj] < 0.05, 2, .5),
              color = dark_colors[3]) +
    
    
    
    labs(y = paste0(variable, " (", units, ")"), x = xlab)+
    scale_x_continuous(breaks = c(1, 2, 3), 
                       labels = c("low", "", "high"), 
                       expand = x_exp) +
    tsl_theme +
    theme(panel.grid = element_blank()) + 
    scale_color_discrete(name = "Season", type = season_colors) + 
    scale_fill_discrete(name = "Season", type = season_colors)
}
# plot_seasonal("cond", units = "μS/cm") + 
#   # annotate("text", x = 1, 
#   #          y = 218.4, 
#   #          label = "Mean Mekong River conductivity", 
#   #          size = 2.5, 
#   #          hjust= .2) +
#   # geom_hline(aes(yintercept = 150))+
#   # annotate("text", x = 1, 
#   #          y = 155, 
#   #          label = "Mean Tonle Sap River conductivity", 
#   #          size = 2.5, 
#   #          hjust= .2) +
#   ylim(90, 270)
library(patchwork)
# add season labels to the first plot
p1 <- 
  plot_seasonal("DIP") +
  geom_text(aes(x = x, y = pred, label = season), 
            nudge_x = c(0.07, 0.07, 0, -.1, 0.07), 
            hjust = c(0, 0, .5, 1, 0),
            nudge_y = c(.1, .1, .2, 0, .1), 
            color = dark_colors[c(1:4, 1)], 
            size = 2.5
            )
p2 <- plot_seasonal("DIN")
p3 <- plot_seasonal("DIN.DIP", hline = c(20, 50)) + labs(y = "DIN/DIP") + 
  scale_y_continuous(breaks = c(30, 60, 90)) +
  geom_text(data = data.frame(x = c(0.8), 
                              y = c(50, 35, 20)), 
            aes(x = x, y = y, vjust = c(-.4, 0.2, 1.3)), 
            label = c("P-limited", "co-limited", "N-limited"), 
            #color = "#477f47", 
            alpha = 1,
            hjust = -.1, 
            size = 2.5) 

p_dissolved <- (p1 + p2 + p3) + plot_annotation(
  title = "Dissolved"
)

p4 <- plot_seasonal("TP")
p5 <- plot_seasonal("TN")
p6 <- plot_seasonal("TN.TP", hline = c(20, 50)) + labs(y = "TN/TP") + 
  geom_text(data = data.frame(x = c(0.8), 
                              y = c(50, 35, 20)), 
            aes(x = x, y = y, vjust = c(-.4, 0.2, 1.3)),, 
            label = c("P-limited", "co-limited", "N-limited"), 
            #color = "#477f47", 
            alpha = 1,
            hjust = -.1, 
            size = 2.5) 

p7 <- plot_seasonal("DSi")
# add line for approximate saturation
p8 <-  plot_seasonal("DO", units = "mg/L", xlab = "Water Level", hline = 7.75) +
  annotate("text", 
           x = 1.2, 
           y = 7.9,
           label = "Mean DO saturation", 
           size = 2.5, 
           hjust= .2)
p9 <- plot_seasonal("chla", units = "μg/L")

p_total <- (p4 + p5 +p6) + plot_annotation(
  title = "total"
)

plot_hysteresis <- p_dissolved / p_total / (p7 + p8 + p9) + plot_layout(guides = "collect") & 
  plot_annotation(tag_levels = "a", tag_suffix = ".") & theme(
                                                                plot.tag.location = "plot", 
                                                              plot.margin = rep(unit(.3, "cm"), 4))


png(here::here(output_dir, "hysteresis.png"), width = 9, height = 8, units = "in", res = 240)
print(plot_hysteresis)
dev.off()

if (FALSE) { # Don't run in automated pipeline
  # plot lake level vs concentration
  ggplot(data) + 
    geom_point(aes(x = season, 
                   y = chla, 
                   color = dataset, 
                   shape = dataset)) + 
    tsl_theme + 
    scale_y_log10() +
    scale_x_discrete(limits = c("low", "rising", "high", "falling"))
  
  
  mrc <- fread(here::here(data_dir, "MRC/mrc_water_quality_clean.csv"))
  mrc[, season := factor(season, levels = c("low", 
                                            "rising", 
                                            "high", 
                                            "falling"))]
  pk_cond <- mrc[dataset == "mrc-Prek Kdam", .(pred = mean(cond)*100), season]
  pk_cond <- pk_cond[order(season)]
  pk_cond <- rbind(pk_cond, pk_cond[1, ])
  pk_cond$x <- c(1, 2, 3, 2, 1)
  arrow <- arrow(type = "closed", angle = 15, length = unit(0.15, "inches"))
  
  ggplot() + 
    geom_path(data = pk_cond[c(2,3),],
              aes(x = x, y = pred),
              arrow = arrow,
              color = dark_colors[3]) +
    
    # Rising water
    geom_point(data = pk_cond[2, ], 
               aes(x = x, y = pred, color = season), 
               size = 4, 
               shape = 21,
               fill = light_colors[2], 
               color = dark_colors[2]) +
    geom_path(data = pk_cond[c(1,2),], 
              aes(x = x, y = pred), 
              arrow = arrow, 
              color = dark_colors[2]) + 
    
    # low water
    geom_point(data = pk_cond[1, ], 
               aes(x = x, y = pred, color = season), 
               size = 4,  
               shape = 21,
               fill = light_colors[1], 
               color = dark_colors[1]) +
    geom_path(data = pk_cond[c(4,1),], 
              aes(x = x, y = pred), 
              arrow = arrow, 
              color = dark_colors[1]) + 
    
    
    # falling water
    geom_point(data = pk_cond[4, ], 
               aes(x = x, y = pred, color = season), 
               size = 4,  
               shape = 21,
               fill = light_colors[4], 
               color = dark_colors[4]) +
    geom_path(data = pk_cond[c(3,4),], 
              aes(x = x, y = pred), 
              arrow = arrow,
              color = dark_colors[4]) +
    
    # high water
    geom_point(data = pk_cond[3, ], 
               aes(x = x, y = pred, color = season), 
               size = 4,  
               shape = 21,
               fill = light_colors[3], 
               color = dark_colors[3]) +
    
    # only draw arrow head and not line
    geom_path(data = pk_cond[c(2,3),], aes(x = c(2.99, 3), 
                                           y = c(pk_cond[2,]$"pred" + .99*(pk_cond[3,]$"pred"-pk_cond[2,]$"pred"), 
                                                 pk_cond[3,]$"pred")), 
              arrow = arrow,
              color = dark_colors[3]) +
    labs(y = paste0("Cond (uS/cm)"), x = "")+
    scale_x_continuous(breaks = c(1, 2, 3), 
                       labels = c("low", "", "high")) +
    tsl_theme +
    theme(panel.grid = element_blank()) + 
    scale_color_discrete(type = season_colors) +
    ylim(c(90, 270))
  
  
  
  # plot_seasonal("TP")
  # plot_seasonal("TN")
  # 
  # plot_seasonal("TN.TP")
  # plot_seasonal("chla")
  # plot_seasonal("DO")
  # plot_seasonal("cond")
  
  
}
