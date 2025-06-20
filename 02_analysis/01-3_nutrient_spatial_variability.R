# in this script, I:
# 1. compare concentrations in the Mekong (from MRC) to lake concentrations
# 2. compare concentrations across lake regions by season

library(data.table)
library(ggplot2)
library(patchwork)
library(lme4)
library(sf)
data_dir <- "00_data"
output_dir <- "03_outputs"
source(here::here("03_figures/themes.R"))

# setup -----


data <- fread(here::here(data_dir, "combined-data.csv"))
data_mrc <- fread(here::here(data_dir, "MRC/mrc_water_quality_clean.csv"))
data_mrc <- data_mrc[grepl("Backprea", dataset) |
                       grepl("Kompong Luong", dataset) | 
                       grepl("Prek Kdam", dataset) | 
                       grepl("Chhnang", dataset)]
data_mrc[, region := ifelse(env == "river_tonlesap", "TSR", ifelse(
  env == "river_tributary", "Tributary", ifelse(
    env == "river_mekong", "Mekong", region
  )
))]
data_mrc[, dataset := "mrc"]
data <- rbind(data[, names(data_mrc), with = FALSE], data_mrc[date > min(data$date)])
# look at MRC data at backprea (floodplain/tributary), 
# Kompong Luong (lake), Prek Kdam (TSR), and Kr. Ghhnang (Mekong upstream)


# look at TN, TP, NO3, conductivity



data[region == "", region := NA]
# set levels to control intercept

data[, dataset := factor(dataset, levels = c("burnett", 
                                             "holtgrieve", 
                                             "miller", 
                                             "heu", 
                                             "loken", 
                                             "yoshikawa", 
                                             "mrc"))]
data[, region := factor(region, levels = c("Tributary", 
                                           "North",
                                           "Central",
                                           "South", 
                                           "TSR", 
                                           "Mekong"))]
data[, season := factor(season, levels = c("low", 
                                           "rising", 
                                           "high", 
                                           "falling"))]
data <- data[!env %in% c("edge", "floodplain") & depth == "surface"]

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
       "cond")

pred_df <- data.table(
  season = character(), 
  region = character(), 
  dataset = character(), 
  y = character(), 
  pred = numeric(), 
  lwr = numeric(), 
  upr = numeric()
)
aov_df <- data.frame(
  y = character(), 
  F_value = numeric(), 
  p_value = numeric()
)


for (y in Y) {
  
  formula <- formula(paste(y, "+delta ~ dataset + region + season"))
  formula_interaction <- formula(paste(y, "+delta ~ dataset + region + season + season:region"))
  
  null_model <- glm(formula,  
                      control = ctl,
                    family = Gamma(link="log"), 
                    data= data, 
                    na.action = na.omit)
  interaction_model <- glm(formula_interaction,  
                           family = Gamma(link="log"), 
                           data= data, 
                           na.action = na.omit)
  anova_result <- anova( null_model, interaction_model)
  anova_p <- anova_result$`Pr(>F)`[2]
  anova_f <- anova_result$F[2]
  aov_df <- rbind(aov_df, 
                  data.frame(
                    y = y, 
                    f_value = anova_f, 
                    p_value = anova_p
                  ))
  
  
  
  
  # Model containing both season and region
  
  # show predictions for burnett for each region and season
  df <- expand.grid(season = c("low", "rising", "high", "falling"), 
                    region = unique(interaction_model$model$region), 
                    dataset = "burnett")
  dt <- as.data.table(df)
  dt$y <- y
  dt$pred <- numeric()
  dt$lwr <- numeric()
  dt$upr <- numeric()
  
  preds <- predict(interaction_model, newdata = df, 
                   type = "link", se.fit = TRUE)
  
  # wald confidence intervals based on standard error. 
  
  dt$pred <- preds$fit
  dt$pred[attr(preds$fit, "non-estim")] <- NA
  dt$lwr <- dt$pred - 1.96*preds$se.fit
  dt$upr <- dt$pred + 1.96*preds$se.fit
  
  # transform preds to response-scale
  dt[, `:=`(pred = exp(pred), 
            lwr = exp(lwr), 
            upr = exp(upr))]
  pred_df <- rbind(pred_df, dt)
}

aov_df$adj_p_value <- p.adjust(aov_df$p_value, method = "bonferroni")

# I'm just gonna make the figure here to make it simpler
.p <- function(p) {
  p_ <- "_p_"
  if (p > 0.5) {
    p_str <- " > .5"
  } else if (p > 0.01) {
    p_str <- paste0(" = ", sub("^0+", "", round(p, 2)))
  } else if (p > 0.001) {
    p_str <- paste0(" = ", sub("^0+", "", round(p, 3)))
  } else {
    p_str <- " < .001"
  }
  bquote(~italic("p")~.(p_str))
}

pred_df$region = factor(pred_df$region, levels = levels(data$region))

plot_season_region <- function(var, title = var) {
  ggplot(pred_df[y == var]) +
    geom_point(aes(x = season, y = region, size = pred)) + 
    scale_y_discrete(limits = c("Mekong", "TSR", "South", "Central", "North", "Tributary")) +
    tsl_theme + 
    ggtitle(title, subtitle = bquote("Season-region interaction:"~.(.p(aov_df[aov_df$y == var, "adj_p_value"])))) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(angle = 45, vjust = 1, hjust=1), 
          axis.title.x = element_blank(), 
          axis.title.y = element_blank(), 
          legend.position = "none") 
}
png(here::here("03_outputs/season_region_interaction.png"), 
    width = 8, height = 8, units = "in", res = 240)
print((plot_season_region("DIP")  + plot_season_region("DIN") ) /
  (plot_season_region("TP") + plot_season_region("TN")  ) /
  (plot_season_region("DSi") + 
     plot_season_region("cond", title = "Conductivity")) +
    plot_annotation(tag_levels = "a", tag_suffix = ".") & theme(
      plot.tag.location = "plot", 
      plot.margin = rep(unit(.3, "cm"), 4)))
dev.off()


hysteresis_season_region <- function(var, 
                                     title = var, 
                                     units = "μM", 
                                     legend = FALSE) {
  d <- pred_df[y == var]
  d$region = factor(d$region, levels = levels(data$region))
  arrow <- arrow(type = "closed", angle = 15, length = unit(0.15, "inches"))
  plot <- ggplot(d) 
  
  # only include regions with data and make sure they're in the correct order
  for (r in levels(d$region)[levels(d$region) %in% d$region]) {
    d_ <- d[region == r]
    d_$x <- c(1,2,3,2)
    d_ <- rbind(d_, d_[1,])
    plot = plot + #geom_errorbar(data = d_[1:4, ],
                                # mapping = aes(x =x,
                                #               ymin = lwr,
                                #               ymax = upr,
                                #               color = region),
                                # width=0,
                                # linewidth = 1,
                                # alpha = 0.3) +
      
      
      # high water path
      geom_path(data = d_[c(2,3),],
                aes(x = x, y = pred, color = region),
                arrow = arrow
                #linewidth = ifelse(e[to == "high", p_adj] < 0.05, 2, .5),
                ) +
      
      # Rising water
      geom_point(data = d_[2, ], 
                 aes(x = x, y = pred, color = region, fill = region), 
                 size = 4, 
                 shape = 21, 
                 ) +
      geom_path(data = d_[c(1,2),], 
                aes(x = x, y = pred, color = region), 
                arrow = arrow
                #linewidth = ifelse(e[to == "rising", p_adj] < 0.05, 2, .5),
                ) + 
      
      # low water
      geom_point(data = d_[1, ], 
                 aes(x = x, y = pred, color = region, fill = region), 
                 size = 4,  
                 shape = 21) +
      geom_path(data = d_[c(4,1),], aes(x = x, y = pred, color = region), arrow = arrow 
                #linewidth = ifelse(e[to == "low", p_adj] < 0.05, 2, .5),
                #color = dark_colors[1]
                ) + 
      
      
      # falling water
      geom_point(data = d_[4, ], 
                 aes(x = x, y = pred, color = region, fill = region), 
                 size = 4,  
                 shape = 21) +
      geom_path(data = d_[c(3,4),],aes(x = x, y = pred, color = region), arrow = arrow ) +
      
      # high water
      geom_point(data = d_[3, ], 
                 aes(x = x, y = pred, color = region, fill = region), 
                 size = 4,  
                 shape = 21) +
      
      # only draw arrow head and not line
      geom_path(data = d_[c(2,3),], aes(x = c(2.99, 3), 
                                       y = c(d_[2,]$"pred" + .99*(d_[3,]$"pred"-d_[2,]$"pred"), 
                                             d_[3,]$"pred"), 
                                       color = region), 
                arrow = arrow) 
  }
  
  # points <- copy(data)
  # points[, x := ifelse(season == "low", 1, 
  #                    ifelse(season %in% c("rising", "falling"), 2, 
  #                           3))]
  # points[, color := season_colors[season]]
  
    plot = plot + 
    labs(y = paste0(var, " (", units, ")"), x = "water level")+
    scale_x_continuous(breaks = c(1, 2, 3), 
                       labels = c("low", "", "high")) +
    tsl_theme +
    theme(panel.grid = element_blank()) + 
    scale_color_discrete(name = "Region", type = region_colors[names(region_colors) %in% d$region]) + 
    scale_fill_discrete(name = "Region", type = region_colors[names(region_colors) %in% d$region])
    
    if (!legend) {
      plot = plot + theme(legend.position = "none")
    }
    
    plot
}

png(here::here("03_outputs/season_region_hysteresis.png"), 
    width = 8, height = 8, units = "in", res = 240)
print((hysteresis_season_region("DIP")  + hysteresis_season_region("DIN") ) /
        (hysteresis_season_region("TP") + hysteresis_season_region("TN", legend = TRUE)  ) /
        (hysteresis_season_region("DSi") + 
           hysteresis_season_region("cond", title = "Conductivity", units= "μS/cm")) +
        plot_annotation(tag_levels = "a", tag_suffix = ".") & theme(
          plot.tag.location = "plot", 
          plot.margin = rep(unit(.3, "cm"), 4)))
dev.off()


## Now compare MRC data
# re-load data to include river 
data <- fread(here::here(data_dir, "combined-data.csv"))
data_mrc <- fread(here::here(data_dir, "MRC/mrc_water_quality_clean.csv"))
data_mrc <- data_mrc[grepl("Backprea", dataset) |
                       grepl("Kompong Luong", dataset) | 
                       grepl("Prek Kdam", dataset) | 
                       grepl("Chhnang", dataset)]

data <- rbind(data[, names(data_mrc), with = FALSE], data_mrc)
# look at MRC data at backprea (floodplain/tributary), 
# Kompong Luong (lake), Prek Kdam (TSR), and Kr. Ghhnang (Mekong upstream)


# look at TN, TP, NO3, conductivity



# compare Kompong Luoung measurements to our measurements
# they are not very close. 

data_sf <- st_as_sf(data[!is.na(lon)], coords = c("lon", "lat"), crs ="EPSG:4326")
kl <- data_sf[grepl("Kompong Luong", data_sf$dataset), ]
data_sf <- data_sf[!grepl("mrc", data_sf$dataset), ]
data_sf$dist_from_kl <- st_distance(data_sf, kl[1, ])[, 1]

closest <- as.data.table(data_sf[order(data_sf$dist_from_kl)[1:5],])
setDT(kl)
kl_closest <- kl[unlist(lapply(closest$date, \(d) which.min(abs(kl$date - d))))]

# Now compare different MRC locations
data_mrc[, water_year := # water year start on julian day 121
  as.numeric(strftime(date, "%Y")) + 
  (as.numeric(strftime(date, "%j")) > 121)]
by_season <- data_mrc[water_year > 2013, .(TP = mean(TP, na.rm = TRUE), TN = mean(TN, na.rm = TRUE), 
                              NO3 = mean(NO3, na.rm = TRUE), cond = mean(cond, na.rm = TRUE)), 
         by = .(env, season)]


plot_season_mrc <- function(var, title = var) {
  ggplot(by_season) +
    geom_point(aes(x = season, y = by_season[[var]], group = env, color = env)) + 
    geom_line(aes(x = season, y = by_season[[var]], group = env, color = env)) + 
    #scale_y_discrete(limits = c("river_mekong", "river_tonlesap", "Pelagic", "river_tributary")) +
    scale_x_discrete(limits = c("low", "rising", "high", "falling")) +
    tsl_theme + 
    ggtitle(title) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(angle = 45, vjust = 1, hjust=1), 
          axis.title.x = element_blank(), 
          axis.title.y = element_blank(), 
          #legend.position = "none") 
    )
}

# (plot_season_mrc("TP") + plot_season_mrc("cond")) / 
#   (plot_season_mrc("TN") + plot_season_mrc("NO3"))
# 
# 
# ggplot(data_mrc[water_year >= 2005 & water_year < 2010]) + 
#   geom_point(aes(x= date, y = NO3, group = env, color= env)) +
#   geom_line(aes(x= date, y = NO3, group = env, color= env)) +
#   geom_line(aes(x = date, y = water_level_m)) 
# 
# ggplot(data_mrc[water_year >= 2013 & water_year < 2022 ]) + 
#   geom_point(aes(x= date, y = TN, group = env, color= env)) +
#   geom_line(aes(x= date, y = TN, group = env, color= env)) +
#   geom_line(aes(x = date, y = water_level_m*10))
# 
# ggplot(data_mrc[water_year >= 2013 & water_year < 2022 ]) + 
#   geom_point(aes(x= date, y = TP, group = env, color= env)) +
#   geom_line(aes(x= date, y = TP, group = env, color= env)) +
#   geom_line(aes(x = date, y = water_level_m*2))
# 
# ggplot(data_mrc[water_year >= 2013 & water_year < 2022 ]) + 
#   geom_point(aes(x= date, y = cond, group = env, color= env)) +
#   geom_line(aes(x= date, y = cond, group = env, color= env)) +
#   geom_line(aes(x = date, y = water_level_m*20))



## Compare spatial predictions (no season effect)

effects <- fread(here::here("03_outputs/anova_output.csv"))[from == "low"]
pred_df <- data.table(y = character(), 
                      region = factor(character(), levels = c("North", "Central", "South")), 
                      value = numeric(), 
                      lwr = numeric(), 
                      upr = numeric())
for (var in unique(effects$y)) {
  d = effects[y == var]
  season_mean <- mean(d[grepl("season", variable)]$estimate)
  intercept <- d[variable == "(Intercept)"]$estimate + season_mean
  pred_df <- rbind(pred_df, data.frame(y = var, 
                                       region = factor(c("North", "Central", "South"), levels = c("North", "Central", "South")),
                                       value = exp(c(intercept, 
                                                     intercept + d[variable == "regionCentral"]$estimate,
                                                     intercept + d[variable == "regionSouth"]$estimate )), 
                                       lwr = exp(c(d[variable == "(Intercept)"]$lower_2.5 + season_mean, 
                                                   d[variable == "regionCentral"]$lower_2.5 + intercept,
                                                   d[variable == "regionSouth"]$lower_2.5 + intercept)), 
                                       upr = exp(c(d[variable == "(Intercept)"]$upper_97.5 + season_mean, 
                                                   d[variable == "regionCentral"]$upper_97.5 + intercept,
                                                   d[variable == "regionSouth"]$upper_97.5 + intercept))
  ))
}

fwrite(pred_df, here::here("03_outputs/region_preds.csv"))

plot_season_effects <- function(var,  units = "μM", ylab = paste0(var, " (", units, ")") ) {
  d <- pred_df[y == var]
  ggplot(d) + 
    geom_bar(aes(x = region, y = value, fill = region), stat = "identity") + 
    geom_errorbar(aes(x =region, 
                      ymin = lwr, 
                      ymax = upr), width = .2) + 
    #facet_wrap(~y, scale = "free") + 
    scale_x_discrete(limits = c("North", "Central", "South")) + 
    scale_fill_discrete("Region", type = region_colors[c("North", "Central", "South")]) + 
    tsl_theme +
    labs(y = ylab) +
    theme(axis.title.x = element_blank(), 
          legend.position = "none")
}


png(here::here("03_outputs/region_estimates.png"), 
    width = 6, height = 6, units = "in", res = 240)
print((plot_season_effects("DIP") + plot_season_effects("DIN") + plot_season_effects("DIN.DIP", ylab = "DIN/DIP")) /
        (plot_season_effects("TP") + plot_season_effects("TN") + plot_season_effects("TN.TP", ylab = "TN/TP")) / 
        (plot_season_effects("DSi") + plot_season_effects("DO", units = "mg/L") + plot_season_effects("chla", ylab = "chl-a (μg/L)")) + 
        plot_annotation(tag_levels = "a", tag_suffix = ".") & theme(
          plot.tag.location = "plot", 
          plot.margin = rep(unit(.3, "cm"), 4)))
dev.off()

