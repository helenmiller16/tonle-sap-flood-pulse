library(data.table)
library(ggplot2)
library(patchwork)
library(lme4)
data_dir <- "00_data"
figures_dir <- "03_figures"
output_dir <- "03_outputs"
source(here::here(figures_dir, "themes.R"))

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


# N by P -----
din_dip <- glm(DIP + delta ~ log(DIN + delta) + dataset + region,  
               family = Gamma(link="log"), 
               data= data, 
               na.action = na.omit)
summary(din_dip)
ggplot(data) + 
  geom_point(aes(x = DIN, 
                 y = DIP, 
                 color = dataset)) + 
  scale_x_log10() + 
  scale_y_log10() + 
  tsl_theme

tn_tp <- glm(TP + delta ~ log(TN + delta) + dataset + region,  
             family = Gamma(link="log"), 
             data= data, 
             na.action = na.omit)
summary(tn_tp)

ggplot(data) + 
  geom_point(aes(x = TN, 
                 y = TP, 
                 color = dataset)) + 
  scale_x_log10() + 
  scale_y_log10() + 
  tsl_theme

# N, P by cond
# only one dataset with both TN, TP, and cond
tn_cond <- glm(TN ~ cond + region, 
               family = Gamma(link="log"), 
               data= data, 
               na.action = na.omit)
tp_cond <- glm(TP ~ cond +  region, 
               family = Gamma(link="log"), 
               data= data, 
               na.action = na.omit)

dip_cond <- glm(DIP + delta ~ cond + dataset + region, 
                 family = Gamma(link="log"), 
                 data= data, 
                 na.action = na.omit)
din_cond <- glm(DIN ~ cond + dataset + region, 
                family = Gamma(link="log"), 
                data= data, 
                na.action = na.omit)

# Chla and Si by COND
chl_cond <- glm(chla ~ cond + dataset + region, 
                family = Gamma(link="log"), 
                data= data, 
                na.action = na.omit)
summary(chl_cond)

si_cond <- glm(DSi ~ cond + dataset + region, 
               family = Gamma(link="log"), 
               data= data, 
               na.action = na.omit)
summary(si_cond)
ggplot(data) + 
  geom_point(aes(x = cond, 
                 y = DSi, 
                 color = dataset)) + 
  #scale_x_log10() + 
  scale_y_log10() +
  tsl_theme

# make a table of confints
ci <- Reduce(rbind, list(confint(din_dip)[2, ],
                         confint(tn_tp)[2, ], 
                         confint(chl_cond)[2, ], 
                         confint(si_cond)[2, ], 
                         confint(tn_cond)[2, ],
                         confint(din_cond)[2, ],
                         confint(tp_cond)[2, ],
                         confint(dip_cond)[2,]
                         )) 
est <- c(
  coef(din_dip)[2], 
  coef(tn_tp)[2], 
  coef(chl_cond)[2], 
  coef(si_cond)[2], 
  coef(tn_cond)[2], 
  coef(din_cond)[2], 
  coef(tp_cond)[2], 
  coef(dip_cond)[2]
)
ci_table <- cbind(model = c("DIN-DIP", 
        "TN-TP", 
        "Chl-cond", 
        "DSi-cond", 
        "TN-cond", 
        "DIN-cond", 
        "TP-cond", 
        "DIP-cond"), 
        estimate = (est),
      data.frame((ci)))
write.csv(ci_table, here::here(output_dir, "dilution_ci_table.csv"))  

# 
# 
# # Look at MRC conductivity data
# # Tonle Sap River
# # Mekong 
# mrc <- fread(here::here("mrc_water_quality.csv"))
# tsr <- mrc[ env == "river_tonlesap"]
# 
# 
# 
# 
# ggplot(tsr) + 
#   geom_line(aes(x = time, y = Conductivity, group = Name, color = Name)) +
#   xlim(as.POSIXct("2013-01-01"), as.POSIXct("2023-01-01"))
