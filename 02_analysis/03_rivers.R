library(data.table)
library(ggplot2)
library(patchwork)
source(here::here("03_figures/themes.R"))
data_dir <- "00_data"
output_dir <- "03_outputs"


data <- fread(here::here(data_dir, "combined-data.csv"))
river_data <- data[grepl("river", env)]

t.test(river_data[env == "river_mekong" & season == "high"]$TP, river_data[env == "river_mekong" & season == "low"]$TP)
t.test(data[env == "river_mekong" & season == "high" & dataset == "loken"]$TP, data[env == "pelagic" & season == "high" & dataset == "loken"]$TP)
t.test(data[env == "river_mekong" & season == "low" & dataset == "loken"]$TP, data[env == "pelagic" & season == "low" & dataset == "loken"]$TP)

t.test(river_data[env == "river_mekong" & season == "high"]$TN, river_data[env == "river_mekong" & season == "low"]$TN)
t.test(data[env == "river_mekong" & season == "high" & dataset == "loken"]$TN, data[env == "pelagic" & season == "high" & dataset == "loken"]$TN)



P <- ggplot(data[dataset == "loken" & 
              !(env %in% c("edge", "floodplain")) &
              season %in% c("low", "high")]) + 
  geom_point(aes(x = env, y = TP)) +
  facet_wrap(~season) +
  tsl_theme +
  scale_x_discrete(limits = c("pelagic", "river_tonlesap", "river_mekong"),
                   labels = c("pelagic" = "pelagic",
                              "river_tonlesap" ="TSR" ,
                              "river_mekong" = "Mekong")) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1.2, hjust = 1)) +
  xlab("") +
  ggtitle("Total Phosphorus")

N <- ggplot(data[dataset == "loken" & 
                   !(env %in% c("edge", "floodplain")) &
                   season %in% c("low", "high")]) + 
  geom_point(aes(x = env, y = TN)) +
  facet_wrap(~season) +
  tsl_theme +
  scale_x_discrete(limits = c("pelagic", "river_tonlesap", "river_mekong"),
                   labels = c("pelagic" = "pelagic",
                              "river_tonlesap" ="TSR" ,
                              "river_mekong" = "Mekong")) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1.2, hjust = 1)) +
  xlab("") +
  ggtitle("Total Nitrogen")

png(here::here(output_dir, "loken_river_concentrations.png"), width = 4, height = 5, units = "in", res = 120)
P/N
dev.off()  

ggplot(data[dataset == "holtgrieve" & 
              !(env %in% c("edge", "floodplain")) & 
              season == "high"]) + 
  geom_boxplot(aes(x = env, y = DIP)) +
  #facet_wrap(~season) +
  tsl_theme +
  scale_x_discrete(limits = c("pelagic", "river_tonlesap", "river_mekong", "river_tributary"),
  labels = c("pelagic" = "pelagic",
             "river_tonlesap" ="TSR" ,
             "river_mekong" = "Mekong",
             "river_tributary" = "Tributaries")) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1.2, hjust = 1)) +
  xlab("") +
  #ylab("DIP (log-scale)") +
  #scale_y_log10() +
  ggtitle("Holtgrieve dataset (high water)") 



# Nitrogen
ggplot(data[dataset == "loken" & 
              !(env %in% c("edge", "floodplain")) & 
              season %in% c("high", "low")]) + 
  geom_point(aes(x = env, y = TN)) +
  facet_wrap(~season) +
  tsl_theme +
  scale_x_discrete(limits = c("pelagic", "river_tonlesap", "river_mekong"),
                   labels = c("pelagic" = "pelagic",
                              "river_tonlesap" ="TSR" ,
                              "river_mekong" = "Mekong")) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1.2, hjust = 1)) +
  xlab("") +
  ggtitle("Loken dataset")

ggplot(data[dataset == "holtgrieve" & 
              !(env %in% c("edge", "floodplain")) & 
              season == "high"]) + 
  geom_boxplot(aes(x = env, y = DIN)) +
  #facet_wrap(~season) +
  tsl_theme +
  scale_x_discrete(limits = c("pelagic", "river_tonlesap", "river_mekong", "river_tributary"),
                   labels = c("pelagic" = "pelagic",
                              "river_tonlesap" ="TSR" ,
                              "river_mekong" = "Mekong",
                              "river_tributary" = "Tributaries")) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1.2, hjust = 1)) +
  xlab("") +
  #ylab("DIN (log-scale)") +
  # scale_y_log10() +
  ggtitle("Holtgrieve dataset (high water)") 
