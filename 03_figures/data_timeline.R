library(data.table)
library(ggplot2)
library(patchwork)
source(here::here("03_figures/themes.R"))
data_dir <- "00_data"
output_dir <- "03_outputs"

# Prep  data -----
data <- fread(here::here(data_dir, "combined-data.csv"))
hydro <- fread(here::here(data_dir, "MRC/lk_water_level.csv"))
hydro <- hydro[time > as.POSIXct("2012-01-01") & 
                 time < as.POSIXct("2022-05-01")]

data[, type := 
       ifelse(is.na(DIN.DIP) & !is.na(TN.TP), "Total", 
              ifelse(is.na(TN.TP) & !is.na(DIN.DIP), "Dissolved", 
                     ifelse(!is.na(TN.TP) & !(is.na(DIN.DIP)), "Both", 
                            NA)))
]
# Data over time ----- 

order <- c( "loken", "heu", "miller", "holtgrieve","yoshikawa", "burnett")
.simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1, 1)), substring(s, 2),
        sep = "", collapse = " ")
}

data <- data[env == "pelagic" & depth == "surface"]
data[, month := strftime(data$date, format = "%G-%m")]
d1 <- data[, .(.N, date = mean(date)), .(month, dataset, type)]

## Data over time plot -----
p1 <- ggplot(d1) + 
  geom_point(aes(x = date, y = dataset, shape = type, size = N)) +
  scale_y_discrete(limits = order, 
                   labels = sapply(order, .simpleCap)) + 
  scale_shape_manual(values = c(
    "Dissolved" = 0, 
    "Total" = 16, 
    "Both" = 15), 
    limits= c("Dissolved", "Total", "Both")) +
  tsl_theme + 
  theme(axis.title = element_blank(), 
        panel.border = element_blank())

png(here::here(output_dir, "data_collection_date.png"), width = 5, height = 3, units = "in", res = 120)
print(p1)
dev.off()


# Season Plot ----- 
## Data prep -----

# Make sure we have NA for missing dates to plot correctly
hydro[, date := as.Date(time)]
date_seq <- data.table(
  date = seq(min(hydro$date), max(hydro$date), by = as.difftime(1, units = "days"))
)
hydro <- merge(date_seq, hydro, all.x = TRUE)
# and take mean per day
hydro <- hydro[, .(water_level_m = mean(water_level_m)), date]
# add year
hydro <- hydro[, .(
  date, 
  year = as.numeric(strftime(date, "%Y")), 
  water_level_m
)]




data[, month := month(date)]
# julian date (day of year)
data[, dateJul := as.numeric(format(date, "%j"))]

# set season as factor in the correct order
data[, season := factor(season, levels = c("rising", "high", "falling", "low"))]

# julian per month
# # January	2
# February	33
# March	61
# April	92
# May	122
# June	153
# July	183
# August	214
# September	245
# October	275
# November	306
# December	336
jul_mon <- c(2, 33, 61, 92, 122, 153, 183, 214, 245, 275, 306, 336)
mon_names <- strftime(strptime(paste0(1:12, "-01-2020"), format = "%m-%d-%Y"), format = "%b")
# start hydro year in May (low water)
hydro_year_start = jul_mon[5] + 15

# Shift everything back 5 months so year starts in May
hydro_mon <- (jul_mon - hydro_year_start) %% 365

# Add hydro day of year
data[,  hydro_date := (dateJul - hydro_year_start) %% 365]

## Water level ------

hydro[, dateJul := as.numeric(format(date, "%j"))]
hydro[, hydro_day := (dateJul - hydro_year_start) %% 365]
hydro[, hydro_year := ifelse(hydro_day > 365 - hydro_year_start, 
                             year - 1, 
                             year)]
# need to fix Dec 31 for leap years
hydro[dateJul == 366, hydro_year := hydro_year + 1] 
# get daily mean? 
lake_level <- hydro[hydro_year > 2013 & hydro_year < 2022, 
                    .(water_level_m = mean(water_level_m, na.rm = TRUE)), 
                    hydro_day]

# last-minute data cleanup
data[DIN.DIP > 1000, DIN.DIP := NA]
d <- hydro[hydro_year %in% c(2012, 2013, 2014, 2015, 2019, 2020, 2021, 2022) ]
d[, has_data := date %in% data$date]

## Water level Plot -----
water_level_plot <- ggplot(d) + 
  # Seasons change at 
  #apr, Jul, Oct, Jan
  geom_rect(data = data.frame(
    x_start = c(-Inf, hydro_mon[c(6, 9, 12, 3)]+.5),
    x_end = c(hydro_mon[c(6, 9, 12, 3)]-.5, Inf)),
    mapping = aes(xmin = x_start,
                  xmax = x_end,
                  ymin = - Inf,
                  ymax = Inf),
    fill = c("#8c372e", "#458a93", "#2c5619", "#a3640d", "#8c372e"),
    alpha = 0.2) +
  geom_vline(data = data.frame(x = hydro_mon[c(6, 9, 12, 3)]-.5), 
             aes(xintercept = x), 
             color = c("#8c372e", "#458a93", "#2c5619", "#a3640d"))+
  geom_vline(data = data.frame(x = hydro_mon[c(6, 9, 12, 3)]+.5), 
             aes(xintercept = x), 
             color = c("#458a93", "#2c5619", "#a3640d", "#8c372e"))+
  
  geom_line(data = lake_level, 
            aes(x = hydro_day, 
                y = water_level_m, 
                linewidth = "mean 2013-2022"), 
            #color = "#c16400", 
            lwd = 2) +
  geom_line(aes(x = hydro_day, 
                y  = water_level_m, 
                group = as.factor(hydro_year), 
                colour = as.factor(hydro_year))) + 
  geom_point(data = d[has_data == TRUE], 
             aes(x = hydro_day, y = water_level_m, 
                 colour = as.factor(hydro_year))) +
  tsl_theme +
  scale_x_continuous(breaks = hydro_mon[c(6, 9, 12, 3)], 
                     labels = mon_names[c(6, 9, 12, 3)],
                     limits = c(0, 365), 
                     expand = c(0.01, 0)
  ) + 
  theme(panel.grid = element_blank()) +
  labs(x = "", y = "Water Level (m)") +
  # geom_text(data = data.frame(x = hydro_mon[c(7, 10, 1, 4)] + 15,
  #                             y = 10, 
  #                             label = c("rising", "high", "falling", "low")), 
  #           aes(x = x, y = y, label = label), 
  #           color = c("#458a93", "#2c5619", "#a3640d", "#8c372e"))+
  scale_size_manual("", values = 2) + 
  scale_color_discrete(name = "Water Year",
                       # values = c("#d0d1e6", "#a6bddb",
                       #            "#74a9cf",
                       #            "#2b8cbe",
                       #            "#045a8d")
  )



d2 <- data[, .(.N, hydro_date = mean(hydro_date)), .(month, dataset, type)]

## Data by season plot ----- 
data_by_season_plot <- ggplot(d2) + 
  # Seasons change at 
  #apr, Jul, Oct, Jan
  geom_rect(data = data.frame(
    x_start = c(-Inf, hydro_mon[c(6, 9, 12, 3)]+.5),
    x_end = c(hydro_mon[c(6, 9, 12, 3)]-.5, Inf)),
    mapping = aes(xmin = x_start,
                  xmax = x_end,
                  ymin = - Inf,
                  ymax = Inf),
    fill = c("#8c372e", "#458a93", "#2c5619", "#a3640d", "#8c372e"),
    alpha = 0.2) +
  geom_vline(data = data.frame(x = hydro_mon[c(6, 9, 12, 3)]-.5), 
             aes(xintercept = x), 
             color = c("#8c372e", "#458a93", "#2c5619", "#a3640d"))+
  geom_vline(data = data.frame(x = hydro_mon[c(6, 9, 12, 3)]+.5), 
             aes(xintercept = x), 
             color = c("#458a93", "#2c5619", "#a3640d", "#8c372e"))+
  # geom_text(data = data.frame(x = hydro_mon[c(7, 10, 1, 4)] + 15,
  #                             y = "burnett", 
  #                             label = c("rising", "high", "falling", "low")), 
  #           aes(x = x, y = y, label = label),, 
  #           color = c("#458a93", "#2c5619", "#a3640d", "#8c372e"), nudge_y = 0.4)+
  geom_point(aes(x = hydro_date, y = dataset, shape = type, size = N)) +
  tsl_theme +
  scale_x_continuous(breaks = hydro_mon[c(6, 9, 12, 3)], 
                     labels = mon_names[c(6, 9, 12, 3)],
                     limits = c(0, 365), 
                     expand = c(0.01, 0)
  ) +
  scale_y_discrete(limits = order, 
                   labels = sapply(order, .simpleCap),
                   name = "Dataset") + 
  
  scale_shape_manual(values = c("Dissolved" = 0, 
                                "Total" = 16,
                                "Both" = 15), 
                     limits= c("Dissolved", "Total","Both")) +
  theme(axis.title.x = element_blank(), 
        axis.text.x = element_blank(), 
        panel.grid.minor.x = element_blank())


## Boxplot by season ----- 

d3 <- data[, .N, season]

boxplot_by_season <- ggplot(data[, .N, season]) + 
  geom_rect(data = data.frame(
    x_start = c(-Inf, hydro_mon[c(6, 9, 12, 3)]+.5),
    x_end = c(hydro_mon[c(6, 9, 12, 3)]-.5, Inf)),
    mapping = aes(xmin = x_start,
                  xmax = x_end,
                  ymin = 0,
                  ymax = c(0, d3[order(season)]$N) ),
    fill = c("#8c372e", "#458a93", "#2c5619", "#a3640d", "#8c372e"),
    color = c("transparent", "#458a93", "#2c5619", "#a3640d", "#8c372e"),
    alpha = 0.2
  ) +
  geom_text(data = data.frame(x = hydro_mon[c(7, 10, 1, 4)] + 15,
                              y = 20, 
                              label = c("rising", "high", "falling", "low")), 
            aes(x = x, y = y, label = label),, 
            color = c("#458a93", "#2c5619", "#a3640d", "#8c372e"), nudge_y = 0.4)+
  # geom_bar(aes(x = season), 
  #          fill = c("#458a93", "#2c5619", "#a3640d", "#8c372e"),
  #          color = c("#458a93", "#2c5619", "#a3640d", "#8c372e"),
  #          alpha = 0.2) + 
  #scale_x_discrete(limits = c("rising", "high", "falling", "low")) +
  
  scale_x_continuous(breaks = hydro_mon[c(6, 9, 12, 3)], 
                     labels = mon_names[c(6, 9, 12, 3)],
                     limits = c(0, 365), 
                     expand = c(0.01, 0)
  ) +
  ylab("Count") +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank())


## Full season plot 

season_plot <- boxplot_by_season + data_by_season_plot + water_level_plot +
  plot_layout(ncol = 1, nrow = 3, 
              heights = c(1, 3, 4))

png(here::here(output_dir, "data_collection_season.png"), width = 5, height = 5, units = "in", res = 120)
print(season_plot)
dev.off()


water_level_plot_standalone <- 
  ggplot(d) + 
  # Seasons change at 
  #apr, Jul, Oct, Jan
  geom_rect(data = data.frame(
    x_start = c(-Inf, hydro_mon[c(6, 9, 12, 3)]+.5),
    x_end = c(hydro_mon[c(6, 9, 12, 3)]-.5, Inf)),
    mapping = aes(xmin = x_start,
                  xmax = x_end,
                  ymin = - Inf,
                  ymax = Inf),
    fill = c("#8c372e", "#458a93", "#2c5619", "#a3640d", "#8c372e"),
    alpha = 0.2) +
  geom_vline(data = data.frame(x = hydro_mon[c(6, 9, 12, 3)]-.5), 
             aes(xintercept = x), 
             color = c("#8c372e", "#458a93", "#2c5619", "#a3640d"))+
  geom_vline(data = data.frame(x = hydro_mon[c(6, 9, 12, 3)]+.5), 
             aes(xintercept = x), 
             color = c("#458a93", "#2c5619", "#a3640d", "#8c372e"))+
  
  geom_line(data = lake_level, 
            aes(x = hydro_day, 
                y = water_level_m, 
                linewidth = "mean 2013-2022"), 
            #color = "#c16400", 
            lwd = 2) +
  geom_line(aes(x = hydro_day, 
                y  = water_level_m, 
                group = as.factor(hydro_year), 
                #colour = as.factor(hydro_year)
                ), 
            alpha = 0.5) + 
  geom_point(data = d[has_data == TRUE], 
             aes(x = hydro_day, y = water_level_m, 
                 #colour = as.factor(hydro_year)
                 ), 
             alpha = 0.5) +
  tsl_theme +
  scale_x_continuous(breaks = hydro_mon[c(6, 9, 12, 3)], 
                     labels = mon_names[c(6, 9, 12, 3)],
                     limits = c(0, 365), 
                     expand = c(0.01, 0)
  ) + 
  theme(panel.grid = element_blank()) +
  labs(x = "", y = "Water Level (m)") +
  # geom_text(data = data.frame(x = hydro_mon[c(7, 10, 1, 4)] + 15,
  #                             y = 10, 
  #                             label = c("rising", "high", "falling", "low")), 
  #           aes(x = x, y = y, label = label), 
  #           color = c("#458a93", "#2c5619", "#a3640d", "#8c372e"))+
  scale_size_manual("", values = 2) + 
  scale_color_discrete(name = "Water Year",
                       # values = c("#d0d1e6", "#a6bddb",
                       #            "#74a9cf",
                       #            "#2b8cbe",
                       #            "#045a8d")
  ) + 
    geom_text(data = data.frame(x = hydro_mon[c(7, 10, 1, 4)] + 15,
                              y = 8.5, 
                              label = c("rising", "high", "falling", "low")), 
            aes(x = x, y = y, label = label),, 
            color = c("#458a93", "#2c5619", "#a3640d", "#8c372e")) + 
    scale_y_continuous(expand = c(0, 0.02))
png(here::here("03_outputs/water_level_standalone.png"), width = 5, height = 4, units = "in", res = 240)
print(water_level_plot_standalone)
dev.off()
