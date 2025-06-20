library(data.table)
source(here::here("03_figures/themes.R"))
data_dir <- "00_data"
output_dir <- "03_outputs"

# Prep data ----- 
data <- fread(here::here(data_dir, "combined-data.csv"))
#hydro <- fread(here::here(data_dir, "TSL_Hydrologic_Data.csv"))
hydro <- fread(here::here(data_dir, "MRC/lk_water_level.csv"))
# filter to only the years we're interested in for now
hydro <- hydro[time > as.POSIXct("2012-01-01") & 
                 time < as.POSIXct("2022-05-01")]

# And make sure we have NA for missing dates to plot correctly
hydro[, date := as.Date(time)]
date_seq <- data.table(
  date = seq(min(hydro$date), max(hydro$date), by = as.difftime(1, units = "days"))
)
hydro <- merge(date_seq, hydro, all.x = TRUE)
# and take mean per day
hydro <- hydro[, .(water_level_m = mean(water_level_m)), date]





hydro <- hydro[, .(
  date, 
  year = as.numeric(strftime(date, "%Y")), 
  water_level_m
)]



data[, month := month(date)]

# julian date (day of year)
data[, dateJul := as.numeric(format(date, "%j"))]



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
data[,  hydro_date := (dateJul - hydro_year_start) %% 365]

# Make up lake level data
# min May 1 at 1 m
# max 365/2 days after May 1 at 7m
# 
# lake_level <- data.table(
#   water_level_m = 1000*dnorm(seq(1, 365, len= 50), mean = 365/2, sd = 60)+1, 
#   # lowest water is May 1. 
#   dateJul = seq(1, 365, len = 50) + 122
# )

# lake_level <- hydro[, .(
#   water_level_m = WL, 
#   dateJul = DOY
# )]
# lake_level[, hydro_day := (dateJul - hydro_year_start) %% 365]


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
fwrite(lake_level, here::here(output_dir, "mean_lake_level.csv"))

# last-minute data cleanup
data[DIN.DIP > 1000, DIN.DIP := NA]
d <- hydro[hydro_year %in% c(2012, 2013, 2014, 2015, 2019, 2020, 2021, 2022) ]
d[, has_data := date %in% data[env == "pelagic"]$date]


# Water Level Plot ----- 

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
  
  geom_line(aes(x = hydro_day, 
                y  = water_level_m, 
                group = as.factor(hydro_year), 
                colour = as.factor(hydro_year))) + 
  geom_point(data = d[has_data == TRUE], 
             aes(x = hydro_day, y = water_level_m, 
                 colour = as.factor(hydro_year))) +
  # geom_line(data = lake_level, 
  #           aes(x = hydro_day, 
  #               y = water_level_m, 
  #               size = "mean 2013-2022"), 
  #           #color = "#c16400", 
  #           lwd = 2) +
  tsl_theme +
  scale_x_continuous(breaks = hydro_mon[c(6, 9, 12, 3)], 
                     labels = mon_names[c(6, 9, 12, 3)],
                     limits = c(0, 365), 
                     expand = c(0.01, 0)
  ) + 
  theme(panel.grid = element_blank()) +
  labs(x = "", y = "Water Level (m)") +
  geom_text(data = data.frame(x = hydro_mon[c(7, 10, 1, 4)] + 15,
                              y = 10, 
                              label = c("rising", "high", "falling", "low")), 
            aes(x = x, y = y, label = label), 
            color = c("#458a93", "#2c5619", "#a3640d", "#8c372e"))+
  scale_size_manual("", values = 2) + 
  scale_color_discrete(name = "Water Year",
                     # values = c("#d0d1e6", "#a6bddb",
                     #            "#74a9cf",
                     #            "#2b8cbe",
                     #            "#045a8d")
  )
water_level_plot

png(here::here(output_dir, "season_by_water_level.png"), width = 5, height = 4, units = "in", res = 240)
print(water_level_plot)
dev.off()


if (FALSE) { # Don't include these in the automated script
  ## validation
  # Quick check to see if it line's up with Mauricio's data
  
  
  hydro_MA <- fread(here::here(data_dir, "TSL_wl.csv"))
  hydro_MA <- hydro_MA[, .(
    date = as.Date(date),
    year = as.numeric(strftime(date, "%Y")),
    water_level_m = `K. Loung MRC complete with Prek Dam estimates m amsl`
  )]
  hydro_merged <- merge(hydro_MA, hydro, by = "date", 
                        all.x = TRUE, all.y = TRUE)
  ggplot(m) + 
    geom_point(aes(x =  water_level_m.x, y = water_level_m.y, color = hydro_year)) + 
    geom_abline()
  
  
  # Exceedance plot -----
  
  # x axis is number of days that water level exceeded that number
  # y axis is water level 
  
  # start with mean
  water_level_range <- seq(8, 0, by = -0.1)
  
  exceedance <- sapply(water_level_range, function(wl) {
    total_days <- nrow(lake_level)
    s <- sum(lake_level$water_level_m > wl)
    s <- ifelse(s == total_days, NA, s)
    ifelse(s < 1, NA, s)
  })
  dt_mean <- data.table(days = exceedance, 
                        water_level = water_level_range)
  
  
  dt <- data.table(hydro_year = numeric(), 
                   days = numeric(), 
                   water_level = numeric())
  for (y in unique(d$hydro_year) ) {
    d_y <- d[hydro_year == y]
    total_days <- nrow(d_y)
    exceedance <- sapply(water_level_range, function(wl) {
      s <- sum(d_y$water_level_m > wl)
      s <- ifelse(s == total_days, NA, s)
      ifelse(s < 1, NA, s)
    })
    exc_dt <- data.table(
      hydro_year = y,
      days = exceedance, 
      water_level = water_level_range
    )
    dt <- rbind(dt, exc_dt)
  }
  
  ggplot(dt) + 
    geom_line(aes(x = days,
                  y = water_level, 
                  group = hydro_year, 
                  color = hydro_year))+ 
    geom_line(data = dt_mean, 
              aes(x = days, y = water_level), 
              color = "black", 
              linewidth = 2)
  
  
  # another way to do it? 
  
  level_ordered <- lake_level[order(-water_level_m)]
  level_ordered$days_exceeding <- 1:nrow(level_ordered)
  
  
  dt <- data.table(hydro_year = numeric(), 
                   days = numeric(), 
                   water_level_m = numeric(), 
                   has_data = logical())
  
  d[, has_data := date %in% data[env == "pelagic"]$date]
  
  for (y in unique(d$hydro_year) ) {
    d_y <- d[hydro_year == y]
    d_ordered <- d_y[order(-water_level_m)]
    d_ordered$days <- 1:nrow(d_ordered)
    dt <- rbind(dt, d_ordered[, .(hydro_year, days, water_level_m, has_data)])
  }
  
  ggplot(dt) + 
    geom_line(aes(x = days,
                  y = water_level_m, 
                  group = hydro_year, 
                  color = hydro_year))+ 
    geom_point(data=dt[has_data == TRUE], 
               aes(x = days, y = water_level_m, 
                   color = hydro_year))
  # geom_line(data = level_ordered, 
  #           aes(x = days_exceeding, y = water_level_m), 
  #           color = "black", 
  #           linewidth = 2)
  
  # 
  # ggplot(dt) + 
  #   geom_line(aes(x = days,
  #                 y = water_level, 
  #                 group = hydro_year, 
  #                 color = hydro_year))+ 
  #   geom_line(data = dt_mean, 
  #             aes(x = days, y = water_level), 
  #             color = "black", 
  #             linewidth = 2)+
  #   tsl_theme +
  #   theme(panel.grid = element_blank()) +
  
  
  
}
