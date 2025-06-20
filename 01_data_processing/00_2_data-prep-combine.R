library(data.table)
library(sf)

# Read in all datasets ----
data_dir <- "00_data"
mil <- fread(here::here(data_dir, "tsl-nutrients.csv"))
heu <- fread(here::here(data_dir, "heu.csv"))
bur <- fread(here::here(data_dir, "burnett_2017.csv"))
hol <- fread(here::here(data_dir, "holtgrieve.csv"))
lok <- fread(here::here(data_dir, "loken_nutrients.csv"))
yos <- fread(here::here(data_dir, "yoshikawa_2024.csv"))
mrc <- fread(here::here(data_dir, "MRC/mrc_water_quality.csv"))


hydro <- fread(here::here(data_dir, "MRC/lk_water_level.csv"))

# classify river sites: 
# river_tonlesap
# river_mekong
# river_tributary
hol[env == "river", env := ifelse(grepl("Mekong", Sample.Site), "river_mekong", 
                                  ifelse(grepl("Tonle Sap", Sample.Site), "river_tonlesap", 
                                         "river_tributary"))]
lok[env == "river", env := ifelse(grepl("Mekong", site_type), "river_mekong", 
                                  "river_tonlesap")]



# combine them ----
data <- rbindlist(list(
  mil[(CLASS_Z == "Surface") , .(
    date = as.Date(DATE, format = "%m/%d/%y"), 
    lon = longitude, 
    lat = latitude, 
    depth = tolower(CLASS_Z), 
    DIP = PHOSPHATE_uM, 
    DIN = DIN, 
    TP = NA, 
    TN = NA, 
    NO3 = NITRATE_uM,
    DSi = SILICA_uM, 
    chla = CHL_mgL,
    DO = O2_mgL,
    cond = COND * 10000,
    env = tolower(ENVIRON),
    dataset = "miller"
  )], 
  
  hol[(depth_class == "surface"), .(
    date = as.Date(Sample.Date), 
    lon, 
    lat,
    depth = tolower(depth_class), 
    DIP = `PO4.(uM)`, 
    DIN = `NO3.(uM)` + `NO2.(uM)` + `NH4.(uM)`, 
    TP = NA, 
    TN = NA, 
    NO3 = `NO3.(uM)`, 
    DSi = `Si(OH)4.(uM)`, 
    chla = NA, 
    DO = NA,
    cond = NA,
    env = env, 
    dataset = "holtgrieve"
  )],
  
  heu[, .(
    date = as.Date(date, format = "%m/%d/%y"), 
    lon, 
    lat, 
    depth = "surface",
    DIP = NA, 
    DIN = NA, 
    TP = P_uM, 
    TN = N_uM, 
    NO3 = NA, 
    DSi = Si_uM, 
    chla = Chla, 
    DO = DO, 
    cond = NA,
    env = "pelagic",
    dataset = "heu"
  )],
  
  bur[, .(
    date = as.Date(date), 
    lon, 
    lat, 
    depth = ifelse(sample_depth > 1.5, "bottom", "surface"), 
    DIP = DIP, 
    DIN = DIN, 
    TP = TP, 
    TN = TN, 
    NO3 = NO3,
    DSi = DSi, 
    chla = `Chl-a`,
    DO = DO,
    cond = Cond,
    env = "pelagic",
    dataset = "burnett"
  )], 
  
  lok[, .(
    date = as.Date(sample_date), 
    lon = longitude, 
    lat = latitude, 
    depth = "surface", 
    DIP = NA, 
    DIN = DIN_uM, 
    TP = TP_uM, 
    TN = TN_uM, 
    NO3 = NO3, 
    DSi = NA, 
    chla = NA, 
    DO = NA, 
    cond = NA, 
    env = env, 
    dataset = "loken"
  )], 
  
  yos[, .(
    date = as.Date(date), 
    lon, 
    lat, 
    depth = "surface", 
    DIP = `PO4.(uM)`, 
    DIN = DIN, 
    TP = DP, 
    TN = NA, 
    NO3 = `NO3+NO2.(uM)`, 
    DSi = Si_uM, 
    chla = Chl.a, 
    DO = DO, 
    cond = NA, 
    env = env, 
    dataset = "yoshikawa"
  )], 
  
  mrc[, .(
    date = as.Date(time), 
    lon, 
    lat,
    depth = "surface", 
    DIP = NA, 
    DIN = NA, 
    TP = `Total Phosphorous` * 1000 / 30.973761, # mg/L to uM/L
    TN = `Total Nitrogen` * 1000 / 14.0067,  # mg/L to uM/L
    NO3 = `Nitrite-Nitrate` * 1000 / 62.004, # mg/L to uM/L
    DSi = NA, 
    chla = NA, 
    DO = NA,
    cond = Conductivity * 1000 / 100, #mS/m to uS/cm
    env = env, 
    dataset = paste0("mrc-", Description)
  )]
))

# add ratios
data[, DIN.DIP := DIN/DIP]
data[, TN.TP := TN/TP]
# remove any Inf (divide by 0)
data[DIN.DIP == Inf, DIN.DIP := NA]
data[TN.TP == Inf, TN.TP := NA]


jul_mon <- c(2, 33, 61, 92, 122, 153, 183, 214, 245, 275, 306, 336)
mon_names <- strftime(strptime(paste0(1:12, "-01-2020"), format = "%m-%d-%Y"), format = "%b")
# Mar: low
# Jun: rising
# Sep: high
# Dec: falling
season_to_hydro_day = data.table(
  season = c("low", "rising", "high", "falling"), 
  start = c(jul_mon[3], jul_mon[6], jul_mon[9], jul_mon[12]),
  end = c(jul_mon[6]-1, jul_mon[9]-1, jul_mon[12]-1, jul_mon[3]-1)
)

season_map = c(
  # low = apr, may, jun
  "Apr" = "low", 
  "May" = "low", 
  "Jun" = "rising", 
  # rising = jul, aug, sep
  "Jul" = "rising", 
  "Aug" = "rising", 
  "Sep" = "high", 
  # high = oct, nov, dec
  "Oct" = "high", 
  "Nov" = "high", 
  "Dec" = "falling", 
  # falling = jan, feb, mar
  "Jan" = "falling", 
  "Feb" = "falling", 
  "Mar" = "low"
)
data[, season := season_map[strftime(data$date, "%b")]]

data <- data[!duplicated(data)]

# Add region indicator
d_sf <- st_as_sf(data[!is.na(lon)], coords = c('lon', 'lat'), crs = "EPSG:4326")
regions <- read_sf(here::here(data_dir, "TSL-regions.kml"))
#st_write(regions,here::here(data_dir, "tsl-regions.geojson"))

d_sf <- st_intersection(d_sf, regions)
d_sf$Description <- NULL
d_region <- st_set_geometry(d_sf, NULL)
new_data <- merge(data, d_region, 
      all.x = TRUE, 
      sort = FALSE, 
      by = intersect(names(data), names(d_sf)))
setnames(new_data, "Name", "region")

# remove any points which do not fall itno the regions
# because they are not in the lake
# new_data <- new_data[!is.na(region)]


# Add lake level where available

hydro <- hydro[, .(
  date = as.Date(time), 
  year = as.numeric(strftime(time, "%Y")),
  water_level_m 
)][, .(water_level_m = mean(water_level_m)), date]

new_data <- merge(new_data, hydro, all.x = TRUE, by = "date", sort = FALSE)

# save ----

fwrite(new_data[!grepl("mrc", dataset)], here::here(data_dir, "combined-data.csv"))
fwrite(new_data[grepl("mrc", dataset)], here::here(data_dir, "MRC/mrc_water_quality_clean.csv"))
# also write season info for downstream analysis
fwrite(season_to_hydro_day, here::here(data_dir, "season_to_doy.csv"))

