# MRC monitoring data
library(data.table)
library(sf)
#library(ggplot2)
#library(ggrepel)
#library(basemaps)
data_dir <-here::here("00_data/MRC")
raw_data_dir <- file.path(data_dir, "/mrc20241218220443/")


# stations are: 
# Kratie: Mekong, a ways upstream
# Chroy Chang Var: Mekong, near (just upstream) confluence with TSR
# Phnom Penh Port: TSR, near confluence with Mekong
# Kg. Chhnang: TSR near lake outlet(?) -- or Mekong upstream of Phnom Penh
# Kompong Luong: Tonle Sap Lake
# Backprea: Sang Keo river--tributary flowing into TSL from NW
# Prek Kdam: TSR near Phnom Penh


# Get lake level
kl <- fread(file.path(raw_data_dir, "Water Level.Manual@KH_020106_[Kompong Luong].csv"))
kl[, time := as.POSIXct(`Timestamp (UTC+07:00)`, format = "%Y-%m-%dT%H:%MZ", 
                        tz = "Asia/Phnom_Penh")]
fwrite(kl[, .(time, water_level_m = Value)], 
       file.path(data_dir, "lk_water_level.csv"))


files <- list.files(raw_data_dir)
data_files <- files[grepl("csv$", files)]

# get one location file per location (use conductivity)
location_files <- files[grepl("location\\.kml", files) & 
                          grepl("Conductivity", files)]
location_list <- lapply(file.path(raw_data_dir, location_files), 
       read_sf)
locations_sf <- st_as_sf(rbindlist(location_list))
locations <- as.data.table(locations_sf)

coords <- st_coordinates(locations_sf)
locations <- locations[, .(Name, 
                           Description, 
                           lon = coords[, 'X'], 
                           lat = coords[, 'Y'])]
locations[, env := ifelse(
  Description %in% c("Kratie", 
                     "Chroy Chang Var", 
                     "Kg. Chhnang"), 
  "river_mekong", 
  ifelse(
    Description %in% c("Phnom Penh Port",
                       "Prek Kdam"),
    "river_tonlesap", 
    ifelse(
      Description %in% c("Backprea"), 
      "river_tributary", 
      ifelse(
        Description %in% c("Kompong Luong"), 
        "Pelagic", 
        NA
      )
    )
    
  )
)]

locations_sf <- st_transform(locations_sf, "EPSG:3857")

# # Make a map of locations
# basemap <- basemap_ggplot(ext = st_buffer(locations_sf, 10000), map_service = "esri", map_type="natgeo_world_map")
# basemap + 
#   geom_sf(data = locations_sf) + 
#   geom_text_repel(data = locations_sf, 
#                   aes(label = Description, 
#                        geometry = geometry), 
#                    stat= "sf_coordinates")


# load all observations
data_list <- lapply(data_files, function(f) {
  # Parameter.Type@LOCATION.csv
  param <- regmatches(f, regexpr("\\w*", f))
  location <- regmatches(f, regexpr("(?<=@).*(?=(\\.))", f, perl = TRUE))
  
  d <- fread(file.path(raw_data_dir, f))
  d[, .(
    Name = location, 
    param = Parameter, 
    value = Value, 
    time = as.POSIXct(`Timestamp (UTC+07:00)`, 
                      format = "%Y-%m-%dT%H:%MZ", 
                      tz = "Asia/Phnom_Penh")
  )]
})

data <- rbindlist(data_list)
# remove water level
data <- data[param != "Water Level"]
data <- merge(data, locations, by = "Name")
data <- dcast(data, ... ~ param, value.var = "value")

# Fix conductivity units


fwrite(data, file.path(data_dir, "mrc_water_quality.csv"))


