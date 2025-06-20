library(openxlsx)
library(data.table)
data_dir <- "00_data"

hol_raw <- read.xlsx(here::here(data_dir, "HoltgrieveLab_Nutrients_OSB.xlsx"),
                 sheet = "Nutrient Concentrations", 
                 startRow = 3, 
                 detectDate = TRUE) |> data.table()


hol <- copy(hol_raw)
hol[, `:=`(
  # > unique(hol$Sample.Depth)
  # [1] "0.1 m from surface" "0.5 m from bottom" 
  depth_class = ifelse(grepl("surface", Sample.Depth), 
                       "surface", 
                       "bottom"),
  lat = as.numeric(sub("N\\. ", "", Latitude)),
  lon = as.numeric(as.numeric(sub("E\\. ", "", Longitude)))
)]

# Classify samples: 
# river, edge, floodplain, pelagic
hol[, env := ifelse(grepl("R\\W", Sample.Label), 
                    "river", 
                    ifelse(grepl("E\\W", Sample.Label),
                           "edge",
                           ifelse(grepl("O\\W", Sample.Label), 
                                  "pelagic", 
                                  ifelse(grepl("F\\W", Sample.Label), 
                                         "floodplain", 
                                         NA))))]

hol[, env := ifelse(grepl("river", Sample.Site, ignore.case = TRUE), 
                    "river", 
                    ifelse(grepl("edge", Sample.Site, ignore.case = TRUE), 
                           "edge", 
                           ifelse(grepl("open", Sample.Site, ignore.case = TRUE), 
                                  "pelagic", 
                                  ifelse(grepl("flood", Sample.Site), 
                                         "floodplain", 
                                         env))))]

# I think SrP points are all river
hol <- hol[grepl("SrP", Sample.Label), env := "river"]

# For now, label the rest as "unknown" so they're not NA
hol[is.na(env), env := 'unknown']

# I think the ones with "transect" in the name are 
# from the transects we already have

# > hol[grepl('transect', Sample.Site, TRUE), .N, Sample.Site]
#                               Sample.Site     N
#                                    <char> <int>
# 1:                  Anlang Reang transect    48
# 2: Kampong Chhnang/Kampong Preah transect    21
# 3:                  Prek Konteil transect    22
 
hol[, transect := grepl("transect", Sample.Site)]
 
hol[, tsl := env != "river"]
hol <- hol[(transect == FALSE)]
# hol <- hol[(transect == FALSE) & (tsl == TRUE)]
# correct data 
# Prek Kontiel: label as Open, edge, fpl based on transect location\
hol[grepl("PrKo", Sample.Label) & 
      Sample.Date == as.IDate("2015-07-01"), 
    env := 'pelagic']
hol[grepl("PrKo", Sample.Label) & 
      Sample.Date == as.IDate("2015-03-16"), 
    env := "edge"]
hol[grepl("PrKo", Sample.Label) & 
      env == "unknown", 
    env := "floodplain"]
#   Drop the weird one
hol <- hol[!(grepl("PrKo", Sample.Label) & 
            Sample.Date == as.IDate("2015-01-01"))]


# Prek Toal: reassign to near correct points
hol[grepl("PrTo", Sample.Label) & 
      Sample.Date == as.IDate("2015-01-01") & 
      env == "pelagic", 
    `:=`(lat = 13.24449, lon = 103.70402)]
hol[grepl("PrTo", Sample.Label) & 
      Sample.Date == as.IDate("2015-01-01") & 
      env == "edge", 
    `:=`(lat = 13.24751, lon = 103.69703)]
hol[grepl("PrTo", Sample.Label) & 
      Sample.Date == as.IDate("2015-01-01") & 
      env == "floodplain", 
    `:=`(lat = 13.24803, lon = 103.69299)]


# midlake: reassign to correct point
hol[grepl("Mid", Sample.Label), 
    `:=`(lat = 12.69156, lon = 104.20988, 
         env = "pelagic", 
         Sample.Label = "Midlake")]
# Anlang Reang: match to transect data
hol[grepl("AnRe", Sample.Label) & env == "pelagic", 
    `:=`(lat = 12.65388, lon = 104.16687)]
hol[grepl("AnRe", Sample.Label) & env == "edge",
    `:=`(lat = 12.65222, lon =  104.14899)]
hol[grepl("AnRe", Sample.Label) & env == "floodplain",
    `:=`(lat = 12.64576, lon =104.14134)]


# Phat Sanday: reassign to near correct points

hol[grepl("PhSa", Sample.Label) & 
      env == "pelagic" & 
      Sample.Date %in% c(as.IDate("2015-01-01"), as.IDate("2014-09-14")), 
    `:=`(lat = 12.57056, lon = 104.43185)]
hol[grepl("PhSa", Sample.Label) & 
      env == "edge" & 
      Sample.Date %in% c(as.IDate("2015-01-01"), as.IDate("2014-09-14")),
    `:=`(lat = 12.57097, lon =  104.43597)]
hol[grepl("PhSa", Sample.Label) & 
      env == "floodplain" & 
      Sample.Date %in% c(as.IDate("2015-01-01"), as.IDate("2014-09-14")),
    `:=`(lat = 12.54738, lon =104.46304)]

# Balot: reassign to near correct points
hol[grepl("Ba", Sample.Label) & env == "pelagic" & Sample.Date == as.IDate("2015-01-01"), 
    `:=`(lat = 12.74111, lon = 104.3518)]
hol[grepl("Ba", Sample.Label) & env == "edge" & Sample.Date == as.IDate("2015-01-01"), 
    `:=`(lat = 12.74496, lon = 104.3491)]
hol[grepl("Ba", Sample.Label) & env == "floodplain" & Sample.Date == as.IDate("2015-01-01"), 
    `:=`(lat = 12.74914, lon = 104.3396)]

# Boeng Chmar: don't include (??)
hol <- hol[!grepl("BoCh", Sample.Label)]

# different regions

# Lake regions
# Balot 
# Boeung Chhmar
# Kampong Kleang
# Prek Toal
# Prek Konteil
# Phat Sanday
# Midlake

# Transects
# Anlang Reang (2 missing lat/lon)
# Prek Konteil
# Kampong Chhang/Kampong Preah


# Rivers
# Kampong Prahok River
# Sangker River
# Stung Saen River
# Kampong Chhnang/Kampong Preah
# Stoung River (missing lat/lon)
# Upper Saen River (missing lat/lon)
# Chinit river (missing lat/lon)
# 

# write this to look at map in python
fwrite(hol, 
       here::here(data_dir, "holtgrieve.csv"))


# # plot them 
# library(sf)
# library(ggplot2)
# library(basemaps)
# 
# data_sf <- st_as_sf(hol[!is.na(lat)], coords = c("lon", "lat"), crs = "EPSG:4326")
# data_sf <- st_transform(data_sf, "EPSG:3857")
# 
# basemap_ggplot(ext = st_buffer(data_sf, 10000), map_service = "esri", map_type="natgeo_world_map") +
#   geom_sf(data = data_sf[
#                            hol[!is.na(lat)]$lat < 12.5, ], 
#           mapping = aes(color = env), 
#           size = 1) 

