library(data.table)
data_dir <- "00_data"
loken <- fread(here::here(data_dir, "MergedChemistry_Flamebodia.csv"))
loken <- loken[constituent %in% c("TDN", 
                                  "TN", 
                                  "NH4", 
                                  "TP", 
                                  "nitrate", 
                                  "phosphate")]
loken[constituent == "nitrate", constituent := "NO3"]
loken[constituent == "phosphate", constituent := "DIP"]
loken <- dcast(loken, 
      sample_date + site_id + site_type + latitude + longitude~ constituent, 
      value.var = "result_value")

# nitrate and phosphate are ppm
# NH4, TDN, TN, TP are ug/L

# ug/L -> uM/L
# ug/L * 1,000,000 g/ug * 1/(x g/Mol) / 1,000,000 uM/Mol
loken$TN_uM <- loken$TN / 14.0067
loken$TP_uM <- loken$TP / 30.973761
loken$DIN_uM <- loken$TDN / 14.0067
loken$NH4_uM <- loken$NH4 / 18.03846

# ppm -> uM/L
loken$NO3_uM <- loken$NO3 * 1000 / 62.004
loken$DIP_uM <- loken$DIP * 1000 / 94.9714

loken[, env := ifelse(grepl("Edge", site_type), 
                      "edge", NA)]
loken[, env := ifelse(grepl("Middle", site_type), 
                      "pelagic", env)]
loken[, env := ifelse(grepl("Mekong", site_type), 
                      "river", env)]
loken[, env := ifelse(grepl("Outlet", site_type), 
                      "river", env)]

fwrite(loken, here::here(data_dir, "loken_nutrients.csv"))

# # plot them 
# library(sf)
# library(ggplot2)
# library(basemaps)
# 
# data_sf <- st_as_sf(loken, coords = c("longitude", "latitude"), crs = "EPSG:4326")
# data_sf <- st_transform(data_sf, "EPSG:3857")
# 
# basemap_ggplot(ext = st_buffer(data_sf, 10000), map_service = "esri", map_type="natgeo_world_map") +
#   geom_sf(data = data_sf,  
#     mapping = aes(color = env), 
#     size = 1) +
#   data_colors_dark
# 
