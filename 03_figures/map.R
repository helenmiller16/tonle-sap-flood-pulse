library(data.table)
library(sf)
library(ggplot2)
library(ggspatial)
library(sf)
library(basemaps)
library(terra)
library(ggrepel)
source(here::here("03_figures/themes.R"))

data_dir <- "00_data"
output_dir <- "03_outputs"
data <- fread(here::here(data_dir, "combined-data.csv"))
lake_level <- rast(here::here(data_dir, "dur1997daily"))
# rivers from https://data.apps.fao.org/catalog/iso/dc2a5121-0b32-482b-bd9b-64f7a414fa0d
rivers <- read_sf(here::here(data_dir, "rivers_asia_37331"))
rivers <- st_transform(rivers, crs(lake_level))
# mask out 0s 
lake_level <- mask(lake_level, lake_level > 0, maskvalue = 0)
lake_mask <- mask(lake_level, lake_level == 365, maskvalue = 0)
lake_poly <- as.polygons(lake_mask) |> st_as_sf(lake_poly) 
st_crs(lake_poly) <- st_crs(rivers)

data_sf <- st_as_sf(data[!is.na(lat)], coords = c("lon", "lat"), crs = "EPSG:4326")
data_sf$lon <- st_coordinates(data_sf)[, 1]
data_sf$lat <- st_coordinates(data_sf)[, 2]

data_sf <- st_transform(data_sf, crs(lake_level))
#data_sf <- st_transform(data_sf, "EPSG:3857")

regions <- read_sf(here::here(data_dir, "TSL-regions.kml"))
regions <- st_transform(regions, crs(lake_level))

lake_level_df <- as.data.frame(lake_level, xy = TRUE)
lake_level_df <- lake_level_df[lake_level_df$dur1997daily > 0,]

rivers_clipped <- st_intersection(rivers, st_as_sfc(st_bbox(lake_level)))
basemap <- ggplot() + 
  scale_y_continuous(expand = c(0,0), limits = c(1350000, 1508000)) +
  scale_x_continuous(expand = c(0, 0), limits = c(268000, 520000))  +
  coord_fixed() + 
  geom_raster(data = lake_level_df[lake_level_df$dur1997daily > 0,], 
              aes(x = x, y = y, fill = dur1997daily)) 

#basemap <- basemap_ggplot(ext = st_buffer(regions, 10000), map_service = "esri", map_type="natgeo_world_map") 

dataset_labels <- function(x) {
  c(
    yoshikawa = "Yoshikawa et al., 2024", 
    heu = "Heu et al., 2023", 
    burnett = "Burnett et al., 2017", 
    loken = "Loken (unpublished)",
    miller = "Miller (unpublished)", 
    holtgrieve = "Holtgrieve (unpublished)"
  )[x]
}

# set factor order so it shows up in the correct order in the legend
d <- data_sf[data_sf$env == "pelagic" & data_sf$region != "",]
d$dataset <- factor(d$dataset, levels = c("yoshikawa", 
                                                      "burnett", 
                                                      "heu", 
                                                      "loken", 
                                                      "miller", 
                                                      "holtgrieve"))

# Main map with sampling locations -----
main_map <- basemap +
  geom_sf(data = rivers_clipped, aes(linewidth = Strahler, alpha = Strahler), color = "#1d03b2") +
  scale_linewidth(range = c(.5, 1), guide = "none") + 
  scale_alpha(guide = "none") +
  geom_raster(data = lake_level_df[lake_level_df$dur1997daily >= 360,],
              aes(x = x, y = y), fill = "#1d03b2") +
  geom_sf(data = d, 
          mapping = aes(color = dataset, 
                        shape = dataset)) +
  geom_sf(data = regions, linewidth = .5, fill = "transparent")+
  tsl_theme+ 
  theme(
    axis.title.x = element_blank(),  # Remove x-axis label
    axis.title.y = element_blank(),  # Remove y-axis label
    axis.text.x = element_blank(),   # Remove x-axis tick labels
    axis.text.y = element_blank(),   # Remove y-axis tick labels
    axis.ticks.x = element_blank(),  # Remove x-axis ticks
    axis.ticks.y = element_blank(),   # Remove y-axis ticks
    panel.grid = element_blank()
  ) + 
  scale_color_discrete("Dataset", 
                       type = dataset_colors, 
                       labels = dataset_labels) +
  scale_shape("Dataset", 
              labels = dataset_labels) +
   #data_colors_dark +
     scale_fill_continuous(name = "Inundation time",
                     high = "#013896", 
                        low = "#b6d8ba", 
                    breaks = c(3, 6, 9) * 30,
                    labels = c("3 months", "6 months", "9 months"), 
                    limits = c(1, 365)) +
    annotation_scale(location = "bl",
                     pad_x = unit(2.8, "in"), 
                     bar_cols = c("gray30", "gray90"), 
                     line_col = "gray30") + 
    annotation_north_arrow(location = "bl", 
                           #pad_y = unit(1, "cm"), 
                           pad_x = unit(2.2, "in"),
                           height = unit(.5, "in"), 
                           width = unit(.4, "in"), 
                           style = north_arrow_orienteering(
                             line_col = "gray30", 
                             fill = c("gray90", "gray30"), 
                             text_col = "gray30"
                           ))



# Context map -----

countries <- read_sf("C:/github/lmb-metabolism-sensing/data/external/asia_polygons.gpkg")
# make polygon of raster bbox 
map_bbox <- st_as_sf(st_as_sfc(st_bbox(lake_level))) |> 
  st_transform("EPSG:4326")

# Make bbox for countries
lat = c(20.322829, 6.980394)
lon <- c(95.877648, 111.970875)
context_bbox <- data.frame(lon, lat) |> 
  st_as_sf(coords = c('lon', 'lat'), crs = "EPSG:4326") |> 
  st_bbox() |> 
  st_as_sfc()

countries <- countries[context_bbox, ]
rivers_clipped2 <- st_transform(rivers, st_crs(countries))[context_bbox, ]
context_map <- ggplot(countries) + 
  geom_sf() + 
  lims(y = c(6.980394, 20.322829), 
       x = c(95.877648, 111.970875)) +
  geom_sf(data = rivers_clipped2[rivers_clipped2$Strahler > 2, ], 
          aes(alpha = Strahler), 
          color = '#86a8b3', linewidth= .5) + 
  geom_sf(data = st_transform(lake_poly, crs(countries)), fill = '#86a8b3', 
          color = "#86a8b3") + 
  tsl_theme+ 
  theme(panel.background = element_rect(fill = 'lightblue'), 
          axis.title.x = element_blank(),  # Remove x-axis label
          axis.title.y = element_blank(),  # Remove y-axis label
          axis.text.x = element_blank(),   # Remove x-axis tick labels
          axis.text.y = element_blank(),   # Remove y-axis tick labels
          axis.ticks.x = element_blank(),  # Remove x-axis ticks
          axis.ticks.y = element_blank(),   # Remove y-axis ticks
          panel.grid = element_blank(),
          legend.position = "none"
        ) +
   geom_sf(data = map_bbox, 
           linewidth = 2, 
           fill = "transparent")
  
# Map of MRC sites -----

# Map of MRC sites
data_mrc <- fread(here::here(data_dir, "MRC/mrc_water_quality_clean.csv"))
data_mrc <- data_mrc[grepl("Backprea", dataset) |
                       #grepl("Kompong Luong", dataset) | 
                       grepl("Prek Kdam", dataset) | 
                       grepl("Chhnang", dataset)]
data_mrc[, region := ifelse(env == "river_tonlesap", "Tonle Sap River", ifelse(
  env == "river_tributary", "Tributary", ifelse(
    env == "river_mekong", "Mekong", region)
))]
mrc_map_data <- unique(data_mrc[, .(dataset, env, region, lon, lat)])
mrc_map_data <- st_as_sf(mrc_map_data, coords = c("lon", "lat"), crs = "EPSG:4326")
mrc_map_data <- st_transform(mrc_map_data, st_crs(rivers_clipped))



rivers_clipped2 <- st_intersection(rivers, st_as_sfc(st_bbox(c(xmin = 268000, ymin = 1270000, xmax = 580000, ymax = 1508000), crs = st_crs(rivers))))
mrc_map <- ggplot() + 
  scale_y_continuous(expand = c(0,0), limits = c(1270000, 1508000)) +
  scale_x_continuous(expand = c(0, 0), limits = c(268000, 580000))  +
  coord_fixed() + 
  geom_raster(data = lake_level_df[lake_level_df$dur1997daily > 0,], 
              aes(x = x, y = y, fill = dur1997daily))  +
  geom_sf(data = rivers_clipped2, aes(linewidth = Strahler, alpha = Strahler), color = "#1d03b2") +
  scale_linewidth(range = c(.5, 1), guide = "none") + 
  scale_alpha(guide = "none") +
  scale_alpha(guide = "none") +
  geom_raster(data = lake_level_df[lake_level_df$dur1997daily >= 360,],
              aes(x = x, y = y), fill = "#1d03b2") +
  geom_sf(data= mrc_map_data, shape = 21, color = "black", fill = "#e0b233", size = 4) +
   # annotate("text", x = 492584.9,  y= 1277321, label = "Phnom Penh", color = "gray40", nudge_x = 10000)+ 
  #annotate("text", x = 480000,  y= 1274000, label = "Phnom Penh", color = "gray40")+ 
  annotate("point", x = 492584.9,  y= 1277321, color = "gray40", size = 3)+ 
  annotate("text", x = 492584.9-30000,  y= 1277321, label = "Phnom Penh", color = "gray40")+ 
  geom_label_repel(data= st_transform(mrc_map_data, "EPSG:3857"), 
                  aes(label = region, geometry = geometry), 
                  stat = "sf_coordinates") +
  scale_fill_continuous(name = "Inundation time",
                        high = "#013896", 
                        low = "#b6d8ba", 
                        breaks = c(3, 6, 9) * 30,
                        labels = c("3 months", "6 months", "9 months"), 
                        limits = c(1, 365)) +
  theme(#panel.background = element_rect(fill = 'lightblue'), 
        axis.title.x = element_blank(),  # Remove x-axis label
        axis.title.y = element_blank(),  # Remove y-axis label
        axis.text.x = element_blank(),   # Remove x-axis tick labels
        axis.text.y = element_blank(),   # Remove y-axis tick labels
        axis.ticks.x = element_blank(),  # Remove x-axis ticks
        axis.ticks.y = element_blank(),   # Remove y-axis ticks
        panel.grid = element_blank(),
        legend.position = "none"
  ) 

lake_map <- ggplot() + 
  scale_y_continuous(expand = c(0,0), limits = c(1270000, 1508000)) +
  scale_x_continuous(expand = c(0, 0), limits = c(268000, 580000))  +
  coord_fixed() + 
  geom_raster(data = lake_level_df[lake_level_df$dur1997daily > 0,], 
              aes(x = x, y = y, fill = dur1997daily))  +
  geom_sf(data = rivers_clipped2, aes(linewidth = Strahler, alpha = Strahler), color = "#1d03b2") +
  scale_linewidth(range = c(.5, 1), guide = "none") + 
  scale_alpha(guide = "none") +
  scale_alpha(guide = "none") +
  geom_raster(data = lake_level_df[lake_level_df$dur1997daily >= 360,],
              aes(x = x, y = y), fill = "#1d03b2") +
  theme(#panel.background = element_rect(fill = 'lightblue'), 
    axis.title.x = element_blank(),  # Remove x-axis label
    axis.title.y = element_blank(),  # Remove y-axis label
    axis.text.x = element_blank(),   # Remove x-axis tick labels
    axis.text.y = element_blank(),   # Remove y-axis tick labels
    axis.ticks.x = element_blank(),  # Remove x-axis ticks
    axis.ticks.y = element_blank(),   # Remove y-axis ticks
    panel.grid = element_blank(),
    legend.position = "none"
  ) +
  scale_fill_continuous(name = "Inundation time",
                        high = "#013896", 
                        low = "#b6d8ba", 
                        breaks = c(3, 6, 9) * 30,
                        labels = c("3 months", "6 months", "9 months"), 
                        limits = c(1, 365)) 


png(here::here(output_dir, "map_datasets.png"), width = 8, height = 6, 
    units = "in", res = 240)
print(main_map)
dev.off()

png(here::here(output_dir, "context_map.png"), width = 3, height = 3, 
     units = "in", res = 240)
print(context_map)
dev.off()

png(here::here(output_dir, "mrc_site_map.png"), width = 5, height = 4, units = "in", res = 240)
print(mrc_map)
dev.off()


# # get water
# water <- read_sf("C:/github/lmb-metabolism-sensing/data/mekong.geojson")


# 
# library(tmap)
# tm_shape(lake_level) + tm_raster(col.scale = tm_scale_continuous(values = c("#b6d8ba",  "#013896"))) +
#   tm_shape(data_sf[data_sf$env != "river" & data_sf$region != "",]) +
#   tm_symbols(size = .3,
#              shape = "dataset",
#              fill = "dataset",
#              col = "dataset",
#              col.scale = tm_scale(values = c("#1b9e77","#66a61e", "#a6761d",  "#e7298a", "#d95f02", "#e6ab02")),
#              fill.scale = tm_scale(values = c("#1b9e77","#66a61e", "#a6761d",  "#e7298a", "#d95f02", "#e6ab02")),
#              shape.scale = tm_scale(values = c(16, 15, 17, 18, 3, 8)),
#              #col.legend = tm_legend_combine("fill"),
#              shape.legend = tm_legend_combine("fill")
#              )+
#   tm_compass() +
#   tm_scalebar()

# 
# basemap +
#   geom_sf(data = data_sf, mapping = aes(color = inundation)) +
#   tsl_theme+ 
#   theme(
#     axis.title.x = element_blank(),  # Remove x-axis label
#     axis.title.y = element_blank(),  # Remove y-axis label
#     axis.text.x = element_blank(),   # Remove x-axis tick labels
#     axis.text.y = element_blank(),   # Remove y-axis tick labels
#     axis.ticks.x = element_blank(),  # Remove x-axis ticks
#     axis.ticks.y = element_blank()   # Remove y-axis ticks
#   )  + 
#   scale_color_binned(type = "viridis",breaks = c(0, 100, 200, 300))


# 
# environ_map <-  basemap +
#   tsl_theme+ 
#   theme(
#     axis.title.x = element_blank(),  # Remove x-axis label
#     axis.title.y = element_blank(),  # Remove y-axis label
#     axis.text.x = element_blank(),   # Remove x-axis tick labels
#     axis.text.y = element_blank(),   # Remove y-axis tick labels
#     axis.ticks.x = element_blank(),  # Remove x-axis ticks
#     axis.ticks.y = element_blank()   # Remove y-axis ticks
#   ) + 
#   data_colors_dark
# 
# png(here::here(output_dir, "map_env.png"), width = 8, height = 6, 
#     units = "in", res = 240)
# environ_map
# dev.off()
# 
# 

