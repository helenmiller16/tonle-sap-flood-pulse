library(ggplot2)
# Theme and colors -----
location_colors <- c("#47824c", "#c99246", "#a77eb2")
light_colors <- c( "#c59b96", "#c4e5e8", "#95aa8c", "#d1b186", "#9e6d7c","#ea8579" )
dark_colors <- c("#8c372e", "#458a93","#2c5619",  "#a3640d", "#7e5389", "#ea8579" )
data_colors_light <- scale_color_discrete(type = light_colors) 
data_colors_dark <- scale_color_discrete(type = dark_colors) 
# season_colors <- c("low" = "#eabd79", 
#                    "rising" = "#94a3c6", 
#                    "high" = "#b56c4f", 
#                    "falling" = "#47824c")
# season_colors_light <- c("low" = "#fadfb8", 
#                          "rising" = "#d1d8e7", 
#                          "high" = "#e0c2b6", 
#                          "falling" = "#ebd8eb")
tsl_theme <- theme_minimal() + 
  theme(panel.border = element_rect(fill = NA, color = "gray80"), 
        text = element_text(size=11))
inundation_time_labels <- c("300" = "Edge and open water \n (inundation days >= 300)", 
                             "100" = "Floodplain \n (inundation days < 300)")
theme_colors = c("lightgreen" = "#d0e0d2", 
                 'medgreen' = "#a8c49b", 
                 'darkgreen' = "#47824c", 
                 'darkdarkgreen' = "#2c5619")

scale_color_gradient_tsl <- function(low = '#194212', 
                                     high = '#cff756',
                                     ...) {
  scale_colour_gradient(
    low = low,
    high = high,
    ...
  )
}

dataset_colors <- c(
  burnett ="#493e14",
  heu =   "#7e5389",
  holtgrieve = "#456393",
  loken = "#afb24e",
  miller = "#ea8579",
  yoshikawa = "#2f7c58"
)

dataset_colors <- c(
  burnett = "#1b9e77",
    heu = "#66a61e", 
    holtgrieve = "#a6761d",  
    loken = "#e7298a", 
    miller = "#d95f02", 
    yoshikawa = "#e6ab02"
)

region_colors <- c(
  Tributary = "#308ca8",
  North = "#477f61", 
  Central = "#af9946", 
  South = "#55478e", 
  TSR = "#af4f97", 
  Mekong = "#e89f40"
)

season_colors <- c(
  "low" = "#8c372e", 
  "rising" = "#458a93", 
  "high" = "#2c5619", 
  "falling" = "#a3640d"
)
