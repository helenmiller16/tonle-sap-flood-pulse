# Setup -----

library(data.table)
library(ggplot2)
library(GGally)
library(patchwork)
tsl <- fread(here::here("00_data/tsl-nutrients.csv"))
# Set factor levels
tsl$LOCATION <- factor(tsl$LOCATION, levels = c("Northwest", "Central", "Southwest"))
tsl$Si.N <- 1/tsl$N.S
tsl$Si.P <- 1/tsl$P.S

# Themes and colors
figures_dir <- "03_figures"
output_dir <- "03_outputs"
source(here::here(figures_dir, "themes.R"))

data <- fread(here::here("00_data/combined-data.csv"))
data$season <- factor(data$season, levels = c("low", "rising", "high", "falling"))

data_long <- melt(data[env == "pelagic" & depth == "surface"], 
                  id.vars = c("dataset", "region", "season"),
                  measure.vars = c("DIP", "DIN", "TP", "TN", "NO3", "DSi", "chla", "DO", "cond", "DIN.DIP", "TN.TP"))


plot_values <- function(v, legend= FALSE, units = "μM", ylab = v, xlab = FALSE, xax = FALSE) {
  p <- ggplot(data_long[variable == v]) + 
    geom_boxplot(aes(x = season, y = value), outliers = FALSE) + 
    geom_jitter(aes(x = season, y = value, color = region, shape = region), 
                alpha = 0.6, 
                width = 0.1) + 
    scale_y_continuous(sprintf("%s (%s)", ylab, units)) + 
    xlab("Season") +
    tsl_theme + 
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = ifelse(legend, "right", "none")) +
    scale_color_discrete("Region", type = region_colors[c("North", "Central", "South")]) +
    scale_shape_discrete("Region")
  if (!xlab) {p = p + theme(axis.title.x = element_blank())}
  if (!xax) {p = p + theme(axis.text.x = element_blank())}
  p
}

png(here::here(output_dir, "data_summary_region.png"), 
    width = 8, height = 6, units = "in", res = 240)
print((plot_values("DIP")  + plot_values("DIN") + plot_values("DIN.DIP", ylab = "DIN/DIP")) /
  (plot_values("TP") + plot_values("TN")  + plot_values("TN.TP", ylab = "TN/TP", legend = TRUE) ) /
  (plot_values("DSi", xax = TRUE) + 
     plot_values("DO", units = "mg/L", xlab = TRUE, xax = TRUE) + 
     plot_values("chla", units = "μg/L", xax = TRUE)))
dev.off()


# Same thing but points are colored by dataset
plot_values <- function(v, legend= FALSE, units = "μM", ylab = v, xlab = FALSE, xax = FALSE) {
  p <- ggplot(data_long[variable == v]) + 
    geom_boxplot(aes(x = season, y = value), outliers = FALSE) + 
    geom_jitter(aes(x = season, y = value, color = dataset, shape = dataset), 
                alpha = 0.6, 
                width = 0.1) + 
    scale_y_continuous(sprintf("%s (%s)", ylab, units)) + 
    xlab("Season") +
    tsl_theme + 
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = ifelse(legend, "right", "none")) +
    scale_color_discrete("Dataset", type = dataset_colors) +
    scale_shape_discrete("Dataset")
  if (!xlab) {p = p + theme(axis.title.x = element_blank())}
  if (!xax) {p = p + theme(axis.text.x = element_blank())}
  p
}
png(here::here(output_dir, "data_summary_dataset.png"), 
    width = 12, height = 9, units = "in", res = 120)
print((plot_values("DIP")  + plot_values("DIN") + plot_values("DIN.DIP", ylab = "DIN/DIP")) /
        (plot_values("TP") + plot_values("TN")  + plot_values("TN.TP", ylab = "TN/TP", legend = TRUE) ) /
        (plot_values("DSi", xax = TRUE) + 
           plot_values("DO", units = "mg/L", xlab = TRUE, xax = TRUE) + 
           plot_values("chla", units = "μg/L", xax = TRUE)))
dev.off()


plot_by_dataset <- function(v, legend= FALSE, units = "μM", ylab = v, xlab = FALSE, xax = FALSE) {
  p <- ggplot(data_long[variable == v]) + 
    geom_boxplot(aes(x = dataset, y = value), outliers = FALSE) + 
    geom_jitter(aes(x = dataset, y = value, color = season, shape = season), 
                alpha = 0.6, 
                width = 0.1) + 
    scale_y_continuous(sprintf("%s (%s)", ylab, units)) + 
    xlab("Season") +
    tsl_theme + 
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = ifelse(legend, "right", "none"), 
          axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
    scale_color_discrete("Season", type = season_colors) +
    scale_shape_discrete("Season")
  if (!xlab) {p = p + theme(axis.title.x = element_blank())}
  if (!xax) {p = p + theme(axis.text.x = element_blank())}
  p
}
png(here::here(output_dir, "data_summary_by_dataset.png"), 
    width = 12, height = 9, units = "in", res = 120)
print((plot_by_dataset("DIP")  + plot_by_dataset("DIN") + plot_by_dataset("DIN.DIP", ylab = "DIN/DIP")) /
        (plot_by_dataset("TP") + plot_by_dataset("TN")  + plot_by_dataset("TN.TP", ylab = "TN/TP", legend = TRUE) ) /
        (plot_by_dataset("DSi", xax = TRUE) + 
           plot_by_dataset("DO", units = "mg/L", xlab = TRUE, xax = TRUE) + 
           plot_by_dataset("chla", units = "μg/L", xax = TRUE)))
dev.off()


if (FALSE) { # Transect data only
  ##### PCA #####
  data <- na.omit(tsl[, .(NITRATE_uM, NITRITE_uM, AMMONIUM_uM, SILICA_uM, PHOSPHATE_uM, CHL_mgL, LOCATION, STAGE, ENVIRON, CLASS_Z)])
  pca_nutrients <- prcomp( ~ NITRATE_uM + NITRITE_uM + AMMONIUM_uM + SILICA_uM + PHOSPHATE_uM, 
                           data = data, 
                           scale. = TRUE)
  plot_pca <- function(prc, 
                       x = "PC1", 
                       y = "PC2", 
                       color = data$CHL_mgL, 
                       size = data$CHL_mgL) {
    lims <- 
      max(c(
        abs(min(c(pca_nutrients$x[, x]/7, pca_nutrients$rotation[, x]), 
                c(pca_nutrients$x[, y]/7, pca_nutrients$rotation[, y]), 
                c(pca_nutrients$x[, x]/7, pca_nutrients$rotation[, x]), 
                c(pca_nutrients$x[, y]/7, pca_nutrients$rotation[, y]))), 
        max(c(pca_nutrients$x[, x]/7, pca_nutrients$rotation[, x]), 
            c(pca_nutrients$x[, y]/7, pca_nutrients$rotation[, y]), 
            c(pca_nutrients$x[, x]/7, pca_nutrients$rotation[, x]), 
            c(pca_nutrients$x[, y]/7, pca_nutrients$rotation[, y]))
      ))
    
    ggplot(pca_nutrients$x) +
      geom_point(aes(x = .data[[x]]/7, y = .data[[y]]/7,
                     col = color, 
                     size = size
      ) ) + 
      geom_segment(aes(x = 0, y = 0, 
                       xend = .data[[x]], 
                       yend = .data[[y]]), data= pca_nutrients$rotation, 
                   arrow = arrow(length=unit(0.2, 'cm')), 
                   color = "red", 
                   lwd = .2) +
      geom_text(aes(x = .data[[x]], y = .data[[y]], 
                    label = rownames(pca_nutrients$rotation)), 
                data= pca_nutrients$rotation, 
                color = "red", 
                cex = 2.5, 
                vjust = 1.5) +
      coord_fixed()+
      lims(x = c(-lims, lims),
           y = c(-lims, lims)) +
      labs(x = "PC1", y = "PC2", 
           color = "", size = "") +
      tsl_theme 
  }
  
  png(here::here(output_dir, 'PCA.png'), width = 16, height = 12, units = "in", res = 120)
  (
    plot_pca(pca_nutrients) +ggtitle("Colored by Chl-a") + 
      plot_pca(pca_nutrients, color = data$LOCATION, size = 1) +data_colors_light + guides(size = "none") +ggtitle("Location") +
      plot_pca(pca_nutrients, color = data$STAGE, size = 1) + guides(size = "none") + ggtitle("Stage")+
      data_colors_light
  ) / (
    plot_spacer() + 
      plot_pca(pca_nutrients, color = data$CLASS_Z, size = 1) + guides(size = "none") + ggtitle("depth")+
      data_colors_light +
      plot_pca(pca_nutrients, color = data$ENVIRON, size = 1) + guides(size = "none")+ ggtitle("Environment") + 
      data_colors_light
    
  )
  dev.off()
  
  
  
  ##### Correlation plots #######
  
  png(here::here(output_dir, "cor_1.png"), width = 10, height = 7, units = "in", res = 120)
  ggpairs(tsl[, .(
    COND, PH, ORP, O2_PERCENT, PCO2_uatm, PCH4_uatm, D13C_DIC, D13C_CH4, D13C_CO2
  )])
  dev.off()
  
  png(here::here(output_dir, "cor_nutrients.png"), width = 10, height = 7, units = "in", res = 120)
  ggpairs(tsl[, .(
    CHL_mgL, O2_Ar, SILICA_uM, PHOSPHATE_uM, NITRATE_uM, NITRITE_uM, AMMONIUM_uM, DIN
  )])
  dev.off()
  
  png(here::here(output_dir, "cor_model.png"), width = 10, height = 7, units = "in", res = 120)
  ggpairs(tsl[, .(
    LOCATION, STAGE, CLASS_Z, UNDERWATER_d, CHL_mgL, SILICA_uM, PHOSPHATE_uM, DIN, N.P
  )])
  dev.off()
  
  ##### facet plots #####
  
  # variables to plot
  vars <- c("CHL_mgL", "O2_Ar", "PHOSPHATE_uM", "NITRATE_uM", "NITRITE_uM", "AMMONIUM_uM", "SILICA_uM", "N.P", "N.S", "P.S")
  
  # facet by stage and color by location
  if (!dir.exists(here::here(output_dir, "facet_stage"))) {
    dir.create(here::here(output_dir, "facet_stage"))
  }
  for (var in vars) {
    png(paste0(here::here(output_dir, "facet_stage"), "/facet_stage-", var, ".png"), 
        width = 6, height = 4, units = "in", res = 120)
    
    p <- ggplot(tsl) + 
      geom_point(aes_string(x = "UNDERWATER_d", y = var, color = "LOCATION", shape = "LOCATION"))+ 
      theme_minimal() + 
      labs(x = "Inundation Days", y = var) +
      tsl_theme + 
      data_colors_light +
      facet_wrap(~STAGE, ncol = 2) +
      ggtitle(var)
    print(p)
    
    dev.off()
  }
  
  # Facet by location and color by depth
  if (!dir.exists(here::here(output_dir, "facet_location"))) {
    dir.create(here::here(output_dir, "facet_location"))
  }
  for (var in vars) {
    png(paste0(here::here(output_dir, "facet_location"), "/facet_location-", var, ".png"), 
        width = 5, height = 5, units = "in", res = 120)
    
    p <- ggplot(tsl) + 
      geom_point(aes_string(x = "UNDERWATER_d", y = var, color = "CLASS_Z", shape = "CLASS_Z"))+ 
      labs(x = "Inundation Days", y = var) +
      tsl_theme + 
      data_colors_light +
      facet_wrap(~LOCATION, ncol = 1) +
      ggtitle(var)
    print(p)
    
    dev.off()
  }
  
  # Box plots: by stage, location, and depth
  tsl_box<- function(x, y, xlab = F, ylab = F) {
    g <- ggplot(tsl)  + 
      geom_boxplot(aes_string(x = x, y = y), 
                   fill= theme_colors['lightgreen'], 
                   color = theme_colors['darkgreen']) +
      geom_jitter(aes_string(x = x, y = y), 
                  width = 0.2, 
                  color = theme_colors['darkgreen'], 
                  alpha = 0.5) +
      tsl_theme +
      ggtitle(y) +
      scale_y_log10()
    if (!xlab) {
      g <- g + xlab("")
    }
    if (!ylab) {
      g <- g + ylab("")
    }
    g
  }
  
  ## STAGE ##
  p <- function(y) {
    tsl_box(x="STAGE", y=y)
  }
  
  png(here::here(output_dir, "boxplots_stage.png"), width = 8, height = 8, units = "in", res = 120)
  plot <- ( p("CHL_mgL") + p( "PHOSPHATE_uM") + p("SILICA_uM") ) / 
    ( p( "NITRATE_uM") + p("NITRITE_uM") + p("AMMONIUM_uM") ) / 
    ( p("N.P") + p("N.S") + p("P.S"))
  plot + plot_annotation(
    title = "Variables by water stage"
  )
  dev.off()
  
  
  ## LOCATION ##
  p <- function(y) {
    tsl_box(x="LOCATION", y=y)
  }
  
  png(here::here(output_dir, "boxplots_location.png"), width = 8, height = 8, units = "in", res = 120)
  plot <- ( p("CHL_mgL") + p( "PHOSPHATE_uM") + p("SILICA_uM") ) / 
    ( p( "NITRATE_uM") + p("NITRITE_uM") + p("AMMONIUM_uM") ) / 
    ( p("N.P") + p("N.S") + p("P.S"))
  plot + plot_annotation(
    title = "Variables by location"
  )
  dev.off()
  
  ## DEPTH ##
  p <- function(y) {
    tsl_box(x="CLASS_Z", y=y)
  }
  
  png(here::here(output_dir, "boxplots_depth.png"), width = 8, height = 8, units = "in", res = 120)
  plot <- ( p("CHL_mgL") + p( "PHOSPHATE_uM") + p("SILICA_uM") ) / 
    ( p( "NITRATE_uM") + p("NITRITE_uM") + p("AMMONIUM_uM") ) / 
    ( p("N.P") + p("N.S") + p("P.S"))
  plot + plot_annotation(
    title = "Variables by Depth"
  )
  dev.off()
  
  ## ENVIRON ##
  p <- function(y) {
    tsl_box(x="ENVIRON", y=y)
  }
  
  png(here::here(output_dir, "boxplots_environ.png"), width = 8, height = 8, units = "in", res = 120)
  plot <- ( p("CHL_mgL") + p( "PHOSPHATE_uM") + p("SILICA_uM") ) / 
    ( p( "NITRATE_uM") + p("NITRITE_uM") + p("AMMONIUM_uM") ) / 
    ( p("N.P") + p("N.S") + p("P.S"))
  plot + plot_annotation(
    title = "Variables by Environment"
  )
  dev.off()
  
  # Data summary table
  effects <- c("LOCATION", "STAGE", "CLASS_Z", "ENVIRON")
  vars <- c("NITRATE_uM", "NITRITE_uM", "AMMONIUM_uM", "DIN", "SILICA_uM", "PHOSPHATE_uM", "CHL_mgL", 
            "N.P", "Si.N", "Si.P")
  summary <- melt(tsl, id.vars = effects, measure.vars = vars)
  summary[summary$value == Inf, "value"] <- NA
  
  # get mean, sd, N by stage, environment, depth
  summary <- summary[, .(mean = mean(value, na.rm = TRUE), sd = sd(value, na.rm = TRUE), N = sum(!is.na(value))), 
                     .(STAGE, ENVIRON, CLASS_Z, variable)]
  summary$summary <- paste(round(summary$mean, 1), "±", round(summary$sd, 1))
  
  summary <- dcast(summary, variable ~   STAGE + CLASS_Z + ENVIRON , value.var = c("summary", "N"))
  
  fwrite(summary, here::here(output_dir, 'data-summary.csv'))
  
}
