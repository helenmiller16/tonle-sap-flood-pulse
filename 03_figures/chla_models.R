library(data.table)
library(ggplot2)
library(patchwork)

figures_dir <- "03_figures"
source(here::here(figures_dir, "themes.R"))
output_dir <- "03_outputs"
data_dir <- "00_data"

preds_dissolved <- fread(here::here(output_dir, "chla_models/predictions_dissolved.csv"))
present_dissolved <- fread(here::here(output_dir, "chla_models/present_dissolved.csv"))
importance_dissolved <- fread(here::here(output_dir, "chla_models/variable_importance_dissolved.csv"))


preds_total <- fread(here::here(output_dir, "chla_models/predictions_total.csv"))
present_total <- fread(here::here(output_dir, "chla_models/present_total.csv"))
importance_total <- fread(here::here(output_dir, "chla_models/variable_importance_total.csv"))

names_dict <- c(
  "scale(DIN)" = "DIN", 
  "scale(DIP)" = "DIP", 
  "scale(DSi)" = "DSi", 
  "scale(TN)" = "TN", 
  "scale(TP)" = "TP", 
  "scale(TN):season" = "TN * season", 
  "scale(TP):season" = "TP * season",
  "scale(N.P)" = "N/P", 
  "region" = "region", 
  "dataset" = "dataset", 
  "season" = "season", 
  "scale(DIN):season" = "DIN * season", 
  "scale(DIP):season" = "DIP * season",
  "scale(DSi):season" = "DSi * season"
)
names_dict_inverse <- names(names_dict)
names(names_dict_inverse) <- names_dict


id <- importance_dissolved
pd <- present_dissolved

it <- importance_total
pt <- present_total

# make order match
# both match order of weights for total 

order_t <- names_dict[names_dict_inverse[it$predictor][order(it$weight)]]
order_d <- order_t
order_d <- gsub("TN", "DIN", order_d) |> 
  (\(x) gsub("TP", "DIP", x)) () 

## Plot parameter weights -----
importance_t <- ggplot(it) + 
  geom_bar(aes(x = predictor, y = weight), 
           stat = "identity", fill = "#828DA5", 
           color = ifelse(it$weight > 0.9, "#0248e0", "transparent"), 
           linewidth = 1) +
  geom_hline(aes(yintercept = 0.9)) + 
  lims(x = order_t) +
  tsl_theme +
  theme(panel.border = element_blank()) +
  labs(x = "", y = "Importance score") +
  scale_y_continuous(limits = c(0, 1.02), expand = c(0,0)) +
  annotation_custom(grid::textGrob("0.9"), xmin =0.2, xmax = 0.2, ymin = .9, ymax = .9) +
  coord_flip(clip = 'off') 

importance_d <- ggplot(id) + 
  geom_bar(aes(x = predictor, y = weight), 
           stat = "identity", fill = "#828DA5", 
           color = ifelse(id$weight > 0.9, "#0248e0", "transparent"), 
           linewidth = 1) +
  geom_hline(aes(yintercept = 0.9)) + 
  lims(x = order_d) +
  tsl_theme +
  theme(panel.border = element_blank()) +
  labs(x = "", y = "Importance score") +
  scale_y_continuous(limits = c(0, 1.02), expand = c(0,0)) +
  annotation_custom(grid::textGrob("0.9"), xmin =0.2, xmax = 0.2, ymin = .9, ymax = .9) +
  coord_flip(clip = 'off') 
importance <- importance_t + importance_d



## Plot top 10 models -----

#present <- present[value == TRUE]
p_top_t <- ggplot(pt) + 
  geom_tile(data = pt[value == TRUE], aes(x = model, y = variable), fill = "#828DA5")+
  # add grid
  geom_tile(aes(x = model, y = variable), color = "gray90", fill = "transparent") +
  tsl_theme +
  theme(panel.grid = element_blank(), 
        axis.text.x = element_blank(), 
        panel.border = element_blank()) +
  scale_y_discrete(limits = order_t) +
  labs(y = "Variables included",
       x = "Model")

## Plot Delta AIC -----
d <- unique(pt[, .(model, delta)])
p_daic_t <- ggplot(d) +
  geom_hline(aes(yintercept = 2), color = "gray80") + 
  geom_point(aes(x = as.numeric(model), y = delta), shape = 21, size = 5, 
             fill = "#505A70", color = ifelse(d$delta < 2, "#0248e0", "transparent"), 
             stroke = 2) +
  scale_y_reverse(limits= c(7,0)) +
  labs(y = "delta-AIC") +
  tsl_theme +
  scale_x_continuous(breaks = seq(1, 10), limits = c(0.5, 10.5), expand = c(0.01,0.01)) +
  #facet_grid(cols = vars(model)) +
  theme(axis.ticks.x = element_blank(), 
        axis.text.x = element_blank(),
        axis.title.x = element_blank(), 
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(), 
        panel.border = element_blank())+
  coord_trans(clip = "off")

p_top_d <- ggplot(pd) + 
  geom_tile(data = pd[value == TRUE], aes(x = model, y = variable), fill = "#828DA5")+
  # add grid
  geom_tile(aes(x = model, y = variable), color = "gray90", fill = "transparent") +
  tsl_theme +
  theme(panel.grid = element_blank(), 
        axis.text.x = element_blank(), 
        panel.border = element_blank()) +
  scale_y_discrete(limits = order_d) +
  labs(y = "Variables included",
       x = "Model")

## Plot Delta AIC -----
d <- unique(pd[, .(model, delta)])
p_daic_d <- ggplot(d) +
  geom_hline(aes(yintercept = 2), color = "gray80") + 
  geom_point(aes(x = as.numeric(model), y = delta),  shape = 21, size = 5, 
             fill = "#505A70", color = ifelse(d$delta < 2, "#0248e0", "transparent"), 
             stroke = 2) +
  scale_y_reverse(limits= c(7,0)) +
  labs(y = "delta-AIC") +
  tsl_theme +
  scale_x_continuous(breaks = seq(1, 10), limits = c(0.5, 10.5), expand = c(0.01,0.01)) +
  #facet_grid(cols = vars(model)) +
  theme(axis.ticks.x = element_blank(), 
        axis.text.x = element_blank(),
        axis.title.x = element_blank(), 
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(), 
        panel.border = element_blank()) +
  coord_trans(clip = "off")

#panel.grid.minor.x = element_blank()) 

## Make final layout -----

layout <- '
AB
AB
AB
AB
AB
AB
CD
CD
EF
EF
EF
EF
'

tag <- theme(
  plot.tag.location = "plot", 
  #plot.tag = element_text(hjust = -2, vjust = 1), 
  plot.margin = rep(unit(.3, "cm"), 4))
notag <- theme(
  plot.tag = element_blank()
)

p <- importance_t + ggtitle("Total nutrients") + tag + 
  importance_d +ggtitle("Dissolved nutrients") + tag+ 
  p_daic_t +ggtitle("")+ tag + (p_daic_d +ggtitle("")+ theme(axis.title.y = element_blank())+ tag) + 
  p_top_t +notag + (p_top_d+ theme(axis.title.y = element_blank()) + notag) + 
  #plot_spacer() +
  plot_layout(design = layout) #+ plot_annotation(tag_levels = "a") &
  # theme(plot.tag.position = "topleft", 
  #       plot.tag.location = "panel")

p <-  p + plot_annotation(tag_levels = "a", tag_suffix = ".") 


png(here::here(output_dir, "chl_model_selection.png"), width = 10, height = 9, units = "in", res = 240)
print(p)
dev.off()


# Prediciton plots ----- 
data <- fread(here::here(data_dir, "combined-data.csv"))
data[, dataset := factor(dataset, levels = c("burnett", 
                                             "holtgrieve", 
                                             "miller", 
                                             "heu", 
                                             "loken", 
                                             "yoshikawa"))]
data[, region := factor(region, levels = c("North", 
                                           "Central",
                                           "South"))]
data[, season := factor(season, levels = c("low", 
                                           "rising", 
                                           "high", 
                                           "falling"))]

data <- data[env == "pelagic" & depth == "surface", .(
  TN, 
  TP, 
  DIN, 
  DIP,
  chla, 
  DSi, 
  TN.TP, 
  region, 
  season, 
  dataset
)]


#preds_plot_d  <- 

preds_dissolved[, season := factor(season, levels = c("low", 
                                           "rising", 
                                           "high", 
                                           "falling"))]
preds_total[, region := factor(region, levels = c("North", 
                                                      "Central",
                                                      "South"))]
preds_total[, season := factor(season, levels = c("low", 
                                                      "rising", 
                                                      "high", 
                                                  "falling"))]

# remove missing levels from dataset in data
data$dataset <- as.character(data$dataset)
data_d <- data[dataset %in% preds_dissolved$dataset]
preds_plot_d <- ggplot(preds_dissolved) +
  geom_point(data = data_d, aes(x= DIP, y = chla, color = dataset)) + 
  geom_line(mapping = aes(x = DIP,  
                          y = predictions, 
                          group = dataset, 
                          color = dataset, 
                          linetype = dataset),
            linewidth = 1) + 
  # 95% CI
  geom_ribbon(mapping = aes(x = DIP, 
                            ymin = q1,
                            ymax = q2,
                            group = dataset, 
                            fill = dataset, 
                            color = dataset), 
              alpha = 0.1) +
  scale_color_discrete(type = dataset_colors[unique(preds_dissolved$dataset)]) +
  scale_fill_discrete(type = dataset_colors[unique(preds_dissolved$dataset)])+
  #facet_grid(rows = vars(LOCATION), 
  #           cols = vars(as.factor(UNDERWATER_d))) +
  facet_wrap(~season) +
  labs(x = "DIP (μM)", y = "Predicted Chlorophyll-a (μg/L)")+ 
  #scale_color_continuous(name = "DSi (observed)") +
  #scale_size_continuous(name = "DSi (observed)") +
  guides(color= guide_legend(), size=guide_legend()) +
  scale_y_log10() + 
  coord_cartesian(ylim = c(1, 160))+
  tsl_theme


preds_plot_t <- ggplot(preds_total) +
  geom_point(data = data, 
             aes(x= TP, y = chla, color = region)) + 
  geom_line(mapping = aes(x = TP, 
                          group = region, 
                          color = region, 
                          linetype = region,
                          y = predictions),
            linewidth = 1) + 
  # 95% CI
  geom_ribbon(mapping = aes(x = TP,
                            ymin = q1,
                            ymax = q2,
                            group = region,
                            fill = region,
                            color = region),
              alpha = 0.1) +
  #facet_grid(rows = vars(LOCATION), 
  #           cols = vars(as.factor(UNDERWATER_d))) +
  facet_wrap(~season) +
  labs(x = "TP (μM)", y = "Predicted Chlorophyll-a (μg/L)")+ 
  scale_color_discrete("Region", type = region_colors[c("North", "Central", "South")]) + 
  scale_fill_discrete("Region", type = region_colors[c("North", "Central", "South")]) +
  scale_linetype_discrete("Region") +
  scale_y_log10() + 
  coord_cartesian(ylim = c(1, 160))+
  #xlim(c(0, 5)) + 
  tsl_theme 

preds_plot <- preds_plot_t / preds_plot_d + plot_annotation(tag_levels = "a", tag_suffix = ".") 

png(here::here(output_dir, "chl_predictions.png"), 
    width = 12, height = 10, units = "in", res = 150)
print(preds_plot)
dev.off()
