Data Processing
================
Helen Miller
2025-03-20

``` r
knitr::opts_chunk$set(warning = FALSE)
library(data.table)
library(ggplot2)
source(here::here("03_figures/themes.R"))
```

Units are converted to micromolar (μM) for nutrients, micrograms per
liter (μg/L) for chl-a, milligrams per liter (mg/L) for DO, and
microsiemens per centimeter (μS/cm) for conductivity.

To convert nutrients from mg/L to uM:  
mg/L \* 1/1000 g/mg \* 1/(x g/Mol) \* 1,000,000 uM/Mol = mg/L \* 1000 /
molar mass

Molar mass conversions:

N = 14.0067 g/mol  
P = 30.9738 g/mol  
PO4 = 94.9714 g/mol  
Si = 28.08550 g/mol  
SiO2 (silica) = 60.084 g/mol

## Burnett

Input:

| Variable | Input units | Conversion equation | Output units |
|----------|-------------|---------------------|--------------|
| chl-a    | mg/L (?)    | =                   | mg/L         |
| cond     | μS/cm       | =                   | μS/cm        |
| TP       | μM          | =                   | μM           |
| DIP      | μM          | =                   | μM           |
| TN       | μM          | =                   | μM           |
| DIN      | μM          | =                   | μM           |
| DSi      | μM          | =                   | μM           |
| DO       | mg/L        | =                   | mg/L         |

``` r
bur <- fread(here::here("00_data/burnett_2017.csv"))
ggplot(melt(bur, id.vars = c("date", "site"), measure.vars = c("Chl-a", "Cond", "TP", "DIP", "TN", "DIN", "DSi", "DO"))) + 
  geom_histogram(aes(x = value)) + 
  facet_wrap(~variable, scale = "free") +
  tsl_theme
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](README_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

## Heu

P and N are approximately one order of magnitude higher than other
datasets. If I divide them by 10 numbers line up better… It doesn’t
dramatically change results either way.

| Variable | Input units (units reported in publication) | Conversion equation | Output units |
|----|----|----|----|
| chl-a | ug/L | = | ug/L |
| cond | μS/cm | = | μS/cm |
| TP | mg/L | (TP mg/L) \* 1000 / 30.973761 | μM |
| DIP |  |  |  |
| TN | mg/L | (TN mg/L) \* 1000 / 14.0067 | μM |
| DIN |  |  |  |
| DSi | mg/L | (DSi mg/L) \*1000 / 60.0843 | μM |
| DO | mg/L | = | mg/L |

``` r
heu <- fread(here::here("00_data/heu.csv"))
ggplot(melt(heu, id.vars = c("date", "site"), measure.vars = c("Chla", "P_uM", "N_uM", "Si_uM", "DO"))) + 
  geom_histogram(aes(x = value)) + 
  facet_wrap(~variable, scale = "free") +
  tsl_theme
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](README_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

## Holtgrieve

| Variable | Input units | Conversion equation | Output units |
|----|----|----|----|
| chl-a |  |  |  |
| cond |  |  |  |
| TP |  |  |  |
| DIP | μM PO4 | = | μM |
| TN |  |  |  |
| DIN | μM (`NO3.(uM)` + `NO2.(uM)` + `NH4.(uM))` | = | μM |
| DSi | Si(OH)4.(uM) | = | μM |
| DO |  |  |  |

``` r
hol <- fread(here::here("00_data/holtgrieve.csv"))
hol[, DIN := `NO3.(uM)` + `NO2.(uM)` + `NH4.(uM)`]
ggplot(melt(hol, measure.vars = c("PO4.(uM)", "DIN", "Si(OH)4.(uM)"))) + 
  geom_histogram(aes(x = value)) + 
  facet_wrap(~variable, scale = "free") +
  tsl_theme
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](README_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

## Loken

| Variable | Input units   | Conversion equation               | Output units |
|----------|---------------|-----------------------------------|--------------|
| chl-a    |               |                                   |              |
| cond     |               |                                   |              |
| TP       | ug/L TP       | (TP ug/L) / 30.973761             | μM           |
| DIP      | ppm phosphate | (phosphate ppm) \* 1000 / 94.9714 | μM           |
| TN       | ug/L TN       | (TN ug/L) / 14.0067               | μM           |
| DIN      | ug/L TDN      | (TDN ug/L) / 14.0067              | μM           |
| DSi      |               |                                   |              |
| DO       |               |                                   |              |

``` r
lok <- fread(here::here("00_data/loken_nutrients.csv"))
ggplot(melt(lok, measure.vars = c("DIP_uM", "DIN_uM", "TP_uM", "TN_uM"))) + 
  geom_histogram(aes(x = value)) + 
  facet_wrap(~variable, scale = "free") +
  tsl_theme
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](README_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

## Miller

| Variable | Input units | Conversion equation | Output units |
|----|----|----|----|
| chl-a | mg/L ? | = | ? |
| cond | microsiemens per cm ? | \* 10000 (I just did this to get order of magnitude to match…) |  |
| TP |  |  |  |
| DIP | uM phosphate | = | μM |
| TN |  |  |  |
| DIN | uM (Nitrate + nitrite + amonium) | = | μM |
| DSi | uM silica | = | μM |
| DO | mg/L | = | mg/L |

``` r
mil <-fread(here::here("00_data/tsl-nutrients.csv")) 
mil[, cond := COND * 10000]
ggplot(melt(mil, measure.vars = c("PHOSPHATE_uM", "DIN", "SILICA_uM", "CHL_mgL", "O2_mgL", 
                                  "cond"))) + 
  geom_histogram(aes(x = value)) + 
  facet_wrap(~variable, scale = "free") +
  tsl_theme
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](README_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

## Yoshikawa

|  |  |  |  |
|----|----|----|----|
| **Variable** | **Input Units** | **Conversion equation** | **Output units** |
| chla | ug/L (reported in mg m^-3) | = | ug/L |
| cond |  |  |  |
| TP |  |  |  |
| DIP | uM PO4 | = | μM |
| TN |  |  |  |
| DIN | uM (there is a column for `NO3+NO2.(uM)`, a column for `NH4.(uM)` and a column for `DIN` which is usually but not always equal to `NO3+NO2.(uM)` + `NH4.(uM)`\| | = | μM |
| DSi | μM | = | μM |
| DO | mg/L | = |  |

``` r
yos <- fread(here::here("00_data/yoshikawa_2024.csv"))
ggplot(melt(yos, measure.vars = c("PO4.(uM)", "DIN", "DP", "Si_uM", "Chl.a", "DO"))) + 
  geom_histogram(aes(x = value)) + 
  facet_wrap(~variable, scale = "free") +
  tsl_theme
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](README_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
knitr::include_graphics(here::here("03_outputs/data_summary_dataset.png"))
```

<img src="../03_outputs/data_summary_dataset.png" width="1440" />

``` r
knitr::include_graphics(here::here("03_outputs/data_summary_region.png"))
```

<img src="../03_outputs/data_summary_region.png" width="1440" />
