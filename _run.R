# Run all analysis steps

# Clean data
source(here::here("01_data_processing/_run_data_processing.R"))

# Analysis
source(here::here("02_analysis/_run_analysis.R"))

# also make the map
source(here::here("03_figures/_run_figures.R"))
