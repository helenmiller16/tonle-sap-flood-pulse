data_dir <- here::here("01_data_processing")

# data prep
files <- list.files(data_dir)
data_prep_files <- files[grepl("00_1", files)]
for (f in data_prep_files) {
  print(paste("-----", f, "-----"))
  source(here::here("01_data_processing", f))
}


# combine
source(here::here("01_data_processing", "00_2_data-prep-combine.R"))