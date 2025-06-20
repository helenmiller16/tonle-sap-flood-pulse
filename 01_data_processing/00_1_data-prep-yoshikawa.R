library(data.table)
library(openxlsx)
library(docxtractr)

data_dir <- "00_data"
data <- read.xlsx(here::here(data_dir, "yoshikawa_2024.xlsx"))

# dry season Feb-Mar 2014
# assign to March 1
dry <- read.xlsx(here::here(data_dir, "yoshikawa_2024.xlsx"), 
                 sheet = "Dry_Surf", 
                 startRow = 2)
wet <- read.xlsx(here::here(data_dir, "yoshikawa_2024.xlsx"), 
                 sheet = "Rainy_Surf", 
                 startRow = 2)

setDT(dry)
setDT(wet)



# exclude summary rows
dry <- dry[Station %in% as.character(1:18)]
wet <- wet[Station %in% as.character(1:18)]
dry$X1 <- NULL
dry$`2014.Mar` <- NULL
wet$X1 <- NULL
wet$`2014.Oct` <- NULL

dry[, Station := as.numeric(Station)]
wet[, Station := as.numeric(Station)]
# rainy seaon Oct, 2014
# assign to mid-October (Oct 15)\
wet_date <- as.IDate("2014-10-15")
wet$date <- wet_date
# dry season sampling Feb/march 2014
# assign to March 1
dry_date <- as.IDate("2014-03-01")
dry$date <- dry_date


# coordinates from yoshikawa_2019
coords <- data.table(
  Station = 1:18, 
  deg_north = c(
    "N 12° 32' 04\"", 
    "N 12° 36' 07\"",
    "N 12° 40' 20\"",
    "N 12° 41' 32\"", 
    "N 12° 38' 00\"", 
    "N 12° 41' 00\"", 
    "N 12° 48' 00\"", 
    "N 12° 51' 00\"", 
    "N 12° 56' 00\"", 
    "N 12° 56' 00\"",
    "N 13° 00' 00\"",
    "N 13° 03' 60\"",
    "N 13° 03' 00\"",
    "N 13° 05' 60\"",
    "N 13° 09' 60\"",
    "N 13° 12' 00\"",
    "N 13° 16' 08\"",
    "N 13° 07' 60\""
  ), 
  deg_east = c(
    "E 104° 22' 59\"", 
    "E 104° 23' 57\"", 
    "E 104° 24' 29\"", 
    "E 104° 20' 54\"", 
    "E 104° 14' 00\"", 
    "E 104° 12' 00\"", 
    "E 104° 09' 29\"", 
    "E 104° 04' 60\"", 
    "E 104° 00' 00\"",
    "E 103° 51' 60\"", 
    "E 103° 57' 00\"",
    "E 104° 01' 60\"",
    "E 103° 47' 60\"",
    "E 103° 50' 60\"",
    "E 103° 56' 00\"",
    "E 103° 45' 60\"",
    "E 103° 49' 25\"",
    "E 103° 42' 00\""
    )
)
# convert to lat/lon
coords[, 
       `:=`(
         lat_deg = as.numeric(substring(deg_north, 3, 4)),
         lat_min = as.numeric(substring(deg_north, 7, 8)), 
         lat_sec = as.numeric(substring(deg_north, 11, 12)), 
         
         lon_deg = as.numeric(substring(deg_east, 3, 5)), 
         lon_min = as.numeric(substring(deg_east, 8, 9)), 
         lon_sec = as.numeric(substring(deg_east, 12, 13))
         )]
coords[, 
       `:=`(
         lat = lat_deg + lat_min/60 + lat_sec/3600, 
         lon = lon_deg + lon_min/60 + lon_sec/3600
       )]

# Pull DSi from 2020 publication
# Has Si in ug/L
si_wet <- docx_extract_tbl(read_docx(here::here(data_dir, "yoshikawa_2020_wet.docx")))
si <- si_wet[si_wet$Element == "Si", ]
si$Element <- NULL
si$DL <- NULL
si$Mean.SD <- NULL
si_wet_dt <- data.table("Si" = as.numeric(t(si)))
si_wet_dt$Station <- as.numeric(sub("L", "", rownames(t(si))))
si_wet_dt$date <- wet_date

si_dry <- docx_extract_tbl(read_docx(here::here(data_dir, "yoshikawa_2020_dry.docx")))
si <- si_dry[si_dry$Element == "Si", ]
si$Element <- NULL
si$DL <- NULL
si$Mean.SD <- NULL
si_dry_dt <- data.table("Si" = as.numeric(t(si)))
si_dry_dt$Station <- as.numeric(sub("L", "", rownames(t(si))))
si_dry_dt$date <- dry_date

si_dt <- rbind(si_wet_dt, si_dry_dt)

# convert to uMol
# ug/L * 1/1,000,000 g/ug * 1/(x g/Mol) * 1,000,000 uM/Mol

si_dt[, Si_uM := Si / 28.08550]


# Combine wet and dry
data <- rbind(wet, dry)
data <- merge(data, si_dt[, .(Station, date, Si_uM)])

# 17 and 18 are floodplain
data[, env := ifelse(Station < 17, "pelagic", "floodplain")]

data <- merge(data, coords[, .(Station, lat, lon)], by = "Station")
fwrite(data, here::here(data_dir, "yoshikawa_2024.csv"))
