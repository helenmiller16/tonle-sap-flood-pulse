library(pdftools)
library(data.table)
data_dir <- "00_data"
d <- pdf_text(here::here(data_dir, "burnett_2017.pdf")) |> 
  strsplit("\n") |> 
  unlist()
  

# Units seem to match
# micromols for nutrient concentrations
# mg/L for Chl-a 
# uS/cm for conductivity
# separate by date 

jan_2013 <- 
  d[8:36]
  
jul_2013 <- 
  d[c(47, 58:85)]
  
jun_2014 <- 
  d[c(96:99, 110:129)]

nov_2013 <- 
  d[c(140:151, 162:186)]

jul_2013_tsr <- 
  d[c(197:203, 214:233)]


# just paste them together to clean -- date and lat/lon are given
data <- c(jan_2013, jul_2013, jun_2014, nov_2013 
          #,jul_2013_tsr # Don't include river data for now
          )

# remove space in latlon
# data <- sapply(data, \(x) gsub('°\\s', '°', x), USE.NAMES = F)

#list <- regmatches(data, gregexec('\\S+', data)) 
# lat and lon get split
# cleaned <- lapply(list, function(x) {
#   c(x[1], 
#     paste(x[2], x[3]), # lat
#     paste(x[4], x[5]), # lon
#     paste(x[6], x[7], x[8]), # date
#     x[9:length(x)])
# }) 

#l <- cleaned |> lapply(length)

#cleaned[which(l != 23)]


# 23 columns
names <- regmatches(d[4], gregexec('\\S+', d[4]))[[1]]


# get site names
sites <- gsub(" ", "", substr(data, 1, 5) )
# get dates (some don't include dates)
dates <- unlist(regmatches(data, gregexpr(".\\d [[:alpha:]]{3} \\d\\d", data)))
dates <- as.Date(dates, format = "%d %b %y")


# get data (excluding latlon, site and date)
vals <- lapply(data, \(x) {
  l <- gregexpr(".\\d [[:alpha:]]{3} \\d\\d", x)[[1]]+9
  txt <- substring(x, l)
}) |> unlist()
# remove accidental spaces between decimals
vals <- sapply(vals, \(x) gsub('\\s\\.', '.', x), USE.NAMES = F)

vals <- regmatches(vals, gregexec('\\S+', vals)) 
l <- lapply(vals, length)
# site 57 missing SS data
which(l != 19)
c(vals[[57]][1:6], NA, vals[[57]][7:length(vals[[57]])])
vals[[57]] <- c(vals[[57]][1:6], NA, vals[[57]][7:length(vals[[57]])])

df <- Reduce(rbind, vals) |> as.data.table()
colnames(df) <- names[5:length(names)]
df$site <- sites
df$date <- dates

# lat lon are going to be trickier... 
latlon <- lapply(data, \(x) {
  start <- 6
  end <- gregexpr(".\\d [[:alpha:]]{3} \\d\\d", x) [[1]]
  substr(x, start, end - 1)
})
# formats 
# 13° 14.876 # D° MM.MMM
# 13°12.963´# D°MM.MMM´
# 13°14' 11.3\" # D°MM' SS.S"

get_latlon <- function(txt) {
  # first split lat and lon
  split <- regexpr("\\d{3}°", txt) -1
  lat <- substring(txt, 1, split)
  lon <- substring(txt, split)
  lat_deg <- regmatches(lat, regexpr('\\d{2}', lat))
  lon_deg <- regmatches(lon, regexpr('\\d{3}', lon))
  
  if ( grepl('\\d{2}\\.\\d{2,}', txt) ) { 
    # D MM.MMM
    # get decimal minutes
    lat_minutes <- regmatches(lat, regexpr('\\d{2}\\.\\d{2,}', lat))
    lat_dec <- as.numeric(lat_deg) + as.numeric(lat_minutes)/60
    
    lon_minutes <- regmatches(lon, regexpr('\\d{2}\\.\\d.', lon))
    lon_dec <- as.numeric(lon_deg) + as.numeric(lon_minutes)/60
  } else {
    # D°MM' SS
    lat_minutes <- regmatches(lat, regexpr('\\d.\'', lat))
    lat_minutes <- sub("'", "", lat_minutes)
    lat_seconds <- regmatches(lat, regexpr('\\d.\\.\\d', lat))
    lat_dec <- as.numeric(lat_deg) + as.numeric(lat_minutes)/60 + as.numeric(lat_seconds)/3600
    
    lon_minutes <- regmatches(lon, regexpr('\\d.\'', lon))
    lon_minutes <- sub("'", "", lon_minutes)
    lon_seconds <- regmatches(lon, regexpr('\\d.\\.\\d', lon))
    lon_dec <- as.numeric(lon_deg) + as.numeric(lon_minutes)/60 + as.numeric(lon_seconds)/3600
    
  } 
  return(c(lat_dec, lon_dec))
}

latlon_mx <- t(sapply(latlon, get_latlon))
df$lat <- latlon_mx[, 1]
df$lon <- latlon_mx[, 2]
df$campaign <- strftime(df$date, "%m/%Y")

# library(sf)
# library(ggplot2)
# library(basemaps)

# data_sf <- st_as_sf(df, coords = c("lon", "lat"), 
#                     crs = "EPSG:4326")
# 
# if (file.exists(here::here(data_dir, "burnett_2017.geojson"))) {
#   file.remove(here::here(data_dir, "burnett_2017.geojson"))
# }
# write_sf(data_sf, here::here(data_dir, "burnett_2017.geojson"))
# data_plt <- st_transform(data_sf, "EPSG:3857")
# #bounds = st_bbox(st_buffer(data_plt, 10000))
# ggplot() + 
#   basemap_ggplot(ext = st_buffer(data_plt, 10000), map_service = "esri", map_type="natgeo_world_map") +
#   geom_sf(data = data_plt, mapping = aes(color = campaign)) + 
#   lims(x = c(bounds[1], bounds[3]), 
#        y = c(bounds[2], bounds[4])) +
#   tsl_theme + 
#   scale_color_discrete(type = c("#458a93", "#a3640d", "#ea8579", "#a77eb2"))

# fix names  
names(df)[c(1,2)] <- c("total_depth", "sample_depth")

# make sure all data is stored as numbers (not character)
for (name in names(df)[!names(df) %in% c("site", "date", "campaign")]) {
  df[, (name) := lapply(.SD, as.numeric), .SDcols = name] 
}

fwrite(df, here::here(data_dir, "burnett_2017.csv"))
