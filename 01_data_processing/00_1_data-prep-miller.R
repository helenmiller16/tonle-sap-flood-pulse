library(data.table)

data_dir <- "00_data"
data <- fread(here::here(data_dir, "Helen_TSL-updated.csv"))
tsl <- unique(data[!is.na(CLASS_Z) & CLASS_Z != "Sediment"])

# For measurements with duplicates, use median
tsl[, `:=`(PCO2_uatm_med = median(PCO2_uatm), 
           PCH4_uatm_med = median(PCH4_uatm), 
           D13C_DIC_med = median(D13C_DIC), 
           D13C_CO2_med = median(D13C_CO2), 
           D13C_CH4_med = median(D13C_CH4), 
           D18O_O2_med = median(D18O_O2), 
           O2_Ar_med = median(O2_Ar)), .(SITE, CLASS_Z, STAGE)]
tsl <- unique(tsl[, 
                  .(SITE, 
                    DATE, 
                    STAGE, 
                    CLASS_Z, 
                    TRANSECT_POINT, 
                    latitude = NORTHING, 
                    longitude = EASTING, 
                    ENVIRON,
                    UNDERWATER_d,
                    DIST_FPL_m, 
                    LOCATION, 
                    DEPTH_m, 
                    WATER_TC, 
                    WATER_TK, 
                    COND, 
                    PH, 
                    ORP, 
                    REDOX_ORP, 
                    CHL_mgL, 
                    O2_PERCENT, 
                    O2_mgL, 
                    PCO2_uatm = PCO2_uatm_med, 
                    PCH4_uatm = PCH4_uatm_med,
                    D13C_DIC= D13C_DIC_med, 
                    D13C_CH4= D13C_CH4_med, 
                    D13C_CO2 = D13C_CO2_med, 
                    D18O_O2 = D18O_O2_med, 
                    O2_Ar = O2_Ar_med,
                    PHOSPHATE_uM, 
                    SILICA_uM, 
                    NITRATE_uM, 
                    NITRITE_uM, 
                    AMMONIUM_uM)])
# Fix data entry error
tsl[longitude < 101.5, longitude := 104.1413]

# Make sure that we have high flood stage as default
tsl[, STAGE:=factor(STAGE, levels = c("High", "Falling"))]
tsl[, ENVIRON:=factor(ENVIRON, levels=c("Pelagic", "Edge", "Floodplain"))]

# Add binary variables for each environ
tsl[, EDGE:=ifelse(ENVIRON=="Edge", 1, 0)]
tsl[, FPL:=ifelse(ENVIRON=="Floodplain", 1, 0)]
tsl[, OPEN:=ifelse(ENVIRON=="Pelagic", 1, 0)]
tsl[, CLASS_Z:=factor(CLASS_Z, levels = c("Surface", "Bottom"))]
tsl[, DIST_FPL_k:=DIST_FPL_m/1000]

# Add DIN
tsl[, DIN := NITRATE_uM + NITRITE_uM + AMMONIUM_uM]

# Add ratios
tsl[, N.P := DIN/PHOSPHATE_uM]
tsl[, N.S := DIN/SILICA_uM]
tsl[, P.S := PHOSPHATE_uM/SILICA_uM]
tsl[N.P == Inf, N.P := NA]

# Change order of location
tsl$LOCATION <- factor(tsl$LOCATION, levels = c("Northwest", "Central", "Southwest"))


# Write to csv
fwrite(tsl, here::here(data_dir, "tsl-nutrients.csv"))
# geemap cannot handle "." in names
site_info <- unique(tsl[, .(SITE, 
                            TRANSECT_POINT, 
                            latitude, 
                            longitude, 
                            ENVIRON,
                            UNDERWATER_d,
                            DIST_FPL_m, 
                            LOCATION)])

fwrite(site_info, here::here(data_dir, "tsl_sites.csv"))
