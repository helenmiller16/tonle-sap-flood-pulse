library(openxlsx)
library(data.table)
data_dir <- "00_data"

heu <- read.xlsx(here::here(data_dir, "Data_MS2_heu_2023.xlsx"), sheet = "data_tidy") |> data.table()

# mg/L -> uM/L

# mg/L * 1/1000 g/mg * 1/(x g/Mol) * 1,000,000 uM/Mol

heu$P_uM <- heu$TP * 1000 / 30.973761
heu$N_uM <- heu$TN * 1000 / 14.0067
heu$Si_uM <- heu$DSi * 1000 / 60.0843


# geometry based on trying to match the pdf map they gave us... 

lonlat_list <- list(c(104.45506270974708, 12.511239559256559),
     c(104.43580019259566, 12.515396242170919),
     c(104.42039358401412, 12.512840599034492),
     c(104.42210537599613, 12.525419380931293),
     c(104.44201809572269, 12.540500763296944),
     c(104.44261891054202, 12.526676196494943),
     c(104.46021420167972, 12.520140688688283),
     c(104.46244579958011, 12.525168017083558),
     c(104.44545132326175, 12.528435727993388),
     c(104.44347721742679, 12.515699800456357),
     c(104.48055607484866, 12.542092635488856),
     c(104.5079997712871, 12.568803382246257),
     c(104.47448046916723, 12.517368592678539),
     c(104.49284823650122, 12.509240783975656),
     c(104.44578093424141, 12.539705694496659),
     c(104.44887083902657, 12.514527593951918),
     c(104.45556563272774, 12.515365507046283),
     c(104.45463012865002, 12.51362932766621))

lonlat <- as.data.table(Reduce(rbind, lonlat_list))
names(lonlat) <- c("lon", "lat")
lonlat$site <- paste0("JS", 1:18)

heu <- merge(heu, lonlat)
write.csv(heu, here::here(data_dir, "heu.csv"))
