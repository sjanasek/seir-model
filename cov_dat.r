data_path <- paste0("data_", Sys.Date(), ".csv")
if (!file.exists(data_path)) {
  file_url <- "https://www.arcgis.com/sharing/rest/content/items/f10774f1c63e40168479a1feb6c7ca74/data"
  download.file(url = file_url, destfile = data_path, mode = "wb")
}

data <- read.csv(file = data_path)
filtered <- subset(data, Refdatum >= "2020/03/02 00:00:00" & NeuerFall >= 0)
aggregated <- aggregate(AnzahlFall ~ Refdatum, filtered, sum)
aggregated$AnzahlFall <- cumsum(aggregated$AnzahlFall)
plot(seq_len(nrow(aggregated)), aggregated$AnzahlFall)
