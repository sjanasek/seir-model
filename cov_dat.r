data_path <- paste0("data_", Sys.Date(), ".csv")
if (!file.exists(data_path)) {
  file_url <- "https://www.arcgis.com/sharing/rest/content/items/f10774f1c63e40168479a1feb6c7ca74/data"
  download.file(url = file_url, destfile = data_path, mode = "wb")
}

data <- read.csv(file = data_path)
data$AnzahlFall[data$NeuerFall < 0] <- as.integer(0)
data$AnzahlTodesfall[data$NeuerTodesfall < 0] <- as.integer(0)
data$AnzahlGenesen[data$NeuGenesen < 0] <- as.integer(0)

filtered <- subset(data, Refdatum >= "2020/03/02 00:00:00")
aggregated <- aggregate(cbind(AnzahlFall, AnzahlTodesfall, AnzahlGenesen) ~ Meldedatum, filtered, sum)
aggregated$AnzahlFall <- cumsum(aggregated$AnzahlFall)
aggregated$AnzahlTodesfall <- cumsum(aggregated$AnzahlTodesfall)
aggregated$AnzahlGenesen <- cumsum(aggregated$AnzahlGenesen)
aggregated$AnzahlRecovered <- aggregated$AnzahlTodesfall + aggregated$AnzahlGenesen

recovered <- as.matrix(
  ceiling(c(rep(0, 14), head(aggregated$AnzahlFall * 0.8, -14))) +
    floor(c(rep(0, 28), head(aggregated$AnzahlFall * 0.2, -28)))
)

plot(seq_len(nrow(aggregated)), aggregated$AnzahlFall)
lines(seq_len(nrow(aggregated)), recovered, col = "orange")
lines(seq_len(nrow(aggregated)), aggregated$AnzahlRecovered, col = "red")
