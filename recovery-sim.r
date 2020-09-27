library(dplyr)

data <- read.csv(file = "data.csv", stringsAsFactors = FALSE)

data$AnzahlFall[data$NeuerFall < 0] <- as.integer(0)
data$AnzahlGenesen[data$NeuGenesen < 0] <- as.integer(0)
data$AnzahlTodesfall[data$NeuerTodesfall < 0] <- as.integer(0)

aggregated <- aggregate(cbind(AnzahlFall, AnzahlGenesen + AnzahlTodesfall) ~ substr(data$Refdatum, 1, 10), data, sum)
colnames(aggregated) <- c("date", "Idaily", "Rdaily")
aggregated$date <- as.Date(aggregated$date, "%Y/%m/%d")

dates <- seq.Date(aggregated[1, "date"],  aggregated[nrow(aggregated), "date"], 1)
aggregated <- full_join(data.frame(date = dates), aggregated)
aggregated[is.na(aggregated)] <- as.integer(0)

aggregated$Isum <- cumsum(aggregated$Idaily)
aggregated$Rsum <- cumsum(aggregated$Rdaily)

aggregated$R <-
  ceiling(c(rep(0, 14), head(aggregated$Isum * 0.8, -14))) +
    floor(c(rep(0, 28), head(aggregated$Isum * 0.2, -28)))

view <- aggregated[(nrow(aggregated)-60):nrow(aggregated),]
plot(view$date, view$Isum, xlab = "Tage", ylab = "R", type = "l", lty = 2)
lines(view$date, view$Rsum, type = "l")
lines(view$date, view$R, col = "red")
