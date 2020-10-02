library(dplyr)
library(ggplot2)

data <- read.csv(file = "data.csv", stringsAsFactors = FALSE)
data$AnzahlFall[data$NeuerFall < 0] <- as.integer(0)
data$AnzahlGenesen[data$NeuGenesen < 0] <- as.integer(0)
data$AnzahlTodesfall[data$NeuerTodesfall < 0] <- as.integer(0)

aggregated <- aggregate(cbind(AnzahlFall, AnzahlGenesen + AnzahlTodesfall) ~ substr(data$Refdatum, 1, 10), data, sum)
colnames(aggregated) <- c("date", "Idaily", "Rdaily")
aggregated$date <- as.Date(aggregated$date, "%Y/%m/%d")

dates <- seq.Date(aggregated[1, "date"], aggregated[nrow(aggregated), "date"], 1)
aggregated <- full_join(data.frame(date = dates), aggregated)
aggregated[is.na(aggregated)] <- as.integer(0)

aggregated$Isum <- cumsum(aggregated$Idaily)
aggregated$Rsum <- cumsum(aggregated$Rdaily)

aggregated$R <-
  ceiling(c(rep(0, 14), head(aggregated$Isum * 0.8, -14))) +
    floor(c(rep(0, 28), head(aggregated$Isum * 0.2, -28)))

view <- aggregated[(nrow(aggregated) - 60):nrow(aggregated),]

ggplot(data = view, mapping = aes(x = date)) +
  theme_minimal() +
  labs(x = "Zeit", y = "R") +
  scale_linetype_manual(values = c("22", "solid")) +
  geom_line(mapping = aes(y = Isum, linetype = "22"), show.legend = FALSE) +
  geom_line(mapping = aes(y = Rsum, linetype = "solid"), show.legend = FALSE) +
  geom_line(mapping = aes(y = R, linetype = "solid", color = "red"), show.legend = FALSE)
