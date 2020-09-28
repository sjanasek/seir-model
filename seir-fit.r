library(deSolve)
library(dplyr)
library(ggplot2)

data_path <- "data.csv"
if (!file.exists(data_path)) {
  file_url <- "https://www.arcgis.com/sharing/rest/content/items/f10774f1c63e40168479a1feb6c7ca74/data"
  download.file(url = file_url, destfile = data_path, mode = "wb")
}

# In the following we're generating the "geilsten Daten Deutschlands". :)
data <- read.csv(file = data_path, stringsAsFactors = FALSE)

# Only observe Hamburg as rate of infection is fairly stable
data <- data[data$IdBundesland == 14,]

# If any of the "Neu" columns are <0 they indicate revisioned (now invalid) entries.
# "mask" them away so that they won't trouble us when we aggregate() the data below.
data$AnzahlFall[data$NeuerFall < 0] <- as.integer(0)

# Extract basic data summed up per date.
# We cannot make use of the recovered columns in the RKI dataset as we're interested in the amount of _currently_
# actively infected individuals, as opposed to total infected (which is the only thing the dataset provides).
aggregated <- aggregate(AnzahlFall ~ substr(data$Refdatum, 1, 10), data, sum)
colnames(aggregated) <- c("date", "Idaily")
aggregated$date <- as.Date(aggregated$date, "%Y/%m/%d")

# The dataset is sparse and has missing dates. -> Insert missing dates and fill them with zero values.
dates <- seq.Date(aggregated[1, "date"],  aggregated[nrow(aggregated), "date"], 1)
aggregated <- full_join(data.frame(date = dates), aggregated)
aggregated[is.na(aggregated)] <- as.integer(0)

# Generate and estimate columns based on the dataset.
aggregated$Isum <- cumsum(aggregated$Idaily)
# The recovery column is entirely estimated as the RKI doesn't have any data for that.
# "0.8"/"0.2" below were chosen as they approximately match up with the recovery rate the RKI estimates.
aggregated$R <-
  ceiling(c(rep(0, 14), head(aggregated$Isum * 0.8, -14))) +
    floor(c(rep(0, 28), head(aggregated$Isum * 0.2, -28)))
# Now that we have estimated "R" we can remove recovered individuals from the
# "Isum" column to finally calculate the _currently_ actively infected individuals.
aggregated$I <- aggregated$Isum - aggregated$R

seirsim <- function(input, S0, E0, I0, R0, beta, gamma, sigma) {
  out <- ode(
    times = seq_len(nrow(input)),
    y = c(S = S0, E = E0, I = I0, R = R0),
    parms = c(N = N, beta = beta, gamma = gamma, sigma = sigma, dtstep = 1),
    func = function(t, y, parms) {
      with(as.list(c(y, parms)), {
        dS <- dtstep * (-beta * ((S * I) / N))
        dE <- dtstep * (beta * ((S * I) / N) - sigma * E)
        dI <- dtstep * (sigma * E - gamma * I)
        dR <- dtstep * (gamma * I)
        list(c(dS, dE, dI, dR))
      })
    }
  )
  colnames(out)[1] <- "date"
  out <- as.data.frame(out)
  out$date <- input$date
  out
}

# same as seirsim, but rounds its output values to integers, since non-integer individuals don't exist.
seirsimvisual <- function(input, S0, E0, I0, R0, beta, gamma, sigma) {
  out <- seirsim(input, S0, E0, I0, R0, beta, gamma, sigma)
  out$S <- round(out$S)
  out$E <- round(out$E)
  out$I <- round(out$I)
  out$R <- round(out$R)
  out
}

seiroptim <- function(input, S0, E0, I0, R0, gamma, sigma) {
  optimized <- optim(
    method = "L-BFGS-B",
    control = list(parscale = 0.001, factr = 1),
    y = input$I,
    par = 0.5,
    lower = 0,
    upper = 1,
    fn = function(beta, y) {
      out <- seirsim(input, S0, E0, I0, R0, beta, gamma, sigma)
      sum((y - out[, "I"])^2)
    }
  )
  optimized$par
}

phases <- c(
  as.Date("2020-03-02"),
  # no lockdown
  as.Date("2020-03-31"),
  # lockdown
  as.Date("2020-05-07"),
  # no lockdown
  as.Date("2020-07-20"),
  # summer vacation
  as.Date("2020-08-28")
)

allphases <- aggregated[aggregated$date >= phases[1] & aggregated$date <= phases[length(phases)],]
N <- 4000000
R0 <- 0
I0 <- allphases[1, "I"]
E0 <- 20 * I0
S0 <- N - E0 - I0 - R0
gamma <- 0.33
sigma <- 0.19

simulated <- NULL
betas <- list()
for (i in 1:(length(phases) - 1)) {
  from <- phases[i]
  to <- phases[i + 1]
  beta <- betas[i]

  input <- aggregated[aggregated$date >= from & aggregated$date <= to,]
  beta <- seiroptim(input, S0, E0, I0, R0, gamma, sigma)
  s <- seirsimvisual(input, S0, E0, I0, R0, beta, gamma, sigma)

  simulated <- rbind(simulated, s)
  betas[[i]] <- beta

  # Starting parameters for the next iteration equal the final values for the current simulated run,
  # since the next simulated phase should pick up where the current one left off.
  R0 <- s[nrow(s), "R"]
  I0 <- s[nrow(s), "I"]
  E0 <- s[nrow(s), "E"]
  S0 <- N - E0 - I0 - R0
}

vlineDates <- data.frame(date = phases[2:(length(phases) - 1)])
vlines <- geom_vline(data = vlineDates, mapping = aes(xintercept = date), linetype = "dotted")

ggplot(mapping = aes(x = date, y = I)) +
  theme_minimal() +
  scale_linetype_manual(values = c("dashed", "solid")) +
  labs(x = "Zeit", linetype = NULL) +
  geom_line(data = allphases, mapping = aes(linetype = "Ist"), color = "#777777") +
  geom_line(data = simulated, mapping = aes(linetype = "Simuliert")) +
  vlines
