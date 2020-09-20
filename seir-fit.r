library(deSolve)
library(bbmle)

data_path <- paste0("data_", Sys.Date(), ".csv")
data <- read.csv(file = data_path, stringsAsFactors = FALSE)

# If any of the "Neu" columns are <0 they indicate revisioned (now invalid) entries.
# "mask" them away so that they won't trouble us when we aggregate() the data below.
data$AnzahlFall[data$NeuerFall < 0] <- as.integer(0)
data$AnzahlTodesfall[data$NeuerTodesfall < 0] <- as.integer(0)
data$AnzahlGenesen[data$NeuGenesen < 0] <- as.integer(0)

aggregated <- aggregate(cbind(AnzahlFall, AnzahlTodesfall, AnzahlGenesen) ~ Refdatum, subset(data, Refdatum >= "2020/03/02 00:00:00"), sum)

# cumulative infected+recoved sum
data_ir <- cumsum(aggregated$AnzahlFall)
# cumulative recovered estimate
# 0.8/0.2 were chosen in such a way that the last value equals the currently reported actual value from RKI.
data_r <- as.matrix(
  ceiling(c(rep(0, 14), head(data_ir * 0.8, -14))) +
    floor(c(rep(0, 28), head(data_ir * 0.2, -28)))
)
data_i <- data_ir - data_r
data_ir <- data_ir[30:130]
data_r <- data_r[30:130]
data_i <- data_i[30:130]
data_x_axis <- 0:(length(data_i) - 1)
N <- 83200000

seirgen <- function(S0, E0, I0, R0, beta, gamma) {
  ode(
    times = data_x_axis,
    y = c(S = S0, E = E0, I = I0, R = R0),
    parms = c(N = N, beta = beta, gamma = gamma, sigma = 1 / 5.5),
    method = "euler",
    func = function(t, y, parms) {
      S <- y[1]
      E <- y[2]
      I <- y[3]
      with(as.list(parms), {
        dS <- -beta * ((S * I) / N)
        dE <- beta * ((S * I) / N) - sigma * E
        dI <- sigma * E - gamma * I
        dR <- gamma * I
        list(c(dS, dE, dI, dR))
      })
    }
  )
}

fitting <- mle2(
  method = "L-BFGS-B",
  control = list(maxit = 1e6),
  start = list(
    logS0 = log(N - data_ir[1] - data_i[1] - data_r[1]),
    logE0 = log(max(1, data_ir[1])),
    logI0 = log(max(1, data_i[1])),
    logR0 = log(max(1, data_r[1])),
    lbeta = qlogis(0.2),
    lgamma = qlogis(0.2)
  ),
  lower = list(
    logS0 = 0,
    logE0 = 0,
    logI0 = 0,
    logR0 = 0,
    lbeta = qlogis(0),
    lgamma = qlogis(0)
  ),
  upper = list(
    logS0 = log(N),
    logE0 = log(N),
    logI0 = log(N),
    logR0 = log(N),
    lbeta = qlogis(1),
    lgamma = qlogis(0)
  ),
  minuslogl = function(logS0, logE0, logI0, logR0, lbeta, lgamma) {
    out <- seirgen(exp(logS0), exp(logE0), exp(logI0), exp(logR0), plogis(lbeta), plogis(lgamma))
    out_i <- out[, "I"]
    SD <- sqrt(sum((data_i - out_i)^2) / length(data_x_axis))
    -sum(dnorm(data_i, mean = out_i, sd = SD, log = TRUE))
  }
)

theta <- coef(fitting)
theta <- as.numeric(c(exp(theta[1:4]), plogis(theta[5:6])))
fitted <- seirgen(theta[1], theta[2], theta[3], theta[4], theta[5], theta[6])

plot(data_x_axis, data_i, xlim = c(0, nrow(fitted) - 1), ylim = c(0, 300000), xlab = "days")
lines(0:(nrow(fitted) - 1), fitted[, "I"])
lines(data_x_axis, data_r, col = "red")
lines(0:(nrow(fitted) - 1), fitted[, "R"], col = "orange")
