library(deSolve)
library(bbmle)

data <- read.csv(file = "data_2020-09-19.csv")

# If any of the "Neu" columns are <0 they indicate revisioned (now invalid) entries.
# "mask" them away so that they won't trouble us when we aggregate() the data below.
data$AnzahlFall[data$NeuerFall < 0] <- as.integer(0)
data$AnzahlTodesfall[data$NeuerTodesfall < 0] <- as.integer(0)
data$AnzahlGenesen[data$NeuGenesen < 0] <- as.integer(0)

filtered <- subset(data, Refdatum >= "2020/03/02 00:00:00")
aggregated <- aggregate(cbind(AnzahlFall, AnzahlTodesfall, AnzahlGenesen) ~ Refdatum, filtered, sum)

# cumulative infected+recoved sum
data_r <- cumsum(aggregated$AnzahlTodesfall) + cumsum(aggregated$AnzahlGenesen)
data_ir <- cumsum(aggregated$AnzahlFall) + data_r
data_x_axis <- 0:(length(data_ir) - 1)
N <- 83200000

seirgen <- function(S0, E0, I0, beta, gamma) {
  ode(
    times = data_x_axis,
    y = c(S = S0, E = E0, I = I0, R = data_r[1]),
    parms = c(N = N, beta = beta, gamma = gamma, sigma = 1 / 5.5),
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
    logS0 = log(N - data_ir[1]),
    logE0 = log(data_ir[1]),
    logI0 = log(data_ir[1]),
    lbeta = qlogis(0.2),
    lgamma = qlogis(0.2)
  ),
  minuslogl = function(logS0, logE0, logI0, lbeta, lgamma) {
    out <- seirgen(exp(logS0), exp(logE0), exp(logI0), plogis(lbeta), plogis(lgamma))
    out_ir <- out[, "I"] + out[, "R"]
    SD <- sqrt(sum((data_ir - out_ir)^2) / length(data_x_axis))
    -sum(dnorm(data_ir, mean = out_ir, sd = SD, log = TRUE))
  }
)

theta <- coef(fitting)
theta <- as.numeric(c(exp(theta[1:3]), plogis(theta[4:5])))
fitted <- seirgen(theta[1], theta[2], theta[3], theta[4], theta[5])

plot(data_x_axis, data_ir, xlim = c(0, nrow(fitted) - 1), ylim = c(0, 500000), log = "", ylab = "infected+recovered", xlab = "days")
lines(0:(nrow(fitted) - 1), fitted[, "I"] + fitted[, "R"])
