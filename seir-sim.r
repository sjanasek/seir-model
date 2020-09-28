library(deSolve)

seirsim <- function(date, S0, E0, I0, R0, beta, gamma, sigma) {
  out <- ode(
    times = seq_len(length(date)),
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
  out$date <- date
  out
}

# same as seirsim, but rounds its output values to integers, since non-integer individuals don't exist.
seirsimvisual <- function(date, S0, E0, I0, R0, beta, gamma, sigma) {
  out <- seirsim(date, S0, E0, I0, R0, beta, gamma, sigma)
  out$S <- round(out$S)
  out$E <- round(out$E)
  out$I <- round(out$I)
  out$R <- round(out$R)
  out
}

types <- c("S", "E", "I", "R")
typeNames <- c(S = "Susceptible", E = "Exposed", I = "Infectious", R = "Recovered")
phases <- c(
  as.Date("2020-03-02"),
  # no lockdown
  as.Date("2020-04-30"),
  # lockdown
  as.Date("2020-06-30"),
  # no lockdown
  as.Date("2020-08-30"),
  # summer vacation
  as.Date("2020-10-30"),
  as.Date("2020-12-30")
)
betas <- c(
  0.756664,
  0.1775356,
  0.476604,
  0.1775356,
  0.756664
)

alldates <- seq.Date(phases[1], phases[length(phases)], 1)
N <- 4078000
R0 <- 0
I0 <- 14
E0 <- 20 * I0
S0 <- N - E0 - I0 - R0
gamma <- 0.33
sigma <- 0.19

simulateds <- list()
for (i in 1:(length(phases) - 1)) {
  from <- phases[i]
  to <- phases[i + 1]
  beta <- betas[i]

  date <- seq.Date(from, to, 1)
  print(data.frame(i = i, S0 = S0, E0 = E0, I0 = I0, R0 = R0, beta = beta, gamma = gamma, sigma = sigma))
  simulated <- seirsimvisual(date, S0, E0, I0, R0, beta, gamma, sigma)

  simulateds[[i]] <- simulated

  # Starting parameters for the next iteration equal the final values for the current simulated run,
  # since the next simulated phase should pick up where the current one left off.
  R0 <- simulated[nrow(simulated), "R"]
  I0 <- simulated[nrow(simulated), "I"]
  E0 <- simulated[nrow(simulated), "E"]
  S0 <- N - E0 - I0 - R0
}

for (type in types) {
  min <- min(unlist(lapply(simulateds, function(s) min(s[, type]))))
  max <- max(unlist(lapply(simulateds, function(s) max(s[, type]))))
  plot(alldates, rep(0, length(alldates)), main = typeNames[[type]], xlab = "Zeit", ylab = type, ylim = c(min, max), type = "n", cex = 0.5)
  abline(v = phases[2:(length(phases)-1)])

  for (i in seq_len(length(simulateds))) {
    simulated <- simulateds[[i]]
    lines(simulated$date, simulated[, type])
  }
}
