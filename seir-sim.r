library(deSolve)
library(RColorBrewer)

seirgen <- function(date, S0, E0, I0, R0, beta, gamma, sigma) {
  out <- ode(
    times = seq_len(length(date)),
    y = c(S = S0, E = E0, I = I0, R = R0),
    parms = c(N = N, beta = beta, gamma = gamma, sigma = sigma, dtstep = 1),
    method = "euler",
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

colors <- brewer.pal(9, name = "Set1")
phases <- data.frame(
#        no lockdown            lockdown               no lockdown            FLEISCHSKANDAL         MALLE
from = c(as.Date("2020-03-02"), as.Date("2020-03-24"), as.Date("2020-05-07"), as.Date("2020-06-16"), as.Date("2020-06-29")),
to = c(as.Date("2020-03-23"), as.Date("2020-05-06"), as.Date("2020-06-15"), as.Date("2020-06-28"), as.Date("2020-08-31")),
beta = c(0.6, 0.2, 0.6, 0.2, 0.1),
color = colors[1:5]
)

allphases <- seq.Date(from = phases[1, "from"], to = phases[nrow(phases), "to"], by = 1)
N <- 83000000
R0 <- 0
I0 <- 292
E0 <- 20 * I0
S0 <- N - E0 - I0 - R0
gamma <- 0.33
sigma <- 0.19

plot(allphases, rep(0, length(allphases)), xlab = "date", ylab = "infected", ylim = c(0, 1e5), type = "n", cex = 0.5)

for (i in seq_len(nrow(phases))) {
  phase <- phases[i,]
  date <- seq.Date(phase$from, phase$to, 1)
  simulated <- seirgen(date, S0, E0, I0, R0, phase$beta, gamma, sigma)

  lines(simulated$date, simulated$I, col = phase$color)

  # Starting parameters for the next iteration equal the final values for the current simulated run,
  # since the next simulated phase should pick up where the current one left off.
  R0 <- simulated[nrow(simulated), "R"]
  I0 <- simulated[nrow(simulated), "I"]
  E0 <- simulated[nrow(simulated), "E"]
  S0 <- N - E0 - I0 - R0
}
