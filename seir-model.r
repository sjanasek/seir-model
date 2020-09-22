library(deSolve)

N <- 83000000 # population
R0 <- 0 # initially recovered
I0 <- 292 # initially infected
E0 <- 20 * I0 # initially exposed
S0 <- N - E0 - I0 - R0
beta <- 0.9 # rate of transmission/infection
gamma <- 1 / 3 # rate of recovery (infected -> recovered)
sigma <- 0.19 # rate at which exposure leads to infection
steps <- 365
dtstep <- 1

out <- ode(
  times = 1:steps,
  y = c(S = S0, E = E0, I = I0, R = 0),
  parms = c(N = N, beta = beta, gamma = gamma, sigma = sigma, dtstep = dtstep),
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

plot(seq_len(nrow(out)), out[, "I"], type = "l", xlab = "day", ylab = "infected")
