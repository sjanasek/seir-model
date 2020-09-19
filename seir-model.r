library(deSolve)

# constants
N <- 82000000 # population
R0 <- 0 # initially recovered
I0 <- 2 # initially infected
E0 <- 9 # initially exposed
S0 <- N - E0 - I0 - R0
beta <- 1 / 4 # rate of transmission/infection
gamma <- 1 / 20 # rate of recovery (infected -> recovered)
sigma <- 1 / 5 # rate at which exposure leads to infection
steps <- 1000
dtstep <- 1

out <- matrix(0, ncol = 5, nrow = steps)
colnames(out) <- c("time", "S", "E", "I", "R")
outcolrange <- seq_len(nrow(out))

# variables
S <- S0 # initial susceptible
E <- E0 # exposed
I <- I0 # initial infected
R <- R0 # recovered

for (i in outcolrange) {
  Sn <- S
  En <- E
  In <- I
  Rn <- R
  out[i, "time"] <- i
  out[i, "S"] <- Sn
  out[i, "E"] <- En
  out[i, "I"] <- In
  out[i, "R"] <- Rn
  S <- Sn - dtstep * (beta * ((Sn * In) / N))
  E <- En + dtstep * (beta * ((Sn * In) / N) - sigma * En)
  I <- In + dtstep * (sigma * En - gamma * In)
  R <- Rn + dtstep * (gamma * In)
}

SEIR <- function(t, y, parms) {
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

odeout <- ode(c(S = S0, E = E0, I = I0, R = 0), 1:steps, SEIR, c(N = N, beta = beta, gamma = gamma, sigma = sigma), method = "euler")

plot(outcolrange, out[, "I"], type = "l", ylim = c(0, N))
lines(outcolrange, odeout[, "I"], col = "orange")
