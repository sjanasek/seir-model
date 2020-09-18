# constants
N <- 82000000 # population
E0 <- 9 # initially exposed
I0 <- 2 # initially infected
R0 <- 0 # initially recovered
beta <- 1 / 4 # rate of transmission/infection
gamma <- 1 / 20 # rate of recovery (infected -> recovered)
sigma <- 1 / 5 # rate at which exposure leads to infection
steps <- 1000
dtstep <- 0.25

# variables
S <- N - E0 - I0 - R0 # initial susceptible
E <- E0 # exposed
I <- I0 # initial infected
R <- R0 # recovered

out <- matrix(0, ncol = 4, nrow = steps)
outcolrange <- seq_len(nrow(out))

for (i in outcolrange) {
  Sn <- S
  En <- E
  In <- I
  Rn <- R
  S <- Sn - dtstep * (beta * ((Sn * In) / N))
  E <- En + dtstep * (beta * ((Sn * In) / N) - sigma * En)
  I <- In + dtstep * (sigma * En - gamma * In)
  R <- Rn + dtstep * (gamma * In)
  out[i, 1] <- S
  out[i, 2] <- E
  out[i, 3] <- I
  out[i, 4] <- R
}

plot(outcolrange, out[, 1], type = "l", ylim = c(0, N))
lines(outcolrange, out[, 2], col = "orange")
lines(outcolrange, out[, 3], col = "red")
lines(outcolrange, out[, 4], col = "seagreen")
