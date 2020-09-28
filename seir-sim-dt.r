library(deSolve)

N <- 4000000
R0 <- 0
I0 <- 1000
E0 <- 20000
S0 <- N - E0 - I0 - R0
beta <- 0.8
gamma <- 0.33
sigma <- 0.19
steps <- 100

seirgen <- function(dtstep) {
  # dtstep is the step length in time units (e.g. 0.1 days per simulation step).
  # Since dtstep is the step length _after_ the first day we need to subtract 1 from t before dividing by dtstep.
  # E.g. 3 days with a step length of 0.5 result in 5 rows: 1.0, 1.5, 2.0, 2.5, 3.0.
  # Same logic applies for calculating "time" below.
  rows <-  (steps - 1) / dtstep + 1
  out <- matrix(0, ncol = 5, nrow = rows)
  colnames(out) <- c("time", "S", "E", "I", "R")

  S <- S0
  E <- E0
  I <- I0
  R <- R0

  for (i in seq(1, rows)) {
    Sn <- S
    En <- E
    In <- I
    Rn <- R
    out[i, "time"] <- (i - 1) * dtstep + 1
    out[i, "S"] <- Sn
    out[i, "E"] <- En
    out[i, "I"] <- In
    out[i, "R"] <- Rn
    S <- Sn - dtstep * (beta * ((Sn * In) / N))
    E <- En + dtstep * (beta * ((Sn * In) / N) - sigma * En)
    I <- In + dtstep * (sigma * En - gamma * In)
    R <- Rn + dtstep * (gamma * In)
  }

  out
}

out1 <- seirgen(1)
out24 <- seirgen(1 / 24)

plot(out1[, "time"], out1[, "I"], type = "l", xlab = "Zeit", ylab = "I")
lines(out24[, "time"], out24[, "I"], col = "red")
legend("topright", c("1d", "1h"), fill = c("black", "red"), title = "Zeitschrittlänge", horiz = TRUE)
