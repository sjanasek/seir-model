library(deSolve)
library(ggplot2)
library(reshape2)

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

simulated <- NULL
for (i in 1:(length(phases) - 1)) {
  from <- phases[i]
  to <- phases[i + 1]
  beta <- betas[i]

  date <- seq.Date(from, to, 1)
  s <- seirsimvisual(date, S0, E0, I0, R0, beta, gamma, sigma)
  simulated <- rbind(simulated, s)

  # Starting parameters for the next iteration equal the final values for the current simulated run,
  # since the next simulated phase should pick up where the current one left off.
  R0 <- s[nrow(s), "R"]
  I0 <- s[nrow(s), "I"]
  E0 <- s[nrow(s), "E"]
  S0 <- N - E0 - I0 - R0
}

# My intention is to draw one plot per column within the "simulated" data frame.
# This shows how: https://stackoverflow.com/q/18046051
molten <- melt(simulated, id.vars = "date")

vlineDates <- data.frame(date = phases[2:(length(phases) - 1)])
vlines <- geom_vline(data = vlineDates, mapping = aes(xintercept = date), linetype = "dotted")

ggplot(data = molten, mapping = aes(x = date, y = value)) +
  theme_minimal() +
  labs(x = "Zeit", y = NULL) +
  facet_wrap(vars(variable), scales = "free", switch = "y") +
  geom_line() +
  vlines

ggplot(data = molten, mapping = aes(x = date, y = value, color = variable)) +
  theme_minimal() +
  scale_colour_brewer(palette = "Set1") +
  labs(x = "Zeit", y = NULL, colour = NULL) +
  geom_line() +
  vlines
