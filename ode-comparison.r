library(deSolve)
library(ggplot2)
library(reshape2)

x <- seq(1, 10)

euler <- ode(
  times = x,
  y = c(y = 1),
  parms = NULL,
  method = "euler",
  func = function(t, y, parms) {
    list(2 * t)
  }
)

lsoda <- ode(
  times = x,
  y = c(y = 1),
  parms = NULL,
  func = function(t, y, parms) {
    list(2 * t)
  }
)

data <- data.frame(x = x, euler = euler[, "y"], lsoda = lsoda[, "y"])
molten <- melt(data, id.vars = "x")

ggplot(data = molten, mapping = aes(x = x, y = value, linetype = variable)) +
  theme_minimal() +
  labs(title = "f(x) = x^2", y = "y") +
  scale_linetype_manual(values = c("22", "solid")) +
  scale_x_continuous(breaks = x) +
  geom_line() +
  geom_point()
