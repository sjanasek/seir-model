library(deSolve)

x <- seq(1, 10)

euler <- ode(
  times = x,
  y = c(Y = 1),
  parms = NULL,
  method = "euler",
  func = function(t, y, parms) {
    list(2 * t)
  }
)

lsoda <- ode(
  times = x,
  y = c(Y = 1),
  parms = NULL,
  func = function(t, y, parms) {
    list(2 * t)
  }
)

plot(x, euler[, "Y"], type = "b", main = "f(x) = x^2", ylab = "y", ylim = c(0, 100))
lines(x, lsoda[, "Y"], type = "b", col = "orange")
