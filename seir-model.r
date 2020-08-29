S0 <- 82000000 #initial susceptible
E0 <- 9 #exposed
I0 <- 2 #initial infected
R0 <- 0 #recovered
N <- S0+E0+I0+R0

beta <- 1/100000 #transmissionsrate
sigma <- 1/14 #infected to recovered
gamma <- 1/7 #exposed to infected

out1 <- matrix (0, ncol =4, nrow =200)
for (i in 1:200){
  S0n <- S0
  E0n <- E0
  I0n <- I0
  R0n <- R0
  S0 <-  max(0, S0n -beta*((S0n*I0n)))
  E0 <- max(0, E0n + beta*((S0n*I0n)) - sigma * E0n)
  I0 <- max(0, I0n + sigma*E0n - gamma*I0n)
  R0 <- max(0, R0n + gamma*I0n)
  out1[i,1] <- S0
  out1[i,2] <- E0
  out1[i,3] <- I0
  out1[i,4] <- R0
  
}

plot (1:200, out1[,1], type ="l", ylim =c(0,82000000))
lines (1:200, out1[,2], col ="orange" )
lines (1:200, out1[,3], col ="red" )
lines (1:200, out1[,4], col ="seagreen")


  
  

