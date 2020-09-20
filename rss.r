library(deSolve)

load.data=function(file,from,to)
{
dat=read.csv(file)
W=data.frame(substr(dat$Meldedatum,1,10),dat$AnzahlFall,stringsAsFactors = F)
names(W)=c("datum","faelle")
B=aggregate(W$faelle,by=list(datum=W$datum),FUN=sum)
names(B)=c("datum","I")
B$acc=cumsum(B$I)
B$datum=strptime(B$datum,"%Y/%m/%d",tz="UTC")
B=B[B$datum>=from & B$datum<to,]
return(B)
}


#-----------------------------
# Konstanten  (s. RKI/Engbert et. al.)
const=list(
  N=83000000,
  sigma=0.19,
  gamma=1/3,
  Efactor=20)

seir=function (t,x,p)
{
  dS=-p*x["S"]*x["I"]/const$N
  dE=p*x["S"]*x["I"]/const$N-const$sigma*x["E"]
  dI=const$sigma*x["E"]-const$gamma*x["I"]
  dR=const$gamma*x["I"]
  return(list(c(dS,dE,dI,dR)))
}

SIM=function(y,func,times,beta)
{
  return(data.frame(ode(y=ini,func=seir, times=times,parms=beta)))
}

RSS=function(beta, y)
{
 z=SIM(ini,seir,seq(1,length(y),1),beta)
 err=sum((y - z[4])^2)
 return(err)
}

A <- load.data("data_2020-09-20.csv", "2020-03-02", "2020-05-02")

recovered <-
  ceiling(c(rep(0, 14), head(A$acc * 0.8, -14))) +
    floor(c(rep(0, 28), head(A$acc * 0.2, -28)))

A$acc <- A$acc - recovered

#--------------------------------
# Anfangswerte
E0=const$Efactor*A$acc[1]
S0=const$N-A$acc[1]-E0
I0=A$acc[1]
R0=0
ini=c(S=S0,E=E0,I=I0,R=R0)

OPT=optim(0.5,RSS, method = "L-BFGS-B", lower = c(0.1),upper=c(2), y=A$acc, control = list(parscale = c(0.001), factr=1))
OPT


beta=OPT$par

sim=data.frame(ode(func=seir,y=ini,times=1:length(A$acc),parms=beta))

par(mar=c(2,2,2,1))
plot(A$datum,sim$I,col="red",ty="b")
lines(A$datum,A$acc,col="blue",ty="b")
mtext(bquote(beta == .(round(beta, 2))),3,line=-1.5,adj=0.1,col="red",cex=1.3)
