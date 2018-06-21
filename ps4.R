rm(list=ls(all=T))
graphics.off()
#library(tseries)

##Problem 6
##(a)
set.seed(143)
sim.ar1<-arima.sim(list(ar=c(0.2)),n=1000)
sim.ar2<-arima.sim(list(ar=c(0.9)),n=1000)

sim.ma1<-arima.sim(list(ma=c(0.1)),n=1000)
sim.ma2<-arima.sim(list(ma=c(0.7)),n=1000)

# ts-plots

par(mfrow=c(2,2))
ts.plot(sim.ar1,main ="AR(1) with phi=0.2", ylim=c(-5,5))
abline(h=0)
ts.plot(sim.ar2,main ="AR(1) with phi=0.9", ylim=c(-5,5))
abline(h=0)
ts.plot(sim.ma1,main ="MA(1) with theta=0.1", ylim=c(-5,5))
abline(h=0)
ts.plot(sim.ma2,main ="MA(1) with theta=0.7", ylim=c(-5,5))
abline(h=0)

##(b)
# corresponding ACFs und PACFS

par(mfrow=c(2,4))
acf(sim.ar1,main="ACF of AR(1) with phi=0.2")
acf(sim.ar2,main="ACF of AR(1) with phi=0.9")
acf(sim.ma1,main="ACF of MA(1) with theta=0.1")
acf(sim.ma2,main="ACF of MA(1) with theta=0.7")

pacf(sim.ar1,main="PACF of AR(1) with phi=0.2")
pacf(sim.ar2,main="PACF of AR(1) with phi=0.9")
pacf(sim.ma1,main="PACF of MA(1) with theta=0.1")
pacf(sim.ma2,main="PACF of MA(1) with theta=0.7")

par(mfrow=c(1,1))
library(PerformanceAnalytics)
chart.ACFplus(sim.ar2)

##(c)
sim.ar3<-arima.sim(list(ar=c(-0.2)),n=1000)
sim.ma3<-arima.sim(list(ma=c(-0.1)),n=1000)

sim.ar4<-arima.sim(list(ar=c(-0.9)),n=1000)
sim.ma4<-arima.sim(list(ma=c(-0.7)),n=1000)

par(mfrow=c(2,2))
ts.plot(sim.ar3,main ="AR(1) with phi= -0.2", ylim=c(-5,5))
abline(h=0)
ts.plot(sim.ar4,main ="AR(1) with phi= -0.9", ylim=c(-5,5))
abline(h=0)
ts.plot(sim.ma3,main ="MA(1) with theta= -0.1", ylim=c(-5,5))
abline(h=0)
ts.plot(sim.ma4,main ="MA(1) with theta= -0.7", ylim=c(-5,5))
abline(h=0)

# corresponding ACFs und PACFS
par(mfrow=c(2,4)) 
acf(sim.ar3,main="ACF of AR(1) with phi= -0.2")
acf(sim.ar4,main="ACF of AR(1) with phi= -0.9")
acf(sim.ma3,main="ACF of MA(1) with theta= -0.1")
acf(sim.ma4,main="ACF of MA(1) with theta= -0.7")

pacf(sim.ar3,main="PACF of AR(1) with phi= -0.2")
pacf(sim.ar4,main="PACF of AR(1) with phi= -0.9")
pacf(sim.ma3,main="PACF of MA(1) with theta= -0.1")
pacf(sim.ma4,main="PACF of MA(1) with theta= -0.7")

##(d)
sim.ar5<-arima.sim(list(ar=c(0.9,-0.2)),n=1000)
sim.arma1<-arima.sim(list(ar=c(0.9,-0.2), ma=c(0.7,-0.1)),n=1000)
par(mfrow=c(2,1))
ts.plot(sim.ar5,main ="AR(2) with phi=(0.9,-0.2)", ylim=c(-5,5))
abline(h=0)
ts.plot(sim.ar4,main ="ARMA(2,2)", ylim=c(-5,5))
abline(h=0)
par(mfrow=c(2,2))
acf.ar5<- acf(sim.ar5,main="ACF of AR(2) process")
acf.arma1<-acf(sim.arma1,main="ACF of ARMA(2,2) process")
pacf(sim.ar5,main="PACF of AR(2) process")
pacf(sim.arma1,main="PACF of ARMA(2,2) process")


##(e)
par(mfrow=c(1,1))
plot(acf.ar5$acf, col=1, xlab="lags", ylab="ACF")
points(acf.arma1$acf, col=2)


