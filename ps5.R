rm(list=ls(all=T))
graphics.off()

##PS5: Problem 9
library(tseries)
Y = read.csv("exrates.csv")

##(a)
X<- Y[,2:6] # only data, no time index
nob<- length(X[,1])
RD <- log(X[2:nob,])-log(X[1:(nob-1), ])# log returns

# Canadian Dollar - for example:
adf.test(RD[,1]) 
kpss.test(RD[,1])
Box.test(RD[,1], lag = 1, type = c("Box-Pierce", "Ljung-Box"))
par(mfrow=c(1,2))
acf(RD[,1])  
pacf(RD[,1]) 

# model fit 
fit1 = arima(RD[,1], order = c(1, 0, 1)) # ARMA
fit2 = arima(RD[,1], order = c(1, 0, 0)) # ARIMA
fit3 = arima(RD[,1], order = c(0, 0, 1)) # ARIMA

# evaluate fit
fit1$aic # in-sample fit
fit2$aic 
fit3$aic 

BIC(fit1)
BIC(fit2)
BIC(fit3)

resid1 = fit1$residuals[-1]
resid2 = fit2$residuals[-1]

##(b)
par(mfrow=c(1,2))
acf(resid1)
pacf(resid1)

acf(resid2)
pacf(resid2)

Box.test(resid2, lag = 1, type = c("Box-Pierce", "Ljung-Box"))
# forecast comparison
# split sample in two parts
int<- length(RD[,1])-10 
outt<- 10

est.run1 = arima(RD[1:int,1], order = c(1, 0, 1), method = "ML") # in-sample model estimation
est.run2 = arima(RD[1:int,1], order = c(1, 0, 0), method = "ML") # in-sample model estimation
fore1.arima = predict(est.run1, n.ahead = outt) # out-of sample forecast
fore2.arima = predict(est.run2, n.ahead = outt) # out-of sample forecast

future= RD[(int+1): length(RD[,1]),1]

# RMSE
sqrt(mean((fore1.arima$pred - future)^2))
sqrt(mean((fore2.arima$pred - future)^2))

# Variance ratio tests (use Canadian Dollar for example)
# install.packages("vrtest")
library(vrtest)
# define the holding period kvec
kvec <- c(3,5,7)
# VR statistics for different holding times 
# M1: test for iid
# M2:for uncorrelated series with possible heteroskedasticity
Lo.Mac(RD[,1], kvec) 

Boot.test(RD[,1],kvec,nboot=300,wild="Normal")
#par(mfrow=c(1,1))
#VR.plot(RD[,1],kvec)
#Auto.VR(RD[,1]) # A variance ratio test with holding period value chosen by a data dependent procedure

