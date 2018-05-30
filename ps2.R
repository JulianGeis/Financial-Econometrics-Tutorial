rm(list=ls(all=T))
graphics.off()
library(tseries)
library(zoo)
library(PerformanceAnalytics)
MSFT.prices = get.hist.quote(instrument = "msft",start = "1998-01-01",
                             end="2012-05-31",quote="AdjClose",
                             provider = "yahoo", origin="1970-01-01",
                             compression = "m", retclass = "zoo")

SP500.prices = get.hist.quote(instrument = "^gspc",start = "1998-01-01",
                              end="2012-05-31",quote="AdjClose",
                              provider = "yahoo", origin="1970-01-01",
                              compression = "m", retclass = "zoo")

colnames(MSFT.prices) = "MSFT"
colnames(SP500.prices) = "SP500"

MSFT.ret = diff(log(MSFT.prices))
SP500.ret = diff(log(SP500.prices))

MSFT.ret.mat = coredata(MSFT.ret)
SP500.ret.mat = coredata(SP500.ret)

# (a)quantiles
quantile(MSFT.ret.mat)
median(MSFT.ret.mat)
quantile(MSFT.ret.mat, prob=c(0.01,0.05))

# compare it to normal quantiles
qnorm(p=c(0.01,0.05), mean=mean(MSFT.ret.mat),
      sd=sd(MSFT.ret.mat))

# (b)
median(MSFT.ret.mat)
quantile(MSFT.ret.mat, probs = 0.75) - quantile(MSFT.ret.mat, probs = 0.25)
mean(MSFT.ret.mat)
var(MSFT.ret.mat)
sd(MSFT.ret.mat)
skewness(MSFT.ret.mat)
kurtosis(MSFT.ret.mat)
summary(MSFT.ret.mat)

apply(MSFT.ret.mat, 2, kurtosis)

# (c)
# create simulated data
set.seed(123)
gwn=rnorm(length(MSFT.ret.mat),mean=mean(MSFT.ret.mat),
      sd=sd(MSFT.ret.mat))
gwn.zoo=zoo(gwn,index(MSFT.ret))

n1=length(gwn)
plot(ecdf(gwn))
plot(sort(gwn),(1:n1)/n1, type="s")

# compare empirical cdf to standard normal cdf for simulated gaussian data
z1=scale(gwn)   #standardize to have mean zero and sd 1
n1=length(gwn)
F.hat=(1:n1)/n1 #empirical cdf
x1=sort(z1)     #sort from the smallest to largest
y1=pnorm(x1)    #compute standard normal cdf at x

plot(x1, y1, main="Empirical CDF VS Normal CDF")
points(x1, F.hat, lty=1, type="s", col="orange", lwd=3)
legend(x="topleft",legend=c("Normal CDF", "Empirical CDF"), 
       lty = c(1,1), col=c("black","orange"))



# (d)
# MSFT cc return: compare empirical cdf to standard normal cdf
z1=scale(MSFT.ret.mat)
n1=length(MSFT.ret.mat)
F.hat=(1:n1)/n1
x1=sort(z1)
y1=pnorm(x1)

plot(x1, y1, main = "Empirical CDF VS Normal CDF")
points(x1, F.hat, lty=1, type="s", col="orange", lwd=3)
legend(x="topleft",legend=c("Normal CDF", "Empirical CDF"), lty = c(1,1),
       col=c("black","orange"))

# SP500 cc return: compare empirical cdf to standard normal cdf
z1=scale(SP500.ret.mat)
n1=length(SP500.ret.mat)
F.hat=(1:n1)/n1
x1=sort(z1)
y1=pnorm(x1)

plot(x1, y1, main = "Empirical CDF VS Normal CDF")
points(x1, F.hat, lty=1, type="s", col="orange", lwd=3)
legend(x="topleft",legend=c("Normal CDF", "Empirical CDF"), lty = c(1,1),
       col=c("black","orange"))


# (e)
# ks test
ks.test(MSFT.ret.mat, gwn)
ks.test(SP500.ret.mat, gwn)
gwn1=rnorm(length(SP500.ret.mat),mean=mean(SP500.ret.mat),
          sd=sd(SP500.ret.mat))
ks.test(SP500.ret.mat, gwn1)


# (f) QQ plots
par(mfrow=c(1,1))
# 1. compare empirical quantiles to those from normal distribution
par(mfrow=c(2,2))	# 4 panel layout: 2 rows and 2 columns

qqnorm(gwn, main="Gaussian White Noise", col="slateblue1")
qqline(gwn)
qqnorm(MSFT.ret.mat, main="MSFT Returns", col="slateblue1")
qqline(MSFT.ret.mat)
qqnorm(SP500.ret.mat, main="SP500 Returns", col="slateblue1")
qqline(SP500.ret.mat)
par(mfrow=c(1,1))

#par(mfrow=c(2,2))
# 2. t distribution with 3 df
set.seed(123)
tdata = rt(100, df=3)

xx=seq(from=-5,to=5,length=100)
plot(xx, dnorm(xx), type="l", lwd=2, main="Normal and Student-t with 3 df", 
     xlab = "z, t", ylab = "pdf")
points(xx,dt(xx,df=3), type="l", col="orange", lwd=3)
legend(x="topright", legend=c("Normal","Student-t"), lty=c(1,1), col=c("black","orange"),
       lwd=c(2,3))

# 3. log-normal distribution
lndata = rlnorm(100)

plot(xx, dnorm(xx), type="l", lwd=2, main="Normal and log-normal", 
     xlab = "z, t", ylab = "pdf",ylim=c(0,0.7))
points(xx, dlnorm(xx), type="l", lwd=3, col="orange")
legend(x="topleft", legend=c("Normal","log-Normal"), lty=c(1,1), col=c("black","orange"),
       lwd=c(2,3))
# 4. skewed normal distribution
# install.packages("sn")
library(sn)
skew.norm.data = rsn(100, alpha = 3)

plot(xx, dnorm(xx), type="l", lwd=2, main="Normal and skew-normal", 
     xlab = "z, t", ylab = "pdf",ylim=c(0,0.7))
points(xx, dsn(xx, alpha=3), type="l", lwd=3, col="orange")
legend(x="topleft", legend=c("Normal","skew-Normal"), lty=c(1,1), col=c("black","orange"),
       lwd=c(2,3))


qqnorm(tdata)
qqline(tdata)

qqnorm(skew.norm.data)
qqline(skew.norm.data)

# (g)
ks.test(MSFT.ret.mat, gwn)
ks.test(MSFT.ret.mat, tdata)
ks.test(MSFT.ret.mat, lndata)
ks.test(MSFT.ret.mat, skew.norm.data)

ks.test(MSFT.ret.mat, SP500.ret)












