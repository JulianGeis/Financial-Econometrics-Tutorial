#######################
#### Problem Set 2 ####
#######################

# Problem 3
rm(list=ls(all=T))
graphics.off()

MSFT.prices = get.hist.quote(instrument = "msft",start = "1998-01-01",
                             end="2012-05-31",quote="AdjClose",
                             provider = "yahoo", origin="1970-01-01",
                             compression = "m", retclass = "zoo")

SP500.prices = get.hist.quote(instrument = "^gspc",start = "1998-01-01",
                              end="2012-05-31",quote="AdjClose",
                              provider = "yahoo", origin="1970-01-01",
                              compression = "m", retclass = "zoo")

colnames(MSFT.prices)="MSFT"
colnames(SP500.prices)="SP500"

MSFT.ret = diff(log(MSFT.prices))
SP500.ret = diff(log(SP500.prices))

MSFT.ret.mat = coredata(MSFT.ret)
SP500.ret.mat = coredata(SP500.ret)

#(a)quantiles
quantile(MSFT.ret.mat)
median(MSFT.ret.mat)
quantile(MSFT.ret.mat, prob=c(0.01,0.05))

#compare it to normal quantiles
qnorm(p=c(0.01,0.05), mean=mean(MSFT.ret.mat),
      sd=sd(MSFT.ret.mat))

#(b)
median(MSFT.ret.mat)
quantile(MSFT.ret.mat,probs = 0.75)-quantile(MSFT.ret.mat,probs = 0.25)
mean(MSFT.ret.mat)
var(MSFT.ret.mat)
sd(MSFT.ret.mat)
skewness(MSFT.ret.mat)
kurtosis(MSFT.ret.mat)
summary(MSFT.ret.mat)

apply(MSFT.ret.mat,2,kurtosis)

#(c)
#simulate data
set.seed(123)
gwn=rnorm(length(MSFT.ret.mat),mean=mean(MSFT.ret.mat),
      sd=sd(MSFT.ret.mat))
gwn.zoo=zoo(gwn,index(MSFT.ret))



#
n1=length(gwn)
plot(ecdf(gwn))
plot(sort(gwn),(1:n1)/n1, type="s")
#compare empirical cdf to standard normal cdf simulated gaussian data
z1=scale(gwn)
n1=length(gwn)
F.hat=(1:n1)/n1
x1=sort(z1)
y1=pnorm(x1)

plot(x1,y1,main="Empirical CDF VS Normal CDF")
points(x1,F.hat,lty=1,col="orange")
legend(x="topleft",legend=c("Normal CDF", "Empirical CDF"), lty = c(1,1),
                            col=c("black","orange"))

#compare empirical cdf to standard normal cdf of MSFT
z1=scale(MSFT.ret.mat)
n1=length(MSFT.ret.mat)
F.hat=(1:n1)/n1
x1=sort(z1)
y1=pnorm(x1)

plot(x1,y1,main="Empirical CDF VS Normal CDF")
points(x1,F.hat,lty=1,col="orange")
legend(x="topleft",legend=c("Normal CDF", "Empirical CDF"), lty = c(1,1),
       col=c("black","orange"))

#ks test
ks.test(MSFT.ret.mat,gwn)
ks.test(SP500.ret.mat,gwn)

par(mfrow=c(1,1))

qqnorm(gwn,main="Gaussian WN")
qqline(gwn)

qqnorm(MSFT.ret,main="Gaussian WN")
qqline(MSFT.ret)

set.seed(123)
#t distribution
tdata = rt(100, df=3)
#log-normal distribution
lndata = rlnorm(100)
#skewed normal distribution
#skew.norm.data = rsn(100,shape=2)





















