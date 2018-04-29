#######################
#### Problem Set 1 ####
#######################

# Problem 1

setwd("C:/Users/Desktop/finectrics/SS2018")
sbux.df = read.csv(file="sbuxPrices.csv", 
                   header=TRUE, stringsAsFactors=FALSE)
# sbux.df is a data.frame object. Data.frames are rectangular data objects typically with
# observations in rows and variables in columns
class(sbux.df)
str(sbux.df)
head(sbux.df)
tail(sbux.df)
colnames(sbux.df)
class(sbux.df$Date)
class(sbux.df$Adj.Close)

# subsetting operations
sbux.df[1:5,"Adj.Close"]
sbux.df[1:5,6]
sbux.df$Adj.Close[1:5]

# to preserve dimension information
sbux.df[1:5,"Adj.Close",drop=F]
sbux.df[1:5,6,drop=FALSE]

#find the dates on 1994-03-01
which(sbux.df$Date=="1994-03-01")
which(sbux.df$Date=="1995-04-01")
sbux.df[which(sbux.df$Date=="1995-04-01"),]
sbux.df[which(sbux.df$Date=="1994-03-01"):which(sbux.df$Date=="1995-04-01"),]
sbux.df[13:26,]

#create a new data.frame containing the price data with dates as row names
sbuxPrices.df = sbux.df[,"Adj.Close",drop=F]
rownames(sbuxPrices.df) = sbux.df$Date
sbuxPrices.df["1998-01-01",1]

#plot the data
plot(sbux.df$Adj.Close)

#a better plot
plot(sbux.df$Adj.Close,type="l",ylim = c(0,20),
     ylab = "Adjusted close", main = "Monthly closing price of SBUX")
legend(x="topleft",legend="SBUX", lty = 1, lwd=3, col="red")

#compute returns
#simple 1-month returns
n=nrow(sbuxPrices.df)
sbux.ret=(sbuxPrices.df[2:n,1]-sbuxPrices.df[1:(n-1),1])/sbuxPrices.df[1:(n-1),1]
class(sbux.ret)
names(sbux.ret)=rownames(sbuxPrices.df)[2:n]

#Note: to ensure that sbux.ret is a data.frame use drop=FALSE/drop=F 
#when computing returns
sbux.ret.df = (sbuxPrices.df[2:n,1,drop=F]-sbuxPrices.df[1:(n-1),1,drop=F])/sbuxPrices.df[1:(n-1),1,drop=F]
class(sbux.ret.df)

#CC return
sbux.ccret = log(1+sbux.ret)
#alternatively
sbux.ccret = log(sbuxPrices.df[2:n,1])- log(sbuxPrices.df[1:(n-1),1])
names(sbux.ccret)=rownames(sbuxPrices.df)[2:n]
head(cbind(sbux.ret, sbux.ccret))

#plot in separate graphs
par(mfrow=c(2,1))
plot(sbux.ret,type="l",col="blue",lwd=2,ylab="Return",main="Monthly simple return")
abline(h=0)
plot(sbux.ccret,type="l",col="blue",lwd=2,ylab="Return",main="Monthly cc return")
abline(h=0)


#plot the returns on the same graph
par(mfrow=c(1,1))
plot(sbux.ret,type="l",col="blue",lwd=2,ylab="Return",
     main="Monthly simple return",ylim=c(-1,0.5))
abline(h=0)
lines(sbux.ccret,col="red",lwd=2)
legend(x="bottomright", legend = c("Simple","CC"),lty=1,
       lwd=2,col=c("blue","red"))

#compute gross returns 
sbux.gret = 1+ sbux.ret
sbux.fv = cumprod(sbux.gret)

plot(sbux.fv, type="l")


######################################################################

# Problem 2
install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)
library(tseries)
library(zoo)

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

start(MSFT.prices)
end(SP500.prices)
colnames(MSFT.prices)="MSFT"
colnames(SP500.prices)="SP500"

#recall: how to find the usage, arguments of "merge" function
MSFTSP500.prices = merge(MSFT.prices,SP500.prices)

#CC return
MSFT.ret = diff(log(MSFT.prices))
SP500.ret = diff(log(SP500.prices))

MSFT.ret.mat = coredata(MSFT.ret)

# time plots
?plot.zoo
plot(MSFT.prices,main="Monthly closing price of MSFT",col="blue",lwd=3)
plot(SP500.prices,main="Monthly closing price of MSFT",col="blue",lwd=3)

# put returns on the same plot in separate panels
my.panel = function(...){
  lines(...)
  abline(h=0)
}

plot(MSFTSP500.prices, panel=my.panel, lwd=2)
plot(MSFTSP500.prices, plot.type = "single", lty=c("dashed","solid"))




