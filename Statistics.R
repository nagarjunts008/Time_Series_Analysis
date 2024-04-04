library(zoo)
install.packages("dplyr")
install.packages("forecast")
install.packages("quantmod")
install.packages("nortest")
install.packages("Peptides")

library(Peptides)
library(nortest)
library(xts)
library(forecast)
library(stats)
library(tseries)
library(dplyr)
library(quantmod)

##Obtain the time series using the correction code syntax making use of the code below
getSymbols("^GSPC", from = '2009-01-01',to = "2020-12-31",warnings = FALSE,auto.assign = TRUE)
print(head(GSPC,10))

P = GSPC$GSPC.Close
dt = index(GSPC)
xtscclose = xts(P,dt)
print(head(xtscclose,10))
plot(xtscclose)

##Transform your series to log-returns
returns = diff(log(GSPC$GSPC.Close)) 
print(head(returns,10))
plot(returns)

##weekly
sp500_w = to.weekly(xtscclose)
print(head(sp500_w,10))
plot(sp500_w)

##monthly
sp500_m = to.monthly(xtscclose)
print(head(sp500_m,10))
plot(sp500_m)

## Examine the ACF and PACF functions
par(mfrow = c(1, 2))
acf(GSPC$GSPC.Close,xlab="Lag",ylab="ACF",main="ACF")
pacf(GSPC$GSPC.Close,xlab="Lag",ylab="PACF",main="PACF") 
par(mfrow = c(1, 1))

acf(GSPC$GSPC.Close)
pacf(GSPC$GSPC.Close)

## Applying arima() to the dataset 
print(summary(arima(xtscclose , order = c(2,1,2))))
print(summary(auto.arima(GSPC$GSPC.Close, lambda = "auto")))

##Check the data for stationarity using the correct test statistic and comment on the output
print(adf.test(GSPC$GSPC.Close))

##Fit an ARIMA model and determine the correct lag order: Show the 1-liner codes for output.
modelfit <-auto.arima(GSPC$GSPC.Close, lambda = "auto")
summary(modelfit)

#Perform the Ljung-Box test and describe the test-hypothesis and
report/comment on the result.
Box.test(modelfit$residuals,lag=2,type="Ljung-Box")
Box.test(modelfit$residuals, type="Ljung-Box")

# Perform a normality test of your choice on the return series and report the output.Write down the hypothesis test and comment on the p-value
hist(resid(modelfit),col='steelblue',main="Normality",sub="Normality test using Frequency Distribution",xlab="Price",ylab="Frequency Distribution")

##Normality tests
ks.test(returns,'pnorm')
ad.test(returns)
lillie.test(returns)
shapiro.test(returns)
pearson.test(returns)

##The residuals from an ARIMA fit require that:
##a. The residuals have zero mean= 0
##b. Have a finite variance 
##c. Have zero autocovariance= 0
##Using the results from checkresiduals(fitted_model) function comment on the above
checkresiduals(modelfit)
print(head(resid(modelfit),10))

##Mean
mean(resid(modelfit))

##Variance
var(resid(modelfit))

##Auto-Covariance
acf(resid(modelfit),lag.max = 2,type = c("covariance"),plot = FALSE)

##Predict Values
print(nrow(GSPC))
forecast(modelfit, 1)

