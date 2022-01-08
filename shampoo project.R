library(tseries)
library(ggplot2)
library(TSA)
library(fUnitRoots)
library(lmtest)
library(forecast)
library(readr)
library(MLmetrics)
library(gtools)

shampoo <- read_csv("C:/Users/srini/Downloads/shampoo.txt")
View(shampoo)

shampoo

#converting data frame to time series
shampoo.ts <- matrix(shampoo$Sales, nrow = 36, ncol = 1)
shampoo.ts <- as.vector(t(shampoo.ts))
shampoo.ts <- ts(shampoo.ts,start=c(1,1), end=c(3,12), frequency=12)

plot(ts(as.vector(shampoo.ts)),type='o',ylab='Sales')
plot(decompose(shampoo.ts))
plot.ts(shampoo)

#line graph
plot(shampoo$Sales,type="o")

#ggplot
ggseasonplot(shampoo.ts)

#auto-correlation function
acf(shampoo.ts, lag.max = 36)

#partial auto-correlation function
pacf(shampoo.ts,lag.max = 36)

#augmented dickey-fuller test
adf.test(shampoo.ts,alternative = c("stationary", "explosive"), 
         k = trunc((length(shampoo.ts)-1)^(1/3)))


#checking seasonality when D=0
dif=diff(shampoo.ts)
plot(dif)
adf.test(dif)


#checking seasonality when D=1
diff.shampoo.1 = diff(dif)
plot(diff.shampoo.1,type='o',ylab='Sales Count of Shampoo')
adf.test(diff.shampoo.1)

par(mfrow=c(1,1))
res2 = armasubsets(y=diff.shampoo.1,nar=10,nma=10,y.name='test',ar.method='ols')

plot(res2)
length(shampoo$Sales)


#prediction
arima_012_ml <- arima(shampoo.ts,order=c(0,1,2),method='ML')
coeftest(arima_012_ml)
auto.arima(shampoo.ts)
fit=Arima(shampoo.ts,c(0,1,2))
plot(forecast(fit,h=10))
fit
predict(arima_012_ml,n.ahead = 12,newxreg = NULL,se.fit=TRUE)
pred<-predict(arima_012_ml,n.ahead = 12,newxreg = NULL,se.fit=TRUE)

#MAPE
map<-round(MAPE(shampoo.ts[13:24],pred),3)
map



