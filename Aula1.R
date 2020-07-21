install.packages("simts")
install.packages("prophet")
install.packages("astsa")
install.packages("forecast")
# Series Temporais --------------------------------------------------------

X <- rnorm(1000,mean=0,sd=2)
par(mfrow=c(1,2))
ts.plot(X)
acf(X,main="")

# Simulating a stationary AR(1) process
phi <- 0.5
n <- 1000
e <- rnorm(n,mean=0,sd=2)
y <- rep(NA,n)
y[1] <- e[1]
for (i in 2:n){
  y[i] <- phi*y[i-1] + e[i]
}
par(mfrow=c(1,2))
ts.plot(y)
acf(y,main="")

# Simulating a non stationary AR(1) process
phi <- 1
n <- 1000
e <- rnorm(n,mean=0,sd=2)
y <- rep(NA,n)
y[1] <- e[1]
for (i in 2:n){
  y[i] <- phi*y[i-1] + e[i]
}
par(mfrow=c(1,2))
ts.plot(y)
acf(y,main="")

# Another form for simulating an ARIMA process 
y <- arima.sim(model=list(order=c(0,1,0)),1000)
par(mfrow=c(1,2))
ts.plot(y)
acf(y,main="")

# Simulating a non stationary RB
library(simts)
Xt = gen_nswn(n_total = 1000)
par(mfrow=c(1,2))
plot(Xt)
acf(Xt,main="")


###############################################################
### Time series decomposition
###############################################################

# Using basis functions
data(AirPassengers)
AP <- AirPassengers
par(mfrow=c(1,1))
plot(AP)
decomposeAP <- decompose(AP,"additive")
plot(decomposeAP)
decomposeAP <- decompose(AP,"multiplicative")
plot(decomposeAP)

# Using forecast package for ts decomposition
library(forecast)
library(ggplot2)
data(AirPassengers)
AP <- AirPassengers
fit2 <- ets(AP)
autoplot(fit2)

# Using fbprophet package for ts decomposition
library(prophet)
library(xts)
data(AirPassengers)
AP <- AirPassengers
df <- data.frame(ds = index(as.xts(AP)),y = coredata(as.xts(AP)))
m <- prophet(df,seasonality.mode = 'multiplicative')
future <- make_future_dataframe(m, periods = 12)
forecast = predict(m,future)
prophet_plot_components(m,forecast)

###############################################################
### Trend analysis
###############################################################

# Deterministic x stochastic trend (random walk with drift)
X <- matrix(0,200,50)
y <- matrix(0,200,50)
y0 <- 0
for (i in 1:50){
  y[1,i] <- 0.5+y0+rnorm(1,0,.25)
  X[,i] <- 0.5*c(1:200)+rnorm(200,0,.25)
  for (j in 2:200){
    y[j,i] <- 0.5+y[j-1,i]+rnorm(1,0,.25)
  }
}
ts.plot(y[,1],ylab="",xlab="Simulação - 50 replicações")
for (i in 2:50){
  lines(y[,i])
}
for (i in 1:50){
  lines(X[,i],col=2)
}
legend("topleft",legend=c("TE","TD"),col=1:2, pch = 1,cex=0.8)

# Difference stationary process
set.seed(123)
X <- 0.5*c(1:200)+rnorm(200,0,.25)
y <- numeric(200)
y0 <- 0
y[1] <- 0.5+y0+rnorm(1,0,.25)
for (j in 2:200){
  y[j] <- 0.5+y[j-1]+rnorm(1,0,.25)
}
trend <- 0.5*c(1:200)
resX <- X-trend
resY <- y-trend

par(mfrow=c(2,4))
ts.plot(X,ylab="x")
lines(trend,col=2)
ts.plot(resX,ylab="res X",main="Resíduo é RB")
acf(resX,main="")
acf(resX,type="partial",main="")

ts.plot(y,ylab="y")
lines(trend,col=2)
ts.plot(resY,ylab="res Y",main="Resíduo não é RB")
acf(resY,main="")
acf(resY,type="partial",main="")

# Trend stationary process
diffX <- diff(X) 
diffY <- diff(y)

par(mfrow=c(2,4))
ts.plot(X,ylab="x")
lines(trend,col=2)
ts.plot(diffX,ylab="res X",main="Resíduo não é RB")
acf(diffX,main="")
acf(diffX,type="partial",main="")

ts.plot(y,ylab="y")
lines(trend,col=2)
ts.plot(diffY,ylab="res Y",main="Resíduo é RB")
acf(diffY,main="")
acf(diffY,type="partial",main="")

ima1 <- arima.sim(list(order = c(0,1,1), ma = 0.9), n = 100)
# Identification
par(mfrow=c(2,3))
ts.plot(ima1)
acf(ima1,main="")
acf(ima1,type="partial",main="")
ts.plot(diff(ima1))
acf(diff(ima1),main="")
acf(diff(ima1),type="partial",main="")


set.seed(1)
ima1 <- arima.sim(list(order = c(0,1,1), ma = 0.9), n = 100)
library(forecast)
Arima(ima1,order=c(0,1,1))

p <- 3
q <- 3
resultsAIC <- matrix(0,p+1,q+1)
colnames(resultsAIC) <- c(0:p)
rownames(resultsAIC) <- c(0:q)
for (i in 0:p){
  for(j in 0:q){
    resultsAIC[i+1,j+1] <- Arima(ima1, order = c(i, 1, j))$aicc
  }
}
which(resultsAIC == min(resultsAIC), arr.ind = TRUE)

res <- Arima(ima1,order=c(0,1,1))$res
Acf(res,main="",xlab="")

Box.test(res,type="Ljung")

library(prophet)
library(xts)
data(AirPassengers)
AP <- AirPassengers
df <- data.frame(ds = index(as.xts(AP)),y = coredata(as.xts(AP)))
m <- prophet(df)
future <- make_future_dataframe(m, 50, freq = 'm')
forecast <- predict(m, future)
plot(m, forecast)
m <- prophet(df, seasonality.mode = 'multiplicative')
future <- make_future_dataframe(m, 50, freq = 'm')
forecast <- predict(m, future)
plot(m, forecast)


getwd()
setwd("C:/Users/cs88416/Downloads")
data = read.table("airline.txt",header=TRUE)
attach(data)
y <- airline[1:132]
par(mfrow=c(2,2))
acf(y,main="Nível",96)
acf(diff(y),main="Primeira diferença",96)
acf(diff(diff(y),12),96,main='',ylab='ACF diff diff')
pacf(diff(diff(y),12),96,main='',ylab='PACF diff diff')

y <- airline[1:132]
fit1 = Arima(y,order=c(1,1,0),seasonal=list(order=c(1,1,0),period=12),method="ML")
fit2 = Arima(y,order=c(1,1,1),seasonal=list(order=c(1,1,0),period=12),method="ML")
fit3 = Arima(y,order=c(0,1,0),seasonal=list(order=c(0,1,0),period=12),method="ML")
fit4 = Arima(y,order=c(0,1,1),seasonal=list(order=c(0,1,0),period=12),method="ML")
fit5 = Arima(y,order=c(1,1,0),seasonal=list(order=c(1,1,1),period=12),method="ML")
fit6 = Arima(y,order=c(1,1,0),seasonal=list(order=c(0,1,0),period=12),method="ML")
aic  = c(fit1$aic,fit2$aic,fit3$aic,fit4$aic,fit5$aic,fit6$aic)
aicc = c(fit1$aicc,fit2$aicc,fit3$aicc,fit4$aicc,fit5$aicc,fit6$aicc)
bic  = c(fit1$bic,fit2$bic,fit3$bic,fit4$bic,fit5$bic,fit6$aicc)
cbind(aic,aicc,bic)


fit = Arima(y,order=c(1,1,0),seasonal=list(order=c(0,1,0),period=12),method="ML")
par(mfrow=c(1,2))
acf(residuals(fit),96,main="")
pacf(residuals(fit),96,main="")

plot(forecast(fit,12))
library(astsa)
sarima.for(y,n.ahead=12,1,1,0,0,1,0,12)
