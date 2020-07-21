install.packages("quantmod")
install.packages("fBasics")
install.packages("ecoseries")

library(quantmod) # Load the package

getSymbols("AAPL") # Download daily prices of Apple stock from Yahoo
head(AAPL)
tail(AAPL)
barChart(AAPL)

plot(AAPL$AAPL.Adjusted)

chartSeries(AAPL$AAPL.Adjusted)


AAPL.rtn = diff(log(AAPL$AAPL.Adjusted))[-1] # Compute log returns

plot(AAPL.rtn)
##################

getSymbols("UNRATE",src="FRED") #Download unemployment rates from FRED.
chartSeries(UNRATE,theme="white")

library(fBasics)
basicStats(AAPL.rtn)


# Commands for individual moments
mean(AAPL.rtn)
var(AAPL.rtn)
stdev(AAPL.rtn) 
sd(AAPL.rtn)

# Simple tests
t.test(AAPL.rtn) # Testing mean return = 0
# Na média, comprar Apple dá dinheiro

hist(AAPL.rtn,nclass=30, breaks = 50,freq = FALSE) # Histogram


x=seq(-.1,.1,.001) # Create a sequence of x with increment 0.001.
# The next command creates normal density
y1=dnorm(x,mean(AAPL.rtn),stdev(AAPL.rtn))
d1=density(AAPL.rtn)
plot(d1$x,d1$y,xlab='ret',ylab='density',type='l',lwd=2)
lines(x,y1,lty=2, col=2, lwd=2)

# TESTE DE RAÍZ UNITÁRIA  - Inflação

getSymbols('CPIAUCSL', src = "FRED")

cpi = window(ts(CPIAUCSL, start=c(1947,01), freq=12),
             start=c(1994,07))
plot(cpi)

inflation = (cpi/lag(cpi,-1)-1)*100
inflation12 = (cpi/lag(cpi,-12) - 1)*100


plot(inflation)
plot(inflation12)

#install.packages('ecoseries')
library(ecoseries)

#ecoseries: An R Interface to Brazilian Central Bank and Sidra APIs and the IPEA Data

ipca = window(ts(series_ipeadata('36482', 
                                 periodicity = 'M')$serie_36482$valor,
                 start=c(1979,12), freq=12), start=c(1994,07))


plot(ipca)

inflacao = (ipca/lag(ipca,-1)-1)*100
inflacao12 = (ipca/lag(ipca,-12) - 1)*100


plot(inflacao)
plot(inflacao12)


library(tseries)

acf(cpi)
acf(ipca)



adf.test(cpi)
adf.test(ipca)


acf(inflation)
acf(inflation12)

adf.test(inflation)
adf.test(inflation12)


acf(inflacao)
acf(inflacao12)

adf.test(inflacao)
adf.test(inflacao12)


log_ipca = log(ipca)
log_inflacao = diff(log_ipca)

adf.test(log_ipca)
adf.test(log_inflacao)

# Sua vez: teste de raiz unitária para dados de COVID-19

require(dplyr)

library(readxl)

library(httr)

library(forecast)

#create the URL where the dataset is stored with automatic updates every day

url <- paste("https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-",format(Sys.time(), "%Y-%m-%d"), ".xlsx", sep = "")

#download the dataset from the website to a local temporary file

GET(url, authenticate(":", ":", type="ntlm"), write_disk(tf <- tempfile(fileext = ".xlsx")))

#read the Dataset sheet into "R"

data <- read_excel(tf)

br = data[data$`countriesAndTerritories` %in% c('Brazil'),]

br = br %>% arrange(-row_number())

ts.plot(br$cases, lwd=2)

br_cases = cumsum(br$cases)[cumsum(br$cases)>0]
br_deaths = cumsum(br$deaths)[cumsum(br$deaths)>0]

ts.plot(br_cases, main='Cases', ylab='', lwd=2)
ts.plot(br_deaths, main='Deaths', ylab='',lwd=2)

adf.test(br_cases)

dif_cases <-  diff(br_cases)
adf.test(dif_cases)

dif_dif_cases <- diff(dif_cases)
adf.test(dif_dif_cases)


adf.test(br_deaths)

dif_deaths <-  diff(br_deaths)
adf.test(dif_deaths)

dif_dif_deaths <- diff(dif_deaths)
adf.test(dif_dif_deaths)


par(mfrow=c(1,1))
plot(co2)

acf(co2)

adf.test(co2)
adf.test(diff(co2))

# Decomposição entre erro, tendência e sazonalidade
plot(decompose(co2, type = "mult"))

co2_decom <- decompose(co2, type = "mult")
co2_trend = co2_decom$trend
co2_seasonal <- co2_decom$seasonal

ts.plot(cbind(co2_trend, co2_trend * co2_seasonal), lty = 1:2, col=1:2,lwd=2)
####################################


vendas = read.csv('PMC.csv', header = TRUE, sep=';')
vendas = ts(vendas[,2], start = c(2003,01), frequency = 12)


par(mfrow=c(1,1))
plot(vendas, col=2)


vendas_decomp <- decompose(vendas, type = "mult")
plot(vendas_decomp)

adf.test(vendas)
adf.test(diff(vendas))

acf(vendas)
acf(diff(vendas))

adf.test(vendas)
adf.test(diff(vendas))

acf(vendas)
acf(diff(vendas))



library(forecast)

# Decomposição entre erro, tendência e sazonalidade
plot(decompose(vendas, type = "mult"))
plot(decompose(vendas, type = "add"))


vendas_decom <- decompose(vendas, type = "mult")
plot(vendas_decom)

vendas_trend = vendas_decom$trend
vendas_seasonal <- vendas_decom$seasonal

ts.plot(cbind(vendas_trend, vendas_trend * vendas_seasonal), lty = 1:2, col=1:2, lwd=2)





# Uma outra forma de ver é pela variação anual, o que "expurga" o efeito da sazonalidade de Natal

vendas_yoy = vendas/Lag(vendas,-12) - 1

plot(vendas_yoy, col=4, lwd=2, main = 'Vendas no Varejo - % YoY', ylab='')
abline(h=0 , lty=2)


acf(vendas_yoy)
adf.test(vendas_yoy)


vendas_mom = vendas/Lag(vendas,-1) - 1

plot(vendas_mom, col=4, lwd=2, main = 'Vendas no Varejo - % MoM', ylab='')
abline(h=0 , lty=2, lwd=2)


acf(vendas_mom)
adf.test(vendas_mom)

########################################

set.seed(123)

X <- 0.5*c(1:200)+rnorm(200,mean=0,sd = .25)
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
ts.plot(X,ylab="y1")
lines(trend,col=2)
ts.plot(resX,ylab="res y1",main="Residuo é RB")
acf(resX,main="")
acf(resX,type="partial",main="")

ts.plot(y,ylab="y2")
lines(trend,col=2)
ts.plot(resY,ylab="res y2",main="Residuo não é RB")
acf(resY,main="")
acf(resY,type="partial",main="")



#####################





set.seed(123)
##  AR(1)

ar1 = function(n,phi1){
  y = rep(0,2*n)
  for (t in 2:(2*n)){
    y[t] = phi1*y[t-1] + rnorm(1,0,1)
  }
  y = y[(n+1):(2*n)]
}



serie = ar1(1000,0.5)
fit = Arima(ar1(1000,0.5), order=c(1,0,0))
par(mfrow=c(1,1))
plot(fit$residuals)




phis = c(0.1, 0.3, 0.4, 0.6, 0.8, 0.9, 0.95, 0.99, 1)
par(mfrow=c(3,3))

# PLOTANDO AR(1) PARA DIFERENTES PHIS
for (i in phis){
  ts.plot(ar1(1000,i))
  
}








#  MA(1)
ma1 = function(n,theta1){
  y = rep(0,2*n)
  eps = rep(0,2*n)
  eps[1] = rnorm(1,0,1)
  
  for (t in 2:(2*n)){
    eps[t] = rnorm(1,0,1)
    y[t] = theta1*eps[t-1] + eps[t]
  }
  y = y[(n+1):(2*n)]
}


#  MA(2)
ma2 = function(n,theta1, theta2){
  y = rep(0,2*n)
  eps = rep(0,2*n)
  eps[1] = rnorm(1,0,1)
  eps[2] = rnorm(1,0,1)
  
  for (t in 3:(2*n)){
    eps[t] = rnorm(1,0,1)
    y[t] = theta1*eps[t-1] + theta2*eps[t-2] + eps[t]
  }
  y = y[(n+1):(2*n)]
}



# PLOTANDO MA(1)
par(mfrow=c(1,1))
ts.plot(ma1(100,0.8),ylab = "")

# PLOTANDO MA(2)
par(mfrow=c(1,1))
ts.plot(ma2(100,0.3, 0.2),ylab = "")



# PLOTANDO AUTOCORRELATION FUNCTION DE UM AR(1) PARA DIFERENTES PHIS
par(mfrow=c(2,3))
for (i in c(0.1, 0.6, 0.95, 0.99,1)){
  acf(ar1(1000,i),50 )
  
}


# PLOTANDO AUTOCORRELATION FUNCTION DE UM MA(1) PARA DIFERENTES THETAS
par(mfrow=c(2,2))
for (i in c(0.1, 0.6, 0.95, 0.99)){
  acf(ma1(1000,i),50 )
  
  
}






# PLOTANDO AUTOCORRELATION FUNCTION DE UM AR(1) PARA DIFERENTES PHIS
par(mfrow=c(2,3))
for (i in c(0.1, 0.6, 0.95, 0.99,1)){
  acf(ar1(1000,i),50 )
  
}


# PLOTANDO AUTOCORRELATION FUNCTION DE UM MA(1) PARA DIFERENTES THETAS
par(mfrow=c(2,2))
for (i in c(0.1, 0.6, 0.95, 0.99)){
  acf(ma1(1000,i),50 )
  
  
}


fit = Arima(ma1(100000,0.7), order=c(0,0,1))
plot(fit$residuals)

# PLOTANDO AUTOCORRELATION FUNCTION DE UM MA(2) 
# (note como agora a ACF é truncada no lag 2 em vez do lag 1)

par(mfrow=c(1,2))
acf(ma2(1000,0.6, 0.7),50 )
pacf(ma2(1000,0.6, 0.7),50)


coef = matrix(NA, 100,2 )
for (i in 1:100){
  fit = Arima(ma2(10*i,0.6, 0.7), order=c(0,0,2))
  coef[i,] = fit$coef[1:2]
}


par(mfrow=c(1,2))
ts.plot(coef[,1], ylab=expression(theta[1]), xlab='Sample Size/10', lwd=2)
abline(h=0.6, lty=2,col=2, lwd=2)

ts.plot(coef[,2], ylab=expression(theta[2]), xlab='Sample Size/10', lwd=2)
abline(h=0.7, lty=2,col=2, lwd=2)

fit = Arima(ma2(1000,0.6, 0.7), order=c(0,0,2))
plot(fit$residuals)

