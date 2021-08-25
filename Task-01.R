setwd("F:/Academic/Semester 07/Financial Econometrics/Take Home Assignment")
install.packages("fpp2")
install.packages("dynlm")
install.packages("strucchange")
install.packages("plyr")
library(dynlm)
library(stats)
library(forecast)
library(datasets)
library(tseries)
library("zoo")
library(fpp2)
library(dplyr)
library(readr)
library("strucchange")
library(vars)
library(plyr)

#load the selected dataset
Apple <- read.csv("AAPL (6).csv")

#Handling missing values
dates = data.frame(Date = seq(as.Date('2011-01-01'), as.Date('2021-03-01'), by = 'days'))
Apple$Date = as.Date(Apple$Date,"%Y-%m-%d")
Apple_full <- merge(Apple,dates,by="Date", all = T)
APPLE <-na.locf(Apple_full, fromLast = TRUE)
close_val=APPLE["Adj.Close"] ##Daily percentage changes in Apple Adj. Close value
Adjclose_log = log(close_val)
close_value<- ts(data = Adjclose_log, frequency = 365, start = c(2011,01,01))
plot.ts(close_value)
lockBinding('close_value',environment())

#Check the process is stationary or not
adf.test(close_value)

#Removing the trend
lm1 <- dynlm(close_val ~ trend(close_val), data = APPLE)
summary(lm1)
#the trend term and other component highly statistically significant. Therefore, there is a deterministic trend component here.

resid1 <- lm1$residuals #detrended Apple
adf.test(resid1)
adf.test(resid1, k=0)
resid1 %>% ggtsdisplay()
# process is still not stationary. there is a other matter


#Letting R select the breakpoints
ex_brk <- breakpoints(close_value ~ 1, h = 0.1)
summary(ex_brk)
#Minimum BIC and RSS is for 8 breaks so we will select 8 as the best number of structural break as
breakdates(ex_brk, breaks = 7)

plot(close_value)
lines(fitted(ex_brk, breaks = 7), col = 4)
lines(confint(ex_brk, breaks =7)) 


#Let's create dummies for the 7 episodes
APPLE$break1 <- 1
APPLE$break1[1:389] <- 0
APPLE$break2 <- 1
APPLE$break2[1:760] <- 0
APPLE$break3 <- 1
APPLE$break3[1:1239] <- 0
APPLE$break4 <- 1
APPLE$break4[1:1812] <- 0
APPLE$break5 <- 1
APPLE$break5[1:2226] <- 0
APPLE$break6 <- 1
APPLE$break6[1:2680] <- 0
APPLE$break7 <- 1
APPLE$break7[1:3269] <- 0


#Check whether removing the effect of the 8 structural breaks makes the series stationary
lm3 <- lm(close_value~break1+break2+break3+break4+break5+break6+break7, data=APPLE)
summary(lm3)
#All structural breaks are significant

resid3 <- lm3$residuals
adf.test(resid3)
adf.test(resid3,k=0)
 

#Differencing
d.close_value= diff(close_value,differences = 1)
adf.test(d.close_value)

#Identification
#Initial guesses for p,d and q
d.close_value %>% ggtsdisplay()
acf(d.close_value, lag =50 )
pacf(d.close_value, lag =50)
 #q=0
#p=,9,10,20

#Estimation of model selection
arima_1 <- Arima(d.close_value, order=c(9,0,0))
summary(arima_1)

arima_2 <- Arima(d.close_value, order=c(10,0,0))
summary(arima_2)

arima_3 <- Arima(d.close_value, order=c(20,0,0))
summary(arima_3)

arima_4 <- Arima(d.close_value,order = c(31,0,0))
summary(arima_4)

fit<- auto.arima(d.close_value, seasonal = FALSE)
fit
checkresiduals(fit)
plot(fit)


AIC(arima_1,arima_2,arima_3,arima_4)
BIC(arima_1,arima_2,arima_3,arima_4)

#Diagonistic checking 
#Check whether estimated residuals are white noise
checkresiduals(arima_1)
checkresiduals(arima_3)
checkresiduals(fit)

#LB test
#H0 : residual is white noise
#H1 : residual is not white noise


#Forecasting
#autoplot function
autoplot(forecast(arima_3))
#autoplot(forecast(fit))
#(around 30% of sample)  for testing
d.col.train <- window(close_value, end=c(2018,02))
d.col.test <- window(close_value, start=c(2018,03))

arima.train <- Arima(d.col.train, order=c(20,0,0))

#model accuracy
accuracy(forecast(arima.train, h=1113), d.col.test)

forecast <-forecast(arima.train, h=1113)

# Plot forecasts and test set
autoplot(close_value) +
  autolayer(forecast, series = "ARIMA(20,0,0)", alpha = 0.5) 


#Let's test for ARCH effects formally
install.packages("aTSA")
library(aTSA)

#First, let's fit a white noise model
M1 <- arima(d.close_value, order=c(20,0,0),include.mean=TRUE)
arch.test(M1, output=TRUE) 

#Now let's fit an ARCH model
install.packages("fGarch")
library(fGarch)
install.packages("rugarch")
library(rugarch)

model1 <- garchFit(~arma(20,0)+ garch(1,0), d.close_value,include.mean = TRUE, trace=FALSE)
model1 
summary(model1) #ARCH test suggests that there is some autocorrelation left in the residual
#LB test suggests that even though residual is white noise, the squared resid is not 

model1a <- garchFit( ~arma(20,0)+ garch(2,0), d.close_value, include.mean=TRUE, trace=FALSE)
summary(model1a)

model1b <- garchFit( ~arma(20,0)+garch(3,0), d.close_value, include.mean=TRUE, trace=FALSE)
summary(model1b) #At ARCH(3) we get squared residuals that are white noise

model1c <- garchFit( ~arma(20,0)+garch(4,0), d.close_value, include.mean=TRUE, trace=FALSE)
summary(model1c) #At ARCH(4) we get squared residuals that are white noise

model1d <- garchFit( ~arma(20,0)+garch(5,0), d.close_value, include.mean=TRUE, trace=FALSE)
summary(model1d)

model1e <- garchFit( ~arma(20,0)+garch(6,0), d.close_value, include.mean=TRUE, trace=FALSE)
summary(model1e)

model1f <- garchFit( ~arma(20,0)+garch(7,0), d.close_value, include.mean=TRUE, trace=FALSE)
summary(model1f)

model1g <- garchFit( ~arma(20,0)+garch(8,0), d.close_value, include.mean=TRUE, trace=FALSE)
summary(model1g)

model1h <- garchFit( ~arma(20,0)+garch(9,0), d.close_value, include.mean=TRUE, trace=FALSE)
summary(model1h)

model1i <- garchFit( ~arma(20,0)+garch(10,0), d.close_value, include.mean=TRUE, trace=FALSE)
summary(model1i)

model1j <- garchFit( ~arma(20,0)+garch(11,0), d.close_value, include.mean=TRUE, trace=FALSE)
summary(model1j)

model1k <- garchFit( ~arma(20,0)+garch(12,0), d.close_value, include.mean=TRUE, trace=FALSE)
summary(model1k)

model1l <- garchFit( ~arma(20,0)+garch(13,0), d.close_value, include.mean=TRUE, trace=FALSE)
summary(model1l)

#Let's go for a GARCH model
model2 <- garchFit(~arma(20,0)+garch(1,1), d.close_value, include.mean=TRUE, trace=FALSE)
model2 

#both alpha1 and beta1 are statistically significant 

summary(model2) 

#with GARCH(1,1), we get the resids and sq resids to be white noise

prediction <- predict(model2, n.ahead=2, plot=TRUE)

#GJR GARCH
gjrgarch1 <- garchFit(~arma(20,2)+garch(1,1), leverage=T,d.close_value,trace=F,include.mean=T)
summary(gjrgarch1)

#Gamma is negative but insignificant 
#E-GARCH
egarch1 <- ugarchfit(ugarchspec(mean.model=list(armaOrder=c(20,0),include.mean=T),variance.model=list(model="eGARCH",garchOrder=c(1,1))),d.close_value)
egarch1 
#Only beta1 is significant 




 

