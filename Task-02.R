setwd("F:/Academic/Semester 07/Financial Econometrics/Take Home Assignment")
newdata<- read.csv('newdata1.csv')
install.packages("AER")
install.packages("car")
install.packages("vars")
library(AER) #for `ivreg()`
library(lmtest) #for `coeftest()` and `bptest()`.
library(car) #for `hccm()` robust standard errors
library(sandwich)
library(vars)


close <- ts(data = newdata$Adj.Close, frequency = 12, start = c(1992,1))
GDP <- ts(data = newdata$Monthly.Real.GDP.Index, frequency = 12, start = c(1992,1))
S.P <- ts(data = newdata$S.P.500.Adj.close, frequency = 12, start = c(1992,1))
inflation <- ts(data = newdata$Inflation, frequency = 12, start = c(1992,1))

par(mfrow = c(4, 1))
plot.ts(close, ylab = expression(paste("Adj.Close")))
plot.ts(GDP, ylab = expression(paste("Monthly.Real.GDP.Index")))
plot.ts(S.P, ylab = expression(paste("S.P.500.Adj.close")))
plot.ts(inflation, ylab = expression(paste("Inflation")))

PP.test(close)
d.close=diff(close,differences = 1)
PP.test(d.close)

PP.test(GDP)
d.GDP=diff(GDP,differences = 1)
PP.test(d.GDP)

PP.test(S.P)
d.SP=diff(S.P,differences = 1)
PP.test(d.SP)

PP.test(inflation)

#Set the VAR dataset
c1 <- cbind(d.close, d.GDP, d.SP ,inflation)
c1 <- c1[-1,]
colnames(c1) <- cbind("D.close","D.GDP","D.SP","Inflation")

#Lag selection
lagselect <- VARselect(c1, lag.max = 15, type = "const")
lagselect$selection #let's go for 2 lags
#Outputs given are based on 4 criteria: Akaike, Hannan-Quinn, Schwarz, and Final Prediction Error

#Model estimation
Model1 <- VAR(c1, p = 2, type = "const", season = NULL, exog = NULL) 
summary(Model1)

#Granger causality - easier to have individual equations estimated
cause.ex <- causality(Model1, cause = "D.close")
cause.ex
#Evidence that changes in exchange rates do lead to future changes in imports and exports
#Some evidence (10%) that instantaneous changes to exchange cause changes in imports and exports

cause.m <- causality(Model1, cause = "D.GDP")
cause.m

cause.x <- causality(Model1, cause = "D.SP")
cause.x

cause.y <- causality(Model1, cause = "Inflation")
cause.y

#Let's generate impulse response functions for a shock to imports

GDPirf <- irf(Model1, impulse = "D.GDP", response = "D.GDP", n.ahead = 40, boot = TRUE)
plot(GDPirf, ylab = "D.GDP", main = "GDP shock to GDP")

Closeirf <- irf(Model1, impulse = "D.GDP", response = "D.close", n.ahead = 40, boot = TRUE)
plot(Closeirf, ylab = "close", main = "GDP shock to close")

SPirf <- irf(Model1, impulse = "D.GDP", response = "D.SP", n.ahead = 40, boot = TRUE)
plot(SPirf, ylab = "S.P", main = "GDP shock to S.P")

Inflationirf <- irf(Model1, impulse = "D.GDP", response = "Inflation", n.ahead = 40, boot = TRUE)
plot(Inflationirf, ylab = "Inflation", main = "GDP shock to Inflation")

#Variance decomposition
bv.vardec <- fevd(Model1, n.ahead = 10)
plot(bv.vardec)

#Adding contemporaneous effects using a structural VAR - let's assume exchange rates change first (unaffected by changes in others) and exports change last (affected by changes in others)
# Estimate structural coefficients
a <- diag(1, 4)
a[lower.tri(a)] <- NA
a

svar_est <- SVAR(Model1, Amat = a, max.iter = 1000)

#Contemporaneous effects
svar_est
svar_est$Ase # standard errors 








