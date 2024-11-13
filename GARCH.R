library(astsa)
library(data.table)
library(readxl)
library(xts)
library(fGarch)

#Analysis of U.S. GNP
u = sarima(diff(log(gnp)), 1, 0, 0)
acf2(resid(u$fit)^2, 20)

summary(garchFit(~arma(1,0)+garch(1,0), diff(log(gnp))))



#Example 5.5 ARCH Analysis of the DJIA Returns
library(xts)
djiar = diff(log(djia$Close))[-1]
acf2(djiar) # exhibits some autocorrelation (not shown)
acf2(djiar^2) # oozes autocorrelation (not shown)
library(fGarch)
summary(djia.g <- garchFit(~arma(1,0)+garch(1,1), data=djiar,
                           cond.dist='std'))
plot(djia.g) # to see all plot options

summary(djia.ap <- garchFit(~arma(1,0)+aparch(1,1), data=djiar,
                            cond.dist='std'))
plot(djia.ap) # to see all plot options (none shown)
