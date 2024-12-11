library(astsa)
library(data.table)
library(readxl)
library(xts)
library(fGarch)

setwd("C:/Users/ACER/Desktop/时间序列R/Time-Series-Analysis")
#Analysis of U.S. GNP
u = sarima(diff(log(gnp)), 1, 0, 0)
acf2(resid(u$fit)^2, 20)

summary(garchFit(~arma(1,0)+garch(1,0), diff(log(gnp))))



#Example 5.5 GARCH Analysis of the DJIA Returns
djiar = diff(log(djia$Close))[-1]
acf2(djiar) # exhibits some autocorrelation
acf2(djiar^2) # oozes autocorrelation

summary(djia.g <- garchFit(~arma(1,0)+garch(1,1), data=djiar,
                           cond.dist='std'))
plot(djia.g) # to see all plot options


#APARCH分析
summary(djia.ap <- garchFit(~arma(1,0)+aparch(1,1), data=djiar,
                            cond.dist='std'))
plot(djia.ap) # to see all plot options选择0退出



#沪深300的ARCH分析
csi300 <-read.table('csi300.csv',sep=',',head=T)
csi300 <- csi300[,c("date","close")]
csi300$date <- as.Date(csi300$date)
# 将数据框转换为xts对象
csi300_xts <- xts(csi300[, -which(names(csi300) == "date")], order.by = csi300$date)
return <- diff(log(csi300_xts))[-1]
summary(return.g <- garchFit(~arma(1,0)+aparch(1,1), data=return,
                           cond.dist='std'))
plot(return.g)
