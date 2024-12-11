library(astsa)
library(data.table)
library(readxl)
library(xts)
library(vars)

# 假设cmort, tempr, part是已经加载的数据集，分别代表心血管死亡率、温度和颗粒物水平
# 将这些数据组合成一个矩阵
x <- cbind(cmort, tempr, part)
summary(VAR(x, p=1, type='both')) #1阶

#筛选阶数
VARselect(x, lag.max=10, type="both")$selection
#BIC选择了p = 2阶模型，AIC和FPE选择了p = 9阶模型，Hannan-Quinn选择了p = 5阶模型。

VARselect(x, lag.max=10, type="both")$criteria

summary(fit <- VAR(x, p=2, type="both")) #2阶
acf(resid(fit), 52)#残差的相互关系图
serial.test(fit, lags.pt=12, type="PT.adjusted") #多元Q-test

#预测
(fit.pr = predict(fit, n.ahead = 24, ci = 0.95))
fanchart(fit.pr)


#数据集换为EuStockMarkets：欧洲股市四个主要指标每日收盘价指数：德国DAX（Ibis）、瑞士SMI、法国CAC和英国FTSE
dat <- EuStockMarkets
VARselect(dat, lag.max=20, type="both")$selection#BIC选择了p = 1阶模型
summary(fit2 <- VAR(dat, p=1, type="both")) #1阶
(fit.pr2 = predict(fit2, n.ahead = 24, ci = 0.95))
fanchart(fit.pr2)



#The Spliid Algorithm拟合曲线
library(marima)
model = define.model(kvar=3, ar=c(1,2), ma=c(1))
arp = model$ar.pattern; map = model$ma.pattern
cmort.d = resid(detr <- lm(cmort~ time(cmort), na.action=NULL))
xdata = matrix(cbind(cmort.d, tempr, part), ncol=3) # strip ts attributes
fit = marima(xdata, ar.pattern=arp, ma.pattern=map, means=c(0,1,1),
             penalty=1)
# resid analysis (not displayed)
innov = t(resid(fit)); plot.ts(innov); acf(innov, na.action=na.pass)
# fitted values for cmort
pred = ts(t(fitted(fit))[,1], start=start(cmort), freq=frequency(cmort)) +
  detr$coef[1] + detr$coef[2]*time(cmort)
plot(pred, ylab="Cardiovascular Mortality", lwd=2, col=4); points(cmort)
# print estimates and corresponding t^2-statistic
short.form(fit$ar.estimates, leading=FALSE)
short.form(fit$ar.fvalues, leading=FALSE)
short.form(fit$ma.estimates, leading=FALSE)
short.form(fit$ma.fvalues, leading=FALSE)