#第一章
library(astsa)
library(data.table)
library(readxl)
library(xts)
library(plotly)
#library(ggplot2)

setwd("C:/Users/ACER/Desktop/时间序列R/Time-Series-Analysis")


# #Example 1.1 Johnson & Johnson Quarterly Earnings
# plot(jj, type="o", ylab="Quarterly Earnings per Share")

# #Example 1.4 Dow Jones Industrial Average
# library(xts)
# djiar = diff(log(djia$Close))[-1] # approximate returns
# plot(djiar, main="DJIA Returns", type="n")
# lines(djiar)

##########例1：数据的一些基本常识
#White Noise and Moving Averages
w = rnorm(500,0,1) # 500 N(0,1) variates
v = filter(w, sides=2, filter=rep(1/3,3)) # moving average
par(mfrow=c(2,1))
plot.ts(w, main="white noise")
plot.ts(v, ylim=c(-3,3), main="moving average")

#Random Walk with Drift
set.seed(154) # so you can reproduce the results
w = rnorm(200); x = cumsum(w) # two commands in one line
wd = w +.2; xd = cumsum(wd)
plot.ts(xd, ylim=c(-5,55), main="random walk", ylab='')
lines(x, col=4); abline(h=0, col=4, lty=2); abline(a=0, b=.2, lty=2)

#Signal in Noise
cs = 2*cos(2*pi*1:500/50 + .6*pi); w = rnorm(500,0,1)
par(mfrow=c(3,1), mar=c(3,2,2,1), cex.main=1.5)
plot.ts(cs, main=expression(2*cos(2*pi*t/50+.6*pi)))
plot.ts(cs+w, main=expression(2*cos(2*pi*t/50+.6*pi) + N(0,1)))
plot.ts(cs+5*w, main=expression(2*cos(2*pi*t/50+.6*pi) + N(0,25)))


###########例2：互相关和样本ACF
#Prediction Using Cross-Correlation
#xt领先于yt，在正侧有一个峰值，如果xt落后于yt，将在负侧有一个峰值
x = rnorm(100)
y = lag(x, -5) + rnorm(100)
ccf(y, x, ylab='CCovF', type='covariance')

# Sample ACF and Scatterplots
(r = round(acf(soi, 6, plot=FALSE)$acf[-1], 3)) # first 6 sample acf values
#[1] 0.604 0.374 0.214 0.050 -0.107 -0.187
par(mfrow=c(1,2))
plot(lag(soi,-1), soi); legend('topleft', legend=r[1])
plot(lag(soi,-6), soi); legend('topleft', legend=r[6])


############例3：预白化和互相关分析
#Prewhitening and Cross Correlation Analysis
set.seed(1492)
num=120; t=1:num
X = ts(2*cos(2*pi*t/12) + rnorm(num), freq=12)
Y = ts(2*cos(2*pi*(t+5)/12) + rnorm(num), freq=12)
Yw = resid( lm(Y~ cos(2*pi*t/12) + sin(2*pi*t/12), na.action=NULL) )
par(mfrow=c(3,2), mgp=c(1.6,.6,0), mar=c(3,3,1,1) )
plot(X)
plot(Y)
acf(X,48, ylab='ACF(X)')
acf(Y,48, ylab='ACF(Y)')
ccf(X,Y,24, ylab='CCF(X,Y)')
ccf(X,Yw,24, ylab='CCF(X,Yw)', ylim=c(-.6,.6))


##########例4：中国CPI和GDP的数据图像
#中国CPI
CPI <- read_excel("CPI.xls")
CPI$截止日期_EndDt <- as.Date(CPI$截止日期_EndDt)
# 将数据框转换为xts对象
CPI <- xts(CPI[, -which(names(CPI) == "截止日期_EndDt")], order.by = CPI$截止日期_EndDt)
#整体
par(mfrow=c(2,1))
# "居民消费价格指数CPI_当月同比(上年同月=100)_CPI_YoY" 
#"居民消费价格指数CPI_环比(上月=100)_CPI_MoM"
#"居民消费价格指数CPI_累计同比(上年同期=100)_CPI_CumuYear"
ts.plot(CPI[,1:3], col=1:3, ylab="num", main="CPI")
#"CPI_翘尾因素_当月(%)_CPI_Hike"
#"CPI-PPI_差值_当月(%)_CPIPPI_Dval" 
#"CPI_新涨价因素_当月(%)_CPI_NewPricFact"
ts.plot(CPI[,4:6], col=1:3, ylab="num", main="other")

#单独CPI
CPI_YoY <- CPI$`居民消费价格指数CPI_当月同比(上年同月=100)_CPI_YoY`
saveRDS(CPI_YoY, file = "CPI.rds")
plot(CPI_YoY, main="居民消费价格指数CPI_当月同比(上年同月=100)", type="n")
par(mfrow=c(3,1))
lines(CPI_YoY)
plot(CPI_YoY, type="o", ylab="居民消费价格指数CPI_当月同比(上年同月=100)")

#Moving Averages
v = filter(CPI_YoY, sides=2, filter=rep(1/3,3))
plot.ts(v, ylim=c(90,140), main="moving average")

#中国GDP
GDP <- read_excel("年度数据.xls",col_names = F)
GDP<- t(GDP)
colnames(GDP) <- GDP[1, ]  # 将第一行设置为列名
GDP <- GDP[-1, ]
GDP[,1] <- as.integer(GDP[,1])
GDP <- data.table(GDP)
setkeyv(GDP,c("指标"))
colnames(GDP)[1] <- "date"
GDP_1 <- GDP[,c(1,3)]
GDP_1$`国内生产总值指数(1978年=100)` <- as.numeric(GDP_1$`国内生产总值指数(1978年=100)`)
saveRDS(GDP_1, file = "GDP.rds")
# Date <- as.Date(paste(rownames(GDP), "-12-31", sep="")) 
# GDP <- xts(GDP, order.by = Date)
# 
# GDP_1 <- GDP$`国内生产总值指数(1978年=100)`
# GDP_1 <- xts(GDP_1, order.by = Date)
# plot(GDP_1, type="o", ylab="国内生产总值指数(1978年=100)")
# plot(GDP_1, main="国内生产总值指数(1978年=100)", type="n")
# lines(GDP_1)
par(mfrow=c(2,1))
plot(GDP_1$date, GDP_1$`国内生产总值指数(1978年=100)`, type = "o", col = "blue",
     xlab = "年份", ylab = "国内生产总值指数(1978年=100)", main = "国内生产总值指数趋势")
plot(GDP_1$date, GDP_1$`国内生产总值指数(1978年=100)`, type = "n", col = "blue",
     xlab = "年份", ylab = "国内生产总值指数(1978年=100)", main = "国内生产总值指数趋势")
lines(GDP_1$date, GDP_1$`国内生产总值指数(1978年=100)`)


# 绘制动态折线图 （可鼠标交互查看具体年份的 GDP 指数值）
fig <- plot_ly(GDP_1, x = ~date, y = ~`国内生产总值指数(1978年=100)`, type = 'scatter', mode = 'lines+markers',
               line = list(color = 'blue', width = 2),
               marker = list(color = 'red', size = 6)) %>%
  layout(title = "国内生产总值指数趋势",
         xaxis = list(title = "年份"),
         yaxis = list(title = "GDP指数"))
fig
