#Simulated AR(1) models: φ = .9 (top); φ = −.9 (bottom)
par(mfrow=c(2,1))
plot(arima.sim(list(order=c(1,0,0), ar=.9), n=100), ylab="x",
     main=(expression(AR(1)~~~phi==+.9)))
plot(arima.sim(list(order=c(1,0,0), ar=-.9), n=100), ylab="x",
     main=(expression(AR(1)~~~phi==-.9)))

#Simulated MA(1) models: θ = .9 (top); θ = −.9 (bottom)
par(mfrow = c(2,1))
plot(arima.sim(list(order=c(0,0,1), ma=.9), n=100), ylab="x",
     main=(expression(MA(1)~~~theta==+.5)))
plot(arima.sim(list(order=c(0,0,1), ma=-.9), n=100), ylab="x",
     main=(expression(MA(1)~~~theta==-.5)))

#Example 3.11 
#The ACF and PACF of an AR(2) model with φ1 = 1.5 and φ2 = −.75
z = c(1,-1.5,.75) # coefficients of the polynomial
(a = polyroot(z)[1]) # print one root = 1 + i/sqrt(3)
arg = Arg(a)/(2*pi) # arg in cycles/pt
1/arg # the pseudo period
#To reproduce Fig. 3.4:
set.seed(8675309)
ar2 = arima.sim(list(order=c(2,0,0), ar=c(1.5,-.75)), n = 144)
plot(ar2, axes=FALSE, xlab="Time")
axis(2); axis(1, at=seq(0,144,by=12)); box()
abline(v=seq(0,144,by=12), lty=2)
#To calculate and display the ACF for this model:
ACF = ARMAacf(ar=c(1.5,-.75), ma=0, 50)
plot(ACF, type="h", xlab="lag")
abline(h=0)

#Fig. 3.5
ACF = ARMAacf(ar=c(1.5,-.75), ma=0, 24)[-1]
PACF = ARMAacf(ar=c(1.5,-.75), ma=0, 24, pacf=TRUE)
par(mfrow=c(1,2))
plot(ACF, type="h", xlab="lag", ylim=c(-.8,1)); abline(h=0)
plot(PACF, type="h", xlab="lag", ylim=c(-.8,1)); abline(h=0)

#Example 3.18 Preliminary Analysis of the Recruitment Series
acf2(rec, 48) # will produce values and a graphic
(regr = ar.ols(rec, order=2, demean=FALSE, intercept=TRUE))
regr$asy.se.coef # standard errors of the estimates


#Example 3.25 Forecasting the Recruitment Series 
regr = ar.ols(rec, order=2, demean=FALSE, intercept=TRUE)
fore = predict(regr, n.ahead=24)
ts.plot(rec, fore$pred, col=1:2, xlim=c(1980,1990), ylab="Recruitment")
U = fore$pred+fore$se; L = fore$pred-fore$se
xx = c(time(U), rev(time(U))); yy = c(L, rev(U))
polygon(xx, yy, border = 8, col = gray(.6, alpha = .2))
lines(fore$pred, type="p", col=2)

#Example 3.26 Backcasting an ARMA(1, 1)
set.seed(90210)
x = arima.sim(list(order = c(1,0,1), ar =.9, ma=.5), n = 100)
xr = rev(x) # xr is the reversed data
pxr = predict(arima(xr, order=c(1,0,1)), 10) # predict the reversed data
pxrp = rev(pxr$pred) # reorder the predictors (for plotting)
pxrse = rev(pxr$se) # reorder the SEs
nx = ts(c(pxrp, x), start=-9) # attach the backcasts to the data
plot(nx, ylab=expression(X[~t]), main='Backcasting')
U = nx[1:10] + pxrse; L = nx[1:10] - pxrse
xx = c(-9:0, 0:-9); yy = c(L, rev(U))
polygon(xx, yy, border = 8, col = gray(0.6, alpha = 0.2))
lines(-9:0, nx[1:10], col=2, type='o')

#Example 3.28

#Example 3.36 Bootstrapping an AR(1)




