am <- arima.sim(list(order=c(p=1,d=0,q=1),ar=0.7,ma=0.3),n=200)
plot(am)

acf(am)
pacf(am)


library(astsa)
acf2(am)

# AR(1)
ar1 <- arima.sim(list(order=c(p=1,d=0,q=0),
                      ar=0.75),n=200) + 40
acf2(ar1)

ar1.fit <- sarima(ar1,p=1,d=0,q=0)
ar1.fit$ttable

# AR(2)
ar2 <- arima.sim(list(order=c(p=2,d=0,q=0),
                      ar=c(1.7,-0.85)),n=200)  + 40
acf2(ar2)

ar2.fit <- sarima(ar2,p=2,d=0,q=0)
ar2.fit$ttable

# MA(1)
ma1 <- arima.sim(list(order=c(p=0,d=0,q=1),
                      ma=-0.85),n=200)  + 40
acf2(ma1)

ma1.fit <- sarima(ma1,p=0,d=0,q=1)
ma1.fit$ttable

# ARMA(1,0,1)
arma11 <- arima.sim(list(order=c(p=1,d=0,q=1),
                         ar=0.89,ma=-0.45), n=200)
acf2(arma11)

arma11.fit <- sarima(arma11,p=1,d=0,q=1)
arma11.fit$ttable




library(xts)


# Plot GNP series (gnp) and its growth rate
par(mfrow = c(2,1))
plot(gnp)
plot(diff(log(gnp)))

# Plot DJIA closings (djia$Close) and its returns
par(mfrow = c(2,1))
plot(djia$Close)
plot(diff(log(djia$Close)))

# Generate and plot white noise
WN <- arima.sim(model = list(order = c(0, 0, 0)), n = 200)
plot(WN)

# Generate and plot an MA(1) with parameter .9 by filtering the noise
MA <- arima.sim(model = list(order = c(0, 0, 1), ma = .9), n = 200)  
plot(MA)

# Generate and plot an AR(1) with parameters 1.5 and -.75
AR <- arima.sim(model = list(order = c(2, 0, 0), ar = c(1.5, -.75)), n = 200) 
plot(AR)

library(astsa)

# Simulation of AR(1) model
x <- arima.sim(model = list(order = c(1, 0, 0), ar = .85), n = 200) 

plot(x)

# P/ACF pair
acf2(x)

# Fitting and examining the results
xfit <- sarima(x,p=1,d=0,q=0)
xfit$ttable

# Simulation of AR(2) model
x <- arima.sim(model = list(order = c(2, 0, 0), ar = c(1.5, -.75)), n = 200)


plot(x)

# Fitting and examining the results
xfit <- sarima(x,p=2,d=0,q=0)
xfit$ttable

#OR
xfit1 <-arima(x,order = c(2,0,0))
xfit1$coef

# Simulation of MA(1) model
x <- arima.sim(model = list(order = c(0, 0, 1), ma = -.8), n = 200)

plot(x)

# Fitting and examining the results
xfit <- sarima(x,p=0,d=0,q=1)
xfit$ttable

# Simulation of ARMA(2,1) model
x <- arima.sim(model = list(order = c(2, 0, 1), ar = c(1, -.9), ma = .8), n = 250)

plot(x)

acf2(x)

xfit <- sarima(x,p=2,d=0,q=1)
xfit$ttable

# Fitting an appropriate model to time series varve
dl_varve <- diff(log(varve))

# Fit an MA(1) to dl_varve.   
sarima(dl_varve,p=0,d=0,q=1)

# Fit an MA(2) to dl_varve. Improvement?
sarima(dl_varve,p=0,d=0,q=2)

# Fit an ARMA(1,1) to dl_varve. Improvement?
sarima(dl_varve,p=1,d=0,q=1)


########Fitting to oil################

# Calculate approximate oil returns
oil_returns <- diff(log(oil))

# Plot oil_returns. Notice the outliers.
plot(oil_returns)

# Plot the P/ACF pair for oil_returns
acf2(oil_returns)

# Assuming both P/ACF are tailing, fit a model to oil_returns
sarima(oil_returns,p=1,d=0,q=1)

