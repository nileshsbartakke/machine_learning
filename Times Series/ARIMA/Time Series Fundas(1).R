library(forecast)

BUNDESBANK <- read.csv("F:/Statistics/Datasets/BUNDESBANK-BBK01_WT5511.csv")
BUNDESBANK_ts <- ts(BUNDESBANK$Value, start=c(1968,4),frequency = 12)

start(BUNDESBANK_ts) # Start Time Point
end(BUNDESBANK_ts) # End Time Point
# Fraction of time between the observations, for monthly - 1/12, for quarterly - 1/4
deltat(BUNDESBANK_ts) 

stat <- arima.sim(model=list(order=c(0,0,0)), n=50)
ts.plot(stat)

nonstat <- arima.sim(model=list(order=c(0,3,0)),n=50)
ts.plot(nonstat)

######################################################

wn <- arima.sim(model=list(order=c(0,0,0)), n = 50)
ts.plot(wn) # Mean=0 and SD=1 by default
abline(h=0, lty=3, col="red")
mean(wn)
sd(wn)

wn <- arima.sim(model = list(order=c(0,0,0)), n = 50, mean=23, sd=10)
ts.plot(wn)
abline(h=23, lty=3, col="red")
mean(wn)
sd(wn)

rw <- arima.sim(model=list(order=c(0,1,0)), n=50)
ts.plot(rw)

rw_diff <- diff(rw)
ts.plot(rw_diff)
abline(h=0, lty=3, col="red")
mean(rw_diff)
sd(rw_diff)


rw_drift <- arima.sim(model = list(order = c(0, 1, 0)), n = 50, mean = 5)
ts.plot(rw_drift)


rw_drift_diff <- diff(rw_drift) 
ts.plot(rw_drift_diff)
abline(h=5, lty=3, col="red")
mean(rw_drift_diff)
sd(rw_drift_diff)

acf(JohnsonJohnson,10, plot = F)

acf(JohnsonJohnson,10)

acf(JohnsonJohnson,lag.max = 1)

#########################################################
#####AR Processes
########################################################

x <- arima.sim(model = list(ar=0.4), n = 100)
y <- arima.sim(model = list(ar=0.8), n = 100)
z <- arima.sim(model = list(ar=-0.9), n = 100)
plot.ts(cbind(x, y, z), main="x(phi=0.4),y(phi=0.8),z(phi=-0.9)")

par(mfcol=c(3,1))
acf(x)
acf(y)
acf(z)

par(mfcol=c(1,1))

# Simulating AR model to x
arima(x, order = c(1,0,0))

AR <- arima(AirPassengers, order = c(1,0,0))
print(AR)
ts.plot(AirPassengers)
AR_fitted <- AirPassengers - residuals(AR)
points(AR_fitted, type = "l", col = 2, lty = 2)

predict(AR)
predict(AR,n.ahead = 6)

# Simulating the MA model
x <- arima.sim(model = list(ma=0.9), n = 100)
y <- arima.sim(model = list(ma=-0.9), n = 100)
z <- arima.sim(model = list(ma=0.01), n = 100)

plot.ts(cbind(x, y, z), main="x(theta=0.9),y(theta=-0.9),z(theta=0)")

par(mfcol=c(3,1))
acf(x)
acf(y)
acf(z)

par(mfcol=c(1,1))

# Simulating AR model to x
arima(x, order = c(0,0,1))

MA <- arima(Nile, order = c(0,0,1))
print(MA)
ts.plot(Nile)
MA_fit <- Nile - residuals(MA)
points(MA_fit, type = "l", col = 2, lty = 2)

#Predicting for MA

predict(MA)
predict(MA,n.ahead = 6)
