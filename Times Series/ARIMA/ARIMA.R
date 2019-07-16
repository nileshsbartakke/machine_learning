library(fpp2)
data(package="fpp2")

data("debitcards")
plot(debitcards)

acf2(wmurders)
dbt.fit <- sarima(wmurders,p=1,d=0,q=0)
c(AIC=dbt.fit$AIC,BIC=dbt.fit$BIC)
dbt.fit$ttable

acf2(wmurders)
acf2(diff(wmurders))
dbt.fit <- sarima(wmurders,p=1,d=1,q=0)
c(AIC=dbt.fit$AIC,BIC=dbt.fit$BIC)
dbt.fit$ttable



library(astsa)
# Simulating ARIMA(2,1,0) model with drift 2
x <- arima.sim(model = list(order = c(2, 1, 0),
                            ar=c(1.5,-0.75)), n = 250, mean = 2)

x.fit <- sarima(x, p = 2, d = 1, q = 0)

x.fit$ttable
acf2(x)
acf2(diff(x))


data("globtemp")
plot(globtemp)
acf2(globtemp)
acf2(diff(globtemp))



globtemp.fit <- sarima(globtemp,2,1,0,details = F)
globtemp.fit$ttable

globtemp.fit <- sarima(globtemp,3,1,0,details = F)
globtemp.fit$ttable

globtemp.fit <- sarima(globtemp,3,1,1,details = F)
globtemp.fit$ttable

#autoplot(globtemp)
library(fpp2)

library(forecast)
fit <- auto.arima(globtemp)
checkresiduals(fit)

fit <- auto.arima(austa)
checkresiduals(fit)

air.fit <- auto.arima(AirPassengers)
air.fit
