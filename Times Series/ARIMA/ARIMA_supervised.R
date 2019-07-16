library(fpp2)
data("auscafe")

autoplot(auscafe)

# Number of Observations in Validation data
nValid <- 22
# Number of Observations in Training data
nTrain <- 426 - nValid
# Training data and Validation data partitioned using window()
train.ts <- window(auscafe, start = c(1982,4), end = c(1982, nTrain + 1))
valid.ts <- window(auscafe, start = c(1982, nTrain + 2))

library(forecast)
#### Smoothing Methods
model_arima <- auto.arima(train.ts)
pred.arima <- forecast(model_arima,h=nValid)
accuracy(pred.arima$mean, valid.ts)
