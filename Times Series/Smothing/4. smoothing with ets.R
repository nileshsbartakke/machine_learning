### install.packages("rdatamarket")
#library(rdatamarket)
#dminit("ccee6752c2ba4281bc7112d20bcfce5f")
#mlk <- dmlist("https://datamarket.com/data/set/22ox/monthly-milk-production-pounds-per-cow-jan-62-dec-75#!ds=22ox")


setwd("F:\\Statistics\\Datasets")
mlk <- read.csv("monthly-milk-production-pounds-p.csv")
milk_ts <- ts(data = mlk$Milk, start = c(1962,1), frequency = 12 )


# Number of Observations in Validation data
nValid <- 38
# Number of Observations in Training data
nTrain <- nrow(mlk) - nValid
# Training data and Validation data partitioned using window()
train.ts <- window(milk_ts, start = c(1962,1), end = c(1962, nTrain + 1))
valid.ts <- window(milk_ts, start = c(1962, nTrain + 2))

library(forecast)

################### Simple Smoothing ########################
ses <- ets(train.ts, model = "ANN", alpha = 0.2)
ses.pred <- forecast(ses, h = nValid, level = 0)

plot(ses.pred,   ylab = "Milk Production", xlab = "Time", bty = "l", xaxt = "n",  main = "", flty = 2)
axis(1, at = seq(1962, 1975, 1), labels = format(seq(1962, 1975, 1)))
lines(ses.pred$fitted, lwd = 2, col = "blue")
lines(valid.ts)

ses
accuracy(ses.pred , valid.ts)

# Without setting the alpha
ses <- ets(train.ts, model = "ANN")
ses.opt <- forecast(ses, h = nValid, level = 0)

plot(ses.opt,   ylab = "Milk Production", xlab = "Time", bty = "l", xaxt = "n",  main = "", flty = 2)
axis(1, at = seq(1962, 1975, 1), labels = format(seq(1962, 1975, 1)))
lines(ses.opt$fitted, lwd = 2, col = "blue")
lines(valid.ts)
ses
accuracy(ses.opt , valid.ts)

################### Holt's Method #########################

# Additive error with additive trend

hesAA <- ets(train.ts, model = "AAN")
hesAA.opt <- forecast(hesAA, h = nValid, level = 0)

plot(hesAA.opt,   ylab = "Milk Production", xlab = "Time", bty = "l", xaxt = "n",  main = "", flty = 2)
axis(1, at = seq(1962, 1975, 1), labels = format(seq(1962, 1975, 1)))
lines(hesAA.opt$fitted, lwd = 2, col = "blue")
lines(valid.ts)
hesAA
accuracy(hesAA.opt , valid.ts)


# Multiplicative error with additive trend

hesMA <- ets(train.ts, model = "MAN")
hesMA.opt <- forecast(hesMA, h = nValid, level = 0)

plot(hesMA.opt,   ylab = "Milk Production", xlab = "Time", bty = "l", xaxt = "n",  main = "", flty = 2)
axis(1, at = seq(1962, 1975, 1), labels = format(seq(1962, 1975, 1)))
lines(hesMA.opt$fitted, lwd = 2, col = "blue")
lines(valid.ts)
hesMA
accuracy(hesMA.opt , valid.ts)




# Muliplicative error with multiplicative trend

hesMM <- ets(train.ts, model = "MMN")
hesMM.opt <- forecast(hesMM, h = nValid, level = 0)

plot(hesMM.opt,   ylab = "Milk Production", xlab = "Time", bty = "l", xaxt = "n",  main = "", flty = 2)
axis(1, at = seq(1962, 1975, 1), labels = format(seq(1962, 1975, 1)))
lines(hesMM.opt$fitted, lwd = 2, col = "blue")
lines(valid.ts)
hesMM
accuracy(hesMM.opt , valid.ts)


# Holt-Winters Additive Trend, Additive Seasonality

hwAA <- ets(train.ts, model = "AAA")
hwAA.opt <- forecast(hwAA, h = nValid, level = 0)

plot(hwAA.opt,   ylab = "Milk Production", xlab = "Time", bty = "l", xaxt = "n",  main = "", flty = 2)
axis(1, at = seq(1962, 1975, 1), labels = format(seq(1962, 1975, 1)))
lines(hwAA.opt$fitted, lwd = 2, col = "blue")
lines(valid.ts)
hwAA
accuracy(hwAA.opt , valid.ts)

# Holt-Winters Additive Trend, Multiplicative Seasonality

hwAM <- ets(train.ts, model = "MAM")
hwAM.opt <- forecast(hwAM, h = nValid, level = 0)

plot(hwAM.opt,   ylab = "Milk Production", xlab = "Time", bty = "l", xaxt = "n",  main = "", flty = 2)
axis(1, at = seq(1962, 1975, 1), labels = format(seq(1962, 1975, 1)))
lines(hwAM.opt$fitted, lwd = 2, col = "blue")
lines(valid.ts)
hwAM
accuracy(hwAM.opt , valid.ts)

####################The Best########################
hwes <- ets(train.ts, allow.multiplicative.trend = TRUE,additive.only = FALSE)
hwes.opt <- forecast(hwes, h = nValid, level = 0)

plot(hwes.opt,   ylab = "Milk Production", xlab = "Time", 
     bty = "l", xaxt = "n",  main = "", flty = 2)
axis(1, at = seq(1962, 1975, 1), labels = format(seq(1962, 1975, 1)))
lines(hwes.opt$fitted, lwd = 2, col = "blue")
lines(valid.ts)
hwes
accuracy(hwes.opt , valid.ts)
