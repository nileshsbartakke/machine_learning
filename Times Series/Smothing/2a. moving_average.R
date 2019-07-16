### install.packages("rdatamarket")

#library(rdatamarket)
#dminit("ccee6752c2ba4281bc7112d20bcfce5f")
#mlk <- dmlist("https://datamarket.com/data/set/22ox/monthly-milk-production-pounds-per-cow-jan-62-dec-75#!ds=22ox")
#milk_ts <- ts(data = mlk$Value, start = c(1962,1), frequency = 12 )

## write.csv(mlk,"E:\\Statistics\\Time Series\\MilkProd.csv")

setwd("F:\\Statistics\\Datasets")
mlk <- read.csv("monthly-milk-production-pounds-p.csv",
                colClasses =c("character","numeric") )
milk_ts <- ts(data = mlk$Milk, start = c(1962,1), frequency = 12 )

plot(milk_ts)

# Number of Observations in Validation data
nValid <- 38
# Number of Observations in Training data
nTrain <- nrow(mlk) - nValid
# Training data and Validation data partitioned using window()
train.ts <- window(milk_ts, start = c(1962,1), end = c(1972, 11))
valid.ts <- window(milk_ts, start = c(1972, 12))

################################# Centered Moving Average #################################

library(forecast)
maMilk1Cent  <- ma(milk_ts, order = 3)
plot(milk_ts,   ylab = "Milk Production", xlab = "Time", xaxt = "n", main = "")
axis(1, at = seq(1962, 1975, 1), labels = format(seq(1962, 1975, 1)))
lines(maMilk1Cent , lwd = 2, col = "blue")

################################# Trailing Moving Average ##################################

library(zoo)
maMilk3Trail <- rollmean(train.ts, k = 3, align = "right")
lastMA <- tail(maMilk3Trail, 1)
predMA <- ts(rep(lastMA, nValid), start = c(1962, nTrain + 1), 
             end = c(1962, nTrain + nValid), freq = 12)
plot(train.ts,   ylab = "Milk Production", xlab = "Time", bty = "l", 
     xaxt = "n",  main = "")
axis(1, at = seq(1962, 1975, 1), labels = format(seq(1962, 1975, 1)))
lines(maMilk3Trail, lwd = 2, col = "blue")
lines(predMA, lwd = 2, col = "red", lty = 2) 
lines(valid.ts)
accuracy(predMA,valid.ts)

################################### Differencing ##########################################

df.train.1 <- diff(train.ts,lag = 1)
library(zoo)
maMilk3Trail <- rollmean(df.train.1, k = 3, align = "right")
lastMA <- tail(maMilk3Trail, 1)
predMA <- ts(rep(lastMA, nValid), start = c(1962, nTrain + 1), 
             end = c(1962, nTrain + nValid), freq = 12)
plot(df.train.1,   ylab = "Milk Production", xlab = "Time", bty = "l", 
     xaxt = "n",  main = "")
axis(1, at = seq(1962, 1975, 1), labels = format(seq(1962, 1975, 1)))
lines(maMilk3Trail, lwd = 2, col = "blue")
lines(predMA, lwd = 2, col = "blue", lty = 2) 

