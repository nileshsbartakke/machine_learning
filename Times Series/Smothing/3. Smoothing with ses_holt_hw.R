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

sesMilk <- ses(train.ts, h = nValid, alpha = 0.2)
plot(sesMilk,   ylab = "Milk Production", xlab = "Time", bty = "l", 
     xaxt = "n",  main = "", flty = 2)
axis(1, at = seq(1962, 1975, 1), labels = format(seq(1962, 1975, 1)))
lines(sesMilk$fitted, lwd = 2, col = "blue")
lines(valid.ts)
sesMilk$model
accuracy(sesMilk , valid.ts)


# Without setting the alpha
sesMilk_opt <- ses(train.ts, h = nValid)
plot(sesMilk_opt,   ylab = "Milk Production", xlab = "Time", bty = "l", xaxt = "n",  main = "", flty = 2)
axis(1, at = seq(1962, 1975, 1), labels = format(seq(1962, 1975, 1)))
lines(sesMilk_opt$fitted, lwd = 2, col = "blue")
lines(valid.ts)
sesMilk_opt$model
accuracy(sesMilk_opt , valid.ts)


################### Holt's Linear Trend Method #########################
# Setting Aplha and Beta
holtMilk <- holt(train.ts,alpha = 0.5,beta = 0.05,initial = "simple",h = nValid)
plot(holtMilk,   ylab = "Milk Production", xlab = "Time", bty = "l", 
     xaxt = "n",  main = "", flty = 2)
axis(1, at = seq(1962, 1975, 1), labels = format(seq(1962, 1975, 1)))
lines(holtMilk$fitted, lwd = 2, col = "blue")
lines(valid.ts)

holtMilk$model
accuracy(holtMilk , valid.ts)

#Without setting Alpha and Beta
holtMilkLin <- holt(train.ts,initial = "simple",h = nValid)
plot(holtMilkLin,   ylab = "Milk Production", xlab = "Time", 
     bty = "l", xaxt = "n",  main = "", flty = 2)
axis(1, at = seq(1962, 1975, 1), labels = format(seq(1962, 1975, 1)))
lines(holtMilkLin$fitted, lwd = 2, col = "blue")
lines(valid.ts)

holtMilkLin$model
accuracy(holtMilkLin , valid.ts)

#inital = Optimal

holtMilkLin <- holt(train.ts,initial = "optimal",h = nValid)
plot(holtMilkLin,   ylab = "Milk Production", xlab = "Time", 
     bty = "l", xaxt = "n",  main = "", flty = 2)
axis(1, at = seq(1962, 1975, 1), labels = format(seq(1962, 1975, 1)))
lines(holtMilkLin$fitted, lwd = 2, col = "blue")
lines(valid.ts)

holtMilkLin$model
accuracy(holtMilkLin , valid.ts)

################# Exponential Trend Method ############################
holtMilkExp <- holt(train.ts,initial = "simple",exponential = TRUE ,h = nValid)
plot(holtMilkExp,   ylab = "Milk Production", xlab = "Time", bty = "l", 
     xaxt = "n",  main = "", flty = 2)
axis(1, at = seq(1962, 1975, 1), labels = format(seq(1962, 1975, 1)))
lines(holtMilkExp$fitted, lwd = 2, col = "blue")
lines(valid.ts)
holtMilkExp$model
accuracy(holtMilkExp , valid.ts)

#################Additive Damped Trend Method ############################
holtMilkDamp <- holt(train.ts,initial = "optimal",damped = TRUE ,h = nValid)
plot(holtMilkDamp,   ylab = "Milk Production", xlab = "Time", bty = "l", xaxt = "n",  main = "", flty = 2)
axis(1, at = seq(1962, 1975, 1), labels = format(seq(1962, 1975, 1)))
lines(holtMilkDamp$fitted, lwd = 2, col = "blue")
lines(valid.ts)
holtMilkDamp$model
accuracy(holtMilkDamp , valid.ts)

#################Multiplicative Damped Trend Method ############################
holtMilkDamp <- holt(train.ts,damped = TRUE ,h = nValid ,exponential = TRUE)
plot(holtMilkDamp,   ylab = "Milk Production", xlab = "Time", bty = "l", xaxt = "n",  main = "", flty = 2)
axis(1, at = seq(1962, 1975, 1), labels = format(seq(1962, 1975, 1)))
lines(holtMilkDamp$fitted, lwd = 2, col = "blue")
lines(valid.ts)
holtMilkDamp$model
accuracy(holtMilkDamp , valid.ts)

###############Holt-Winter's Additive Seasonal Method ##########################
HWMilkAdd <- hw(train.ts,h = nValid ,seasonal = "additive")
plot(HWMilkAdd,   ylab = "Milk Production", xlab = "Time", bty = "l", 
     xaxt = "n",  main = "", flty = 2)
axis(1, at = seq(1962, 1975, 1), labels = format(seq(1962, 1975, 1)))
lines(HWMilkAdd$fitted, lwd = 2, col = "blue")
lines(valid.ts)

HWMilkAdd$model
accuracy(HWMilkAdd , valid.ts)

###############Holt-Winter's Multiplicative Seasonal Method ##########################
HWMilkMult <- hw(train.ts,h = nValid ,seasonal = "multiplicative")
plot(HWMilkMult,   ylab = "Milk Production", xlab = "Time", bty = "l", 
     xaxt = "n",  main = "", flty = 2)
axis(1, at = seq(1962, 1975, 1), labels = format(seq(1962, 1975, 1)))
lines(HWMilkMult$fitted, lwd = 2, col = "blue")
lines(valid.ts)


HWMilkMult$model
accuracy(HWMilkMult , valid.ts)

