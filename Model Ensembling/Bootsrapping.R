vect <- c(7, 23, 45, 67, 42, 56, 90, 12, 58, 79)
mean(vect)

tt <- t.test(vect)
tt$conf.int

resamples <- array(dim=c(1000,10))

for(i in 1:1000){
  resamples[i,] <- sample(vect, size = 10 , replace = T)
}

remeans <- apply(resamples, 1 , mean)
Conf.Limits <- quantile(remeans, c(0.025,0.975))
Conf.Limits




##### Using package boot ################

library(boot)

Average <- function(input , indices) {
  data <- input[indices]
  mean(data)
}

results <- boot(vect, statistic = Average , 
                R = 1000)
print(results)
## Components t0 and t
results$t0
str(results$t)

boot.ci(results, type = "perc")

plot(results)

library(MASS)
data("Boston")
LinearModel <- function(formula, input , indices) {
  RegData <- input[indices , ]
  fit <- lm(formula , data = RegData )
  sm <- summary(fit)
  c(Res.Error = sm$sigma , Adj.R.Sqd = sm$adj.r.squared)
}

results <- boot(Boston,LinearModel,R=1000,formula= medv ~ .)
print(results)
plot(results, index = 1)
plot(results, index = 2)

boot.ci(results,type = "perc", index = 1)
boot.ci(results,type = "perc", index = 2)
