library(MASS)
data("Boston")

library(caret)
set.seed(1992)
intrain<-createDataPartition(y=Boston$medv , p=0.7,list=FALSE)

training   <- Boston[ intrain , ]
validation <- Boston[-intrain , ]


library(party)
model.RF <- cforest(medv ~ ., data = training, 
                    control = cforest_unbiased(ntree = 50))

model.RF

pred.RF <- predict(model.RF, newdata = validation[,-14])

postResample(pred.RF , validation$medv)

MAPE <- function(y, yhat) {
  mean(abs((y - yhat)/y))
}

MAPE(validation$medv , pred.RF)

RMSPE<- function(y, yhat) {
  sqrt(mean((y-yhat)/y)^2)
}

RMSPE(validation$medv , pred.RF)



