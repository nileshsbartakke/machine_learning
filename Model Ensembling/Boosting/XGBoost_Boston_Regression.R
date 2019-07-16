library(MASS)
data("Boston")

library(caret)

set.seed(1234)
trainIndex <- createDataPartition(y=Boston$medv,p=0.7,list=F)

training <- Boston[trainIndex,]
validation <- Boston[-trainIndex,]

library(xgboost)
mat_train <- as.matrix(training[,-14])
mat_validation <- as.matrix(validation[,-14])
model.xgb <- xgboost(data=mat_train,objective="reg:linear",
                     label = training$medv,nrounds = 25)

## With other parameters
model.xgb <- xgboost(data=mat_train,objective="reg:linear",
                     label = training$medv,nrounds = 25,
                     eta=0.6,max_depth=3)

pred.xgb <- predict(model.xgb,newdata=mat_validation)
postResample(pred.xgb,validation$medv)
error <- postResample(pred.xgb,validation$medv)
rmse <- error[["RMSE"]]

#######################################################
## Tunning with caret
myGrid=data.frame(nrounds=seq(20,50,by = 5),
                  lambda=seq(0.1,0.7,by = 0.1),
                  alpha=seq(0.1,0.7,by = 0.1),
                  eta=seq(0.1,0.7,by = 0.1))
model.xgb <- train(x = mat_train,y = training$medv,
                   method = "xgbLinear",tuneGrid = myGrid)

pred.xgb <- predict(model.xgb,newdata=mat_validation)
postResample(pred.xgb,validation$medv)


