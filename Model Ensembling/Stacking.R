library(Ecdat)
data("Housing")

library(caret)
set.seed(1234)
trainIndex <- createDataPartition(y=Housing$price,
                                  p=0.7,list=F)

training <- Housing[trainIndex, ]
validation <- Housing[-trainIndex, ]

fitLM <- lm(price~. , data = training)
pred.LM <- predict(fitLM, newdata = training)

library(rpart)
fitRegTree <- rpart(price~. , data = training, method = "anova")
pred.RegTree <- predict(fitRegTree, newdata = training)
# 
# pred.RegTree2 <- predict(fitRegTree, newdata = validation)
# postResample(pred = pred.RegTree2, obs = validation$price)


trndf <- data.frame(price=training$price, 
                    pred.LM, pred.RegTree)

library(randomForest)
fitRF <- randomForest(price~., data = trndf)

##########Processing Validation Set#####################

fitLM2 <- lm(price~. , data = validation)
pred.LM <- predict(fitLM2, newdata = validation)

fitRegTree <- rpart(price~. , data = validation, method = "anova")
pred.RegTree <- predict(fitRegTree, newdata = validation)


valdf <- data.frame(pred.LM, pred.RegTree)
pred.RF <- predict(fitRF, newdata = valdf)

postResample(pred = pred.RF, obs = validation$price)


##########################################################

### Alone Regression Tree
fitRegTree2 <- rpart(price~. , data = training, method = "anova")
pred.RegTree <- predict(fitRegTree2, newdata = validation)
postResample(pred = pred.RegTree, obs = validation$price)

## Alone Random Forest
fitRF <- randomForest(price~., data = training)
pred.RanFor <- predict(fitRF, newdata = validation)
postResample(pred = pred.RanFor, obs = validation$price)

####Alone Linear Regression
pred.LM2 <- predict(fitLM, newdata = validation)
postResample(pred = pred.LM2, obs = validation$price)


##############################################################################

library(xgboost)
mat_train <- as.matrix(trndf[,-1])
mat_validation <- as.matrix(valdf)
model.xgb <- xgboost(data=mat_train,label = training$price,nrounds = 5)

pred.xgb <- predict(model.xgb,newdata=mat_validation)
postResample(pred.xgb,validation$price)
