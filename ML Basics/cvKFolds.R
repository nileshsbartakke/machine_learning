library(Ecdat)

data("Housing")
str(Housing)

library(caret)
trainIndex <- createFolds(y = Housing$price , k = 5)
fold1 <- Housing[trainIndex$Fold1,]
fold2 <- Housing[trainIndex$Fold2,]
fold3 <- Housing[trainIndex$Fold3,]
fold4 <- Housing[trainIndex$Fold4,]
fold5 <- Housing[trainIndex$Fold5,]

train1 <- Housing[-trainIndex$Fold1,]
train2 <- Housing[-trainIndex$Fold2,]
train3 <- Housing[-trainIndex$Fold3,]
train4 <- Housing[-trainIndex$Fold4,]
train5 <- Housing[-trainIndex$Fold5,]

library(ranger) ## Uses Fast Implementation of Random Forest

################### Fold 1 #####################
model.RF1 <- ranger(price~. , data = train1)
pred.RF1 <- predict(model.RF1 , data = fold1)

MAPE <- function(y, yhat) {
  mean(abs((y - yhat)/y))
}

error1 <- MAPE(fold1$price , pred.RF1$predictions)


################### Fold 2 #####################
model.RF2 <- ranger(price~. , data = train2)
pred.RF2 <- predict(model.RF2 , data = fold2)

MAPE <- function(y, yhat) {
  mean(abs((y - yhat)/y))
}

error2 <- MAPE(fold2$price , pred.RF2$predictions)


################### Fold 3 #####################
model.RF3 <- ranger(price~. , data = train3)
pred.RF3 <- predict(model.RF3 , data = fold3)

MAPE <- function(y, yhat) {
  mean(abs((y - yhat)/y))
}

error3 <- MAPE(fold3$price , pred.RF3$predictions)


################### Fold 4 #####################
model.RF4 <- ranger(price~. , data = train4)
pred.RF4 <- predict(model.RF4 , data = fold4)

MAPE <- function(y, yhat) {
  mean(abs((y - yhat)/y))
}

error4 <- MAPE(fold4$price , pred.RF4$predictions)


################### Fold 5 #####################
model.RF5 <- ranger(price~. , data = train5)
pred.RF5 <- predict(model.RF5 , data = fold5)

MAPE <- function(y, yhat) {
  mean(abs((y - yhat)/y))
}

error5 <- MAPE(fold5$price , pred.RF5$predictions)

############# Averaging the Errors ###############
AvgError <- mean(c(error1,error2,error3,error4,error5))
AvgError
MaxError <- max(c(error1,error2,error3,error4,error5))
MaxError

##################################################

library(caret)
model.RF <- train(price~. , data = Housing, method="ranger",
                  trControl = trainControl(method = "cv", number = 5),
                  tuneGrid = expand.grid(mtry=c(2:5)) )
model.RF


model.RF <- train(price~. , data = Housing, method="ranger",
                  trControl = trainControl(method = "cv", number = 5))

model.RF

