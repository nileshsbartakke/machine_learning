hr <- read.csv("G:/Kaggle/human-resources-analytics/HR_comma_sep.csv")
hr$left <- factor(hr$left)

library(caret)
set.seed(1234)
trainIndex <- createDataPartition(y=hr$left,
                                  p=0.6,list=F)

training <- hr[trainIndex,]

remaining <- hr[-trainIndex,]
validIndex <- createDataPartition(y=remaining$left,
                                  p=0.67,list=F)
validation <- remaining[validIndex ,]
testing <- remaining[-validIndex ,]

library(rpart)
model.rp <- rpart(left~., data = training, method = "class", 
                  control = rpart.control(minsplit = 700))
### Validation
pred.rp <- predict(model.rp, newdata = validation, type = "class")
confusionMatrix(pred.rp, validation$left, dnn=c("Predicted", "Actual"))

### Testing
pred.rp <- predict(model.rp, newdata = testing, type = "class")
confusionMatrix(pred.rp, testing$left, dnn=c("Predicted", "Actual"))

library(ranger)
model.ranger <- ranger(left ~ . , data = training, mtry = 4, importance = 'impurity')

### Validation
pred.rp <- predict(model.ranger, data = validation)
confusionMatrix(pred.rp$predictions, validation$left, dnn=c("Predicted", "Actual"))

### Testing
pred.rp <- predict(model.ranger, data = testing)
confusionMatrix(pred.rp$predictions, testing$left, dnn=c("Predicted", "Actual"))


library(caret)
# Fit random forest: model
model <- train(
  left ~ .,
  tuneGrid = data.frame(mtry = c(2, 3, 7)),
  data = training, method = "ranger",
  trControl = trainControl(method = "cv", number = 5, verboseIter = TRUE)
)

# Print model to console
model

# Plot model
plot(model)

pred.caret <- caret::predict.train(model, newdata = validation, type = "raw")
confusionMatrix(pred.caret , validation$left, dnn=c("Predicted", "Actual"))

pred.caret <- caret::predict.train(model, newdata = testing, type = "raw")
confusionMatrix(pred.caret , testing$left, dnn=c("Predicted", "Actual"))
