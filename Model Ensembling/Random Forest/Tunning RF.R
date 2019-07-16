library(carData)
data("Salaries")

library(caret)
set.seed(2018)
intrain<-createDataPartition(y= Salaries$salary ,p=0.7,list=FALSE)
training   <- Salaries[ intrain , ]
validation <- Salaries[-intrain , ]

library(randomForest)

model.RF <- randomForest(salary~.,data = training,
                        importance=T)

importance(model.RF,type = 2)

pred.RF <- predict(model.RF, newdata = validation, 
                   type = "response")

postResample(pred = pred.RF, obs = validation$salary)

########### Tuning ################

myGrid <- data.frame(mtry=seq(1,4))
tuned.model <- train(salary~.,data = training, 
                     method="rf",tuneGrid=myGrid)
plot(tuned.model)

pred.RF <- predict(tuned.model, newdata = validation,
                   type = "raw")

postResample(pred = pred.RF, obs = validation$salary)
