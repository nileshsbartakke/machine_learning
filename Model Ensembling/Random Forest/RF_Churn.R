library(C50)
data(churn)
library(caret)

library(randomForest)
model.RF <- randomForest(churn ~ . , data = churnTrain ,
                         na.action=na.roughfix, importance=TRUE)

model.RF

importance(model.RF,type = 2)
varImpPlot(model.RF)

pred <- predict(model.RF, newdata = churnTest)

confusionMatrix( table(pred, churnTest$churn) )

library(party)
model.CF <- cforest(churn ~ . , data = churnTrain ,
                         controls = cforest_unbiased(ntree=100) )

pred <- predict(model.CF, newdata = churnTest)

confusionMatrix( table(pred, churnTest$churn) )

####################Ranger#######################

library(ranger)
model.RF <- ranger(churn ~ . , data = churnTrain ,importance='impurity')

model.RF
#plot(model.RF)
importance(model.RF,type = 2)
#varImpPlot(model.RF)
model.RF$variable.importance


pred <- predict(model.RF, data = churnTest)

confusionMatrix( table(pred$predictions, churnTest$churn) )
