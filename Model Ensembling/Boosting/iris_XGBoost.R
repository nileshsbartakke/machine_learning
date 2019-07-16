data("iris")

library(caret)
set.seed(1992)
intrain<-createDataPartition(y=iris$Species,p=0.7,list=FALSE)

training   <- iris[ intrain , ]
validation <- iris[-intrain , ]

library(xgboost)
mat_train <- as.matrix(training[,-5])
mat_validation <- as.matrix(validation[,-5])
lbl_spec <- ifelse(training$Species=="setosa",0,
                   ifelse(training$Species=="versicolor",1,2))

model.xgb <- xgboost(data=mat_train,label =lbl_spec ,nrounds = 9,
                     num_class=3, objective="multi:softmax")

pred.xgb <- predict(model.xgb,newdata=mat_validation)
lbl_spec_val <- ifelse(validation$Species=="setosa",0,
                       ifelse(validation$Species=="versicolor",1,2))

confusionMatrix(as.factor(pred.xgb),as.factor(lbl_spec_val))
