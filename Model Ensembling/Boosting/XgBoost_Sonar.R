library(mlbench)
data("Sonar")

library(caret)
set.seed(1992)
intrain<-createDataPartition(y=Sonar$Class,p=0.7,list=FALSE)

training   <- Sonar[ intrain , ]
validation <- Sonar[-intrain , ]


#training$Class <- ifelse(training$Class == "M", 1, 0)

library(Matrix)
sparse_matrix <- model.matrix(Class ~ ., data = training)
sparse_matrix_train <- sparse_matrix[,-1]

library(xgboost)
dgCMat <- as(sparse_matrix_train, "dgCMatrix")
lbl_binary <- ifelse(training$Class == "M", 1, 0)
model.xgb <- xgboost(data=dgCMat,label=lbl_binary,
                     objective="binary:logistic",
                     nrounds = 5)

sparse_matrix <- model.matrix(Class ~ ., data = validation)
sparse_matrix_val <- sparse_matrix[,-1]

dgCMatValid <- as(sparse_matrix_val, "dgCMatrix")

pred.xgb <- predict(model.xgb,newdata=dgCMatValid)

predCLass <- factor(ifelse(pred.xgb < 0.5, "R","M"))

ctree.perf <- table( predCLass,validation$Class ,
                     dnn=c("Predicted", "Actual"))

confusionMatrix(ctree.perf)
