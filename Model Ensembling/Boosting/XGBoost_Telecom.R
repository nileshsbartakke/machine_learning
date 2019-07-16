telecom <- read.csv("F:\\Statistics\\Cases\\Telecom\\Telecom.csv")

library(caret)

set.seed(333)
intrain <- createDataPartition(y=telecom$Response,
                               p=0.7,list = FALSE)

training <- telecom[intrain,   ]
validation <- telecom[-intrain,]

library(Matrix)
sparse_matrix <- model.matrix( Response ~ ., data = training)
sparse_matrix_train <- sparse_matrix[,-1]

dgCMat <- as(sparse_matrix_train, "dgCMatrix")
lbl_binary <- ifelse(training$Response == "Y" , 1, 0)

library(xgboost)
model.xgb <- xgboost(data=dgCMat,label=lbl_binary,
                     objective="binary:logistic",
                     nrounds = 10)

sparse_matrix <- model.matrix(Response ~ ., 
                              data = validation)
sparse_matrix_val <- sparse_matrix[,-1]

dgCMatValid <- as(sparse_matrix_val, "dgCMatrix")

pred.xgb <- predict(model.xgb,newdata=dgCMatValid)

predCLass <- factor(ifelse(pred.xgb < 0.5, "N","Y"))

ctree.perf <- table( predCLass,validation$Response ,
                     dnn=c("Predicted", "Actual"))

confusionMatrix(ctree.perf,positive = "Y")
