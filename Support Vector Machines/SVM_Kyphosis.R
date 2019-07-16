library(rpart)
data("kyphosis", package = "rpart")

library(caret)

set.seed(1992)
intrain<-createDataPartition(y=kyphosis$Kyphosis,p=0.7,list=FALSE)

training <- kyphosis[intrain,]
validation <- kyphosis[-intrain, ]

library(e1071)
fit.svm <- svm(Kyphosis~., type="C",data=training, kernel="radial")
svm.pred <- predict(fit.svm, newdata=validation)
svm.perf <- table(svm.pred, validation$Kyphosis, dnn=c("Predicted","Actual"))
confusionMatrix(svm.perf) 

plot(fit.svm, training, Age~Number)
plot(fit.svm, training, Age~Start)
plot(fit.svm, training, Number~Start)

tune.out <- tune(svm,Kyphosis~.,data = training, kernel="radial",
                 ranges=list(gamma=c(0.001,0.002,0.005,0.007,0.008,0.01,0.1,1,2,3,4),
                             cost=c(0.001,0.002,0.005,0.007,0.008,0.01,0.1,1,2,3,4)))

summary(tune.out)

tune.out$best.model

svm.pred <- predict(tune.out$best.model, newdata=validation)
svm.perf <- table(svm.pred, validation$Kyphosis, dnn=c("Predicted","Actual"))
confusionMatrix(svm.perf) 
