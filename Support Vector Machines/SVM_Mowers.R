
library(caret)
mowers <- read.csv("F:\\Statistics\\Datasets\\RidingMowers.csv")

set.seed(1992)
intrain<-createDataPartition(y=mowers$Response,p=0.7,list=FALSE)

training <- mowers[intrain, ]
validation <- mowers[-intrain, ]

library(e1071)
fit.svm <- svm(Response~., type="C",data=training, kernel="linear")
svm.pred <- predict(fit.svm, newdata=validation)
svm.perf <- table(svm.pred, validation$Response, dnn=c("Predicted","Actual"))
confusionMatrix(svm.perf) 

#### Visualizing

plot(fit.svm, training, Income~Lot_Size)

tune.out <- tune(svm,Response~.,data = training, kernel="linear",
                 ranges=list(cost=c(0.001,0.002,0.005,0.007,
                                    0.008,0.01,0.1,1,2,3,4)))

summary(tune.out)
