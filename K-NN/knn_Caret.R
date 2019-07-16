library(caret)
mowers <- read.csv("F:\\Statistics\\Datasets\\RidingMowers.csv")

set.seed(1992)
intrain<-createDataPartition(y=mowers$Response,p=0.7,list=FALSE)
training <- mowers[intrain, ]
validation <- mowers[-intrain, ]

# Using knn3 function
fitKNN1 <- knn3(Response ~ .,data=training, k=1)
pred.knn1 <- predict(fitKNN1,newdata=validation,type = "class")
tbl_1 <- table(pred.knn1 , validation$Response  )
confusionMatrix( tbl_1 )


fitKNN3 <- knn3(Response ~ .,data=training, k=3)
pred.knn3 <- predict(fitKNN3,newdata=validation,type = "class")
tbl_3 <- table(pred.knn3 , validation$Response  )
confusionMatrix( tbl_3 )


fitKNN5 <- knn3(Response ~ .,data=training, k=5)
pred.knn5 <- predict(fitKNN5,newdata=validation,type = "class")
tbl_5 <- table(pred.knn5 , validation$Response  )
confusionMatrix( tbl_5 )

######################
#postResample(pred.knn5 , validation$Response)
#twoClassSummary(data.frame(pred=pred.knn5 , obs=validation$Response, pred.prob5[,1]),fitKNN5)
#####################

fitKNN7 <- knn3(Response ~ .,data=training, k=7)
pred.knn7 <- predict(fitKNN7,newdata=validation,type = "class")
tbl_7 <- table(pred.knn7 , validation$Response  )
confusionMatrix( tbl_7 )

library(pROC)

pred.prob1 <- predict(fitKNN1,newdata=validation,type="prob")
plot.roc(validation$Response, pred.prob1[,1], print.auc=TRUE , 
         col="magenta", main="K=1",legacy.axes=TRUE)

pred.prob3 <- predict(fitKNN3,newdata=validation,type="prob" )
plot.roc(validation$Response, pred.prob3[,1] , print.auc=TRUE , col="blue", main="K=3",legacy.axes=TRUE )

pred.prob5 <- predict(fitKNN5,newdata=validation,type="prob")
plot.roc(validation$Response, pred.prob5[,1] , print.auc=TRUE ,
         col="red" , main="K=5",legacy.axes=TRUE)

pred.prob7 <- predict(fitKNN7,newdata=validation,type="prob")
plot.roc(validation$Response, pred.prob7[,1] , print.auc=TRUE , col="green" , main="K=7",legacy.axes=TRUE)

# All in One

plot.roc(validation$Response, pred.prob1[,1] , col="magenta", 
         main = "ROC for Each K",legacy.axes=TRUE)

plot.roc(validation$Response, pred.prob3[,1] , col="blue" , add=TRUE )

plot.roc(validation$Response, pred.prob5[,1] , col="red" , add=TRUE )

plot.roc(validation$Response, pred.prob7[,1] , col="green" , add=TRUE )

legend("bottomright", legend=c("K=1","K=3", "K=5","K=7"),
       col=c("magenta", "blue", "red","green"), lwd=3)

############### Alternative II (ROC) #######################
library(caTools)
colAUC(pred.prob5[,1], validation$Response, plotROC = TRUE)

############### Predicting ################################# 

tM <- read.csv("F:\\Statistics\\Datasets\\testMowers.csv")
fitKNN5 <- knn3(Response ~ .,data=mowers, k=5)
pred.knn5.tm <- predict(fitKNN5,newdata=tM,type = "class")

predicted <- data.frame(tM,pred.knn5.tm)
