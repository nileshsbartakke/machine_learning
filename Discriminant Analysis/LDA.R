library(mlbench)
data("Glass")

Glass$Type <- factor(Glass$Type)

library(caret)
set.seed(1992)
intrain<-createDataPartition(y=Glass$Type , p=0.7,list=FALSE)

training   <- Glass[ intrain , ]
validation <- Glass[-intrain , ]

library(MASS)
fit.lda <- lda(Type ~ . , data = training)

pred.lda <- predict(fit.lda , newdata = validation)

confusionMatrix(pred.lda$class,validation$Type)

