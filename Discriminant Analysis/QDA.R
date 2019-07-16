library(mlbench)
data("Vehicle")

library(caret)
set.seed(1992)
intrain<-createDataPartition(y=Vehicle$Class , p=0.7,list=FALSE)

training   <- Vehicle[ intrain , ]
validation <- Vehicle[-intrain , ]
library(MASS)

##################### LDA ###########################
fit.lda <- lda(Class ~ . , data = training)

pred.lda <- predict(fit.lda , newdata = validation)

confusionMatrix(pred.lda$class,validation$Class)


##################### QDA ###########################
fit.qda <- qda(Class ~ . , data = training)

pred.qda <- predict(fit.qda , newdata = validation)

confusionMatrix(pred.qda$class,validation$Class)
