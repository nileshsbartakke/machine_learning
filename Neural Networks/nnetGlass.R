
library(mlbench)
data("Glass")
Glass$Type <- factor(Glass$Type)

library(caret)
set.seed(1992)
intrain<-createDataPartition(y=Glass$Type , p=0.7,list=FALSE)
training   <- Glass[ intrain , ]
validation <- Glass[-intrain , ]

library(nnet)
fit.nn <- nnet(Type ~ . , data = Glass , subset = intrain,size=5,rang = 0.1,
               decay = 5e-4, maxit = 200)
# OR

fit.nn <- nnet(Type ~ . , data = training ,size=5,rang = 0.1,
               decay = 5e-4, maxit = 200)
pred.nn <- factor(predict(fit.nn, newdata = validation, 
                          type = "class"), 
                  levels = c(1,2,3,5,6,7))
confusionMatrix(pred.nn, validation$Type)


