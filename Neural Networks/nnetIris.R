data("iris")

library(caret)
set.seed(1992)
intrain<-createDataPartition(y=iris$Species , p=0.7,list=FALSE)
training   <- iris[ intrain , ]
validation <- iris[-intrain , ]

library(nnet)
fit.nn <- nnet(Species ~ . , data = iris , subset = intrain,size=5,rang = 0.1,
               decay = 5e-4, maxit = 200)
# OR

fit.nn <- nnet(Species ~ . , data = training ,size=5,rang = 0.1,
               decay = 5e-4, maxit = 200)
pred.nnet <- predict(fit.nn, newdata = validation, 
                     type = "class")
pred.nn <- factor(predict(fit.nn, newdata = validation, 
                          type = "class"))
confusionMatrix(pred.nn, validation$Species)

