
library(mlbench)
data("PimaIndiansDiabetes")

library(caret)
set.seed(1992)
intrain<-createDataPartition(y=PimaIndiansDiabetes$diabetes , 
                             p=0.7,list=FALSE)
training   <- PimaIndiansDiabetes[ intrain , ]
validation <- PimaIndiansDiabetes[-intrain , ]

library(nnet)
fit.nn <- nnet(diabetes ~ . , data = PimaIndiansDiabetes , 
               subset = intrain,size=5,rang = 0.1,
               decay = 5e-4, maxit = 200)
# OR

fit.nn <- nnet(diabetes ~ . , data = training ,size=5,rang = 0.1,
               decay = 5e-4, maxit = 200)
pred.nn <- factor(predict(fit.nn, newdata = validation, 
                          type = "class"))
confusionMatrix(pred.nn, validation$diabetes, positive = "pos")

