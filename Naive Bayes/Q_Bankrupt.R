brupt <- read.csv("F:\\Statistics\\Cases\\Qualitative_Bankruptcy\\Qualitative_Bankruptcy.data.txt")

library(caret)

set.seed(333)
intrain <- createDataPartition(y=brupt$Class,p=0.7,list = FALSE)

training <- brupt[intrain,   ]
validation <- brupt[-intrain,]

library(e1071)
classifier <- naiveBayes(training[,1:6], training[,7]) 

PredY <- predict(classifier, newdata=validation[,-7], 
                 type="class")

PredYProb <- predict(classifier, newdata=validation[,-7],type="raw")

tbl <- table(PredY, validation[,7],dnn=list('predicted','actual'))

confusionMatrix(tbl)

# OR

confusionMatrix(PredY, validation[,7],dnn=list('predicted','actual'))

postResample(PredY, validation[,7]) # For factor variables

library(pROC)
plot.roc(validation[,7],PredYProb[,1],
         legacy.axes=TRUE,print.auc=TRUE )


#### Predicting

tp <- read.csv("F:\\Statistics\\Cases\\Qualitative_Bankruptcy\\ToPredict.csv")
predBR <- predict(classifier, newdata = tp,type = "class")
predicted <- data.frame(tp,predBR)
