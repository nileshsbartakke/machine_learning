telecom <- read.csv("F:\\Statistics\\Cases\\Telecom\\Telecom.csv")

library(caret)

set.seed(333)
intrain <- createDataPartition(y=telecom$Response,
                               p=0.7,list = FALSE)

training <- telecom[intrain,   ]
validation <- telecom[-intrain,]

library(e1071)
classifier <- naiveBayes(training[,1:2], training[,3]) 

PredY <- predict(classifier, newdata=validation[,-3], 
                 type="class")

PredYProb <- predict(classifier, newdata=validation[,-3],
                     type="raw")
# 
# Adjusting the factor levels for relevant output generation
PredY <- factor(PredY,levels = c("Y","N"))
validation[,3] <- factor(validation[,3],levels = c("Y","N"))

tbl <- table(PredY, validation[,3],
             dnn=list('predicted','actual'))
confusionMatrix(tbl)

# OR
confusionMatrix(tbl,positive = "Y")

##ROC
library(pROC)
plot.roc(validation[,3],PredYProb[,2],
         legacy.axes=TRUE,print.auc=TRUE )


objROC <- plot.roc(validation[,3],PredYProb[,2],
                   legacy.axes=TRUE,print.auc=TRUE )
names(objROC)
objROC$thresholds
objROC$sensitivities
objROC$specificities

# Predicting
tsttel <- read.csv("F:\\Statistics\\Cases\\Telecom\\testTelecom.csv")
PredY <- predict(classifier, newdata=tsttel, type="class")

predt <- data.frame(tsttel,PredY)
