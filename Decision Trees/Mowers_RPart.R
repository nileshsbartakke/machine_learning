mowers <- read.csv("F:\\Statistics\\Datasets\\RidingMowers.csv")

library(caret)
set.seed(1992)
intrain<-createDataPartition(y=mowers$Response,p=0.7,list=FALSE)

training   <- mowers[ intrain , ]
validation <- mowers[-intrain , ]

library(rpart)
library(rpart.plot)

fitRPart <- rpart(Response ~ ., method="class", data=training)

fitRPart <- rpart(Response ~ ., method="class", data=training,
                  control = rpart.control(minsplit = 5))

# fitRPart <- rpart(Response ~ ., method="class", data=training,
#                  control = rpart.control(minsplit = 5) , parms = list(split="information"))

rpart.plot(fitRPart,type=4, extra=1 , main = "Classification of Audience")   

rpart.plot(fitRPart,type=4, extra=4 , main = "Classification of Audience")  

pred.rpart <- predict(fitRPart , newdata=validation[,-3],
                      type=c("class"))

tblMowers <- table( pred.rpart , validation$Response)

confusionMatrix(tblMowers)
