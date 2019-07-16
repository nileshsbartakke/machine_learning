

library(mlbench)
data("Sonar")

library(caret)
set.seed(1992)
intrain<-createDataPartition(y=Sonar$Class,p=0.7,list=FALSE)
training <- Sonar[intrain,]
validation <- Sonar[-intrain,]

library(h2o)
h2o.init(nthreads = 4)

train.hex <- as.h2o(training)
valid.hex <- as.h2o(validation)

model.gbm <- h2o.gbm(y=61, x=1:60, training_frame = train.hex, 
                     ntrees = 50, max_depth = 4)

pred.GBM <- h2o.predict(model.gbm,newdata = valid.hex)
pred.df <- as.data.frame(pred.GBM)
pred.df$predict <- factor(pred.df$predict)
confusionMatrix(pred.df$predict,validation$Class)
