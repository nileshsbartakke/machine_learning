library(Ecdat)

data("Housing")
head(Housing)

library(caret)
set.seed(1992)
intrain<-createDataPartition(y=Housing$price , 
                             p=0.7,list=FALSE)

training   <- Housing[ intrain , ]
validation <- Housing[-intrain , ]

library(rpart)
fitRT <- rpart( price ~ . , data = training , method = "anova" )

library(rpart.plot)

rpart.plot(fitRT,type = 4,extra = 1, digits = 5)

pred.RT <- predict(fitRT,newdata = validation )

postResample(pred.RT , validation$price)
# OR
RMSE <- function(y, yhat) {
  sqrt(mean((y - yhat)^2))
}
RMSE(validation$price, pred.RT)

MAPE <- function(y, yhat) {
  mean(abs((y - yhat)/y))
}
MAPE(validation$price , pred.RT)

RMSPE<- function(y, yhat) {
  sqrt(mean((y-yhat)/y)^2)
}
RMSPE(validation$price , pred.RT)

##### Pruning ######

plotcp(fitRT)
fitRT$cptable


fitRT.pruned <- prune(fitRT , cp= 0.01683250   )

rpart.plot(fitRT.pruned,type = 4,extra = 1, digits = 5)

pred.RT.pruned <- predict(fitRT.pruned , newdata = validation)

postResample(pred.RT.pruned , validation$price)
MAPE(validation$price , pred.RT.pruned)  
RMSPE(validation$price , pred.RT.pruned)

###### Conditional Inference Tree ########

library(party)

fitCT <- ctree(price ~ . , data = training )

plot(fitCT , type="simple")

plot(fitCT , type="extended" )

pred.CT <- predict(fitCT , newdata=validation)

postResample(pred.CT , validation$price)
MAPE(validation$price , pred.CT)  
RMSPE(validation$price , pred.CT)  


###############LM####################

fitLM <- lm(price ~ . , data = training )
pred.LM <- predict(fitLM , newdata=validation)
postResample(pred.LM , validation$price)
MAPE(validation$price , pred.LM)  
RMSPE(validation$price , pred.LM)  

