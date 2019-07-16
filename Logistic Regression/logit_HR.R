hr <- read.csv("F:/Kaggle/human-resources-analytics/HR_comma_sep.csv")
hr$left <- factor(hr$left, levels = c(0,1), 
                  labels = c("Stayed","Left"))
hr$Work_accident <- factor(hr$Work_accident, levels = c(0,1), 
                           labels = c("Not Happened","Happened"))

#pairs(hr[,-7],col=hr$left)

fit.lg <- glm(left ~ satisfaction_level , 
              data = hr , family = binomial())
summary(fit.lg)

testdf <- data.frame(satisfaction_level = c(0,0.25,0.5,0.75,1))
pred.lg <- predict.glm(fit.lg , newdata = testdf, 
                       type = "response")
testdf <- data.frame(testdf, pred.lg)
  

fit.lg <- glm(left ~ Work_accident , data = hr , family = binomial())
summary(fit.lg)

testdf <- data.frame(Work_accident = factor(c(0,1), levels = c(0,1), 
                                            labels = c("Not Happened","Happened")))
pred.lg <- predict.glm(fit.lg , newdata = testdf, type = "response")
testdf <- data.frame(testdf, pred.lg)

###################################################################################

fit.lg <- glm(left ~ . , data = hr , family = binomial())
summary(fit.lg)

exp(coef(fit.lg))

###################################################################################

library(caret)
set.seed(1992)
intrain<-createDataPartition(y=hr$left , p=0.7,list=FALSE)
training   <- hr[ intrain , ]
validation <- hr[-intrain , ]
fit.lg <- glm(left ~ . , data = training , family = binomial())
pred.lg <- predict(fit.lg, newdata = validation , 
                   type = "response")
pred.lg.cat <- factor(ifelse(pred.lg < 0.5 , "Stayed" , "Left"), 
                      levels = c("Stayed","Left"))

confusionMatrix(pred.lg.cat , validation$left)
confusionMatrix(pred.lg.cat , validation$left, 
                positive = "Left")

library(pROC)
plot.roc(validation$left, pred.lg, print.auc=TRUE , 
         col="magenta", main="Logistic Regression",
         legacy.axes=TRUE)
