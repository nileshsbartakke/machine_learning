library(glmnet)
library(Matrix)
library(ISLR)
data("Hitters")
Hitters <- Hitters[complete.cases(Hitters),]
x=model.matrix(Salary~.-1,data=Hitters)
y=Hitters$Salary
fit.ridge <- glmnet(x,y,alpha = 0)
plot(fit.ridge,xvar = "lambda",label = TRUE)
cv.ridge <- cv.glmnet(x,y, alpha=0)
plot(cv.ridge)
coef.cv.glmnet(cv.ridge,s="lambda.min")

fit.lasso <- glmnet(x,y,alpha = 1)
plot(fit.lasso,xvar = "lambda",label = TRUE)
cv.lasso <- cv.glmnet(x,y, alpha=1)
plot(cv.lasso)
coef.cv.glmnet(cv.lasso,s="lambda.min")

###########Supervised Learning################
library(caret)

set.seed(1992)
intrain <- createDataPartition(y=Hitters$Salary, p=0.7, list = F)
  
lasso.tr <- glmnet(x[intrain,],y[intrain])
pred <- predict(lasso.tr,x[-intrain,])
dim(pred)
rmse <- sqrt(apply((y[-intrain]-pred)^2,2,mean))

par(mfcol=c(2,1))
plot(lasso.tr$lambda,rmse, type = "b",xlab = "lambda")
plot(log(lasso.tr$lambda),rmse, type = "b",xlab = "log(lambda)")
par(mfrow=c(1,1))

lam.best <- lasso.tr$lambda[order(rmse)[1]]
coef(lasso.tr,s=lam.best)

############## K-fold Cross-Validation ############
# k = 10, alpha = 1 by default
regular.tr.cv <- cv.glmnet(x[intrain,],y[intrain])
pred.regular <- predict.cv.glmnet(regular.tr.cv , x[-intrain,], s="lambda.min")
postResample(pred = pred.regular[,1] , obs = y[-intrain])

#########Alone Regression
training <- Hitters[intrain,]
validation <- Hitters[-intrain,]

fitLM <- train(Salary~.,data = training,method="lm",
               trControl=trainControl(method = "cv",
                                      number = 10))
pre.LM <- predict(fitLM, newdata=validation)
postResample(pred = pre.LM , obs = y[-intrain])
