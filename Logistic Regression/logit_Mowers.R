mowers <- read.csv("F:\\Statistics\\Datasets\\RidingMowers.csv")

attach(mowers)

library(caret)
set.seed(333)
intrain<-createDataPartition(y=mowers$Response,p=0.7,list=FALSE)

training<-mowers[intrain,]
validation<-mowers[-intrain,]




mylogit <- glm(Response ~ Income + Lot_Size,
               data =training, family=binomial(link="logit"),
               na.action=na.pass)
summary(mylogit)
pseudo_R_sq <-  1 - (69.193/171.871)
confint(mylogit)
exp(mylogit$coefficients)

pred_Cust <- predict(mylogit,newdata=validation,type="response")



p_own_t_f <- as.numeric(pred_Cust<0.5) 
fact_p_own_t_f <- factor(p_own_t_f,levels=c(1,0),labels=c("Bought","Not Bought"))
fact_test <- factor( validation$Response,levels=c("Bought","Not Bought"))
confusionMatrix(table(fact_p_own_t_f,fact_test,dnn=list('predicted','actual')))

library(pROC)
plot.roc(validation$Response,pred_Cust,legacy.axes=TRUE,print.auc=TRUE )



mowers2 <- read.csv("E:/Statistics/NB_k_NN/TestMowers.csv")

predicted <-predict(mylogit,newdata=mowers2,type="response")

data.frame(mowers2,as.numeric(predicted>0.5))
