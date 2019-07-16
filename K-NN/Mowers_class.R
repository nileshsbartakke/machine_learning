mowers <- read.csv("F:\\Statistics\\Datasets\\RidingMowers.csv")

library(ggplot2)
ggplot(data=mowers, aes(x=Income, y=Lot_Size,color=Response)) +
  geom_point(pch=21, size=2) +
  labs(title="Riding Mowers Response", x="Income('000 $)", y="Land Owned('000 sq.ft.)")

# OR


plot(mowers$Income,mowers$Lot_Size,type = 'n',main = "Riding Mowers Response",
     xlab="Income('000 $)", ylab = "Land Owned('000 sq.ft.)")
buyers <- mowers[mowers$Response=="Bought",]
non_buyers <- mowers[mowers$Response=="Not Bought",]
points(buyers$Income,buyers$Lot_Size,pch=24,col="blue")
points(non_buyers$Income,non_buyers$Lot_Size,pch=21,col="red")
legend("bottomright",c("Bought","Not Bought"),pch = c(24,21),col = c("blue","red"))


# Classifier Program:

library(caret)
set.seed(1992)
intrain<-createDataPartition(y=mowers$Response,p=0.7,list=FALSE)


trainingWOY <- mowers[intrain,-3]
validationWOY <- mowers[-intrain,-3]

YofTraining <- mowers[intrain,3]
YofValidation <- mowers[-intrain,3]


library(class)

knn1.pred <- knn(trainingWOY,validationWOY,YofTraining,k=1)
tbl_1 <- table(knn1.pred , YofValidation)
confusionMatrix( tbl_1 )

knn3.pred=knn(trainingWOY,validationWOY,YofTraining,k=3)
tbl_3 <- table(knn3.pred , YofValidation)
confusionMatrix( tbl_3 )

knn5.pred=knn(trainingWOY,validationWOY,YofTraining,k=5)
tbl_5 <- table(knn5.pred , YofValidation)
confusionMatrix( tbl_5 )

knn7.pred=knn(trainingWOY,validationWOY,YofTraining,k=7)
tbl_7 <- table(knn7.pred , YofValidation)
confusionMatrix( tbl_7 )