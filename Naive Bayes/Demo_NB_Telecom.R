minitel <- read.csv("F:\\Statistics\\Datasets\\mini_telcom.csv")

library(e1071)
classifier <- naiveBayes(minitel[,1:2], minitel[,3]) 

testmini <- read.csv("F:\\Statistics\\Datasets\\test_mini.csv")

PredY <- predict(classifier, newdata=testmini, type="class")


PredYProb <- predict(classifier, newdata=testmini,type="raw")

