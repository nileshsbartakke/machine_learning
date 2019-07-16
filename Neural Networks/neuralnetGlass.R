library(mlbench)
data(Glass)

library(caret)
set.seed(1992)
intrain<-createDataPartition(y=Glass$Type, p=0.7,list=FALSE)

training   <- Glass[ intrain , ]
validation <- Glass[-intrain , ]

n <- names(training)
f <- as.formula(paste("Type1+Type2+Type3+Type5+Type6+Type7 ~", paste(n[!n %in% "Type"], collapse = " + ")))
f

mat <- model.matrix(~.-1, data = training)
library(neuralnet)
## build model on training dataset; set hidden layers of 4 & 2
model.neural <- neuralnet(formula = f, data = mat, 
                          stepmax = 1e+9,
                          hidden = c(4,2))
plot(model.neural)

val <- as.matrix(validation[,-10])
## use the above model on testing dataset to predict
pred.neural <- compute(model.neural,validation[,-10])
str(pred.neural)

generateOutput <- function(input) {
  gtypes <- c(1,2,3,5,6,7)
  l <- length(input)
  for(i in 1:l) {
    if(input[i] == max(input)) output <- gtypes[i]
  }
  output
}

pred.class <- factor(apply(pred.neural$net.result, 1, generateOutput))

confusionMatrix(pred.class,validation$Type)
