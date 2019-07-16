## load required libraries
library(MASS)
library(caret)
## library for nueral network algorithm
library(neuralnet)

## read Boston dataset from MASS library in variable 'df'
df <- Boston
## plot histogram of response variable 'medv' (median house value)
hist(df$medv)
## get min & max values of all columns in the dataset
maxval <- apply(df,2,max)
minval <- apply(df,2,min)

## scale Boston data using min & max values and assign it to 'df1'
df1 <- as.data.frame(scale(df, center = minval, scale=maxval-minval))
## set seed
set.seed(7764)
trainIndex <- createDataPartition(y=df1$medv , p=0.7, list = F)

## create random number indices for data partitioning

training <- df1[trainIndex,]
testing <- df1[-trainIndex,]
## read column names
allcolumns <- colnames(df1)
## remove response variable 'medv' from the list
predcolumns <- allcolumns[!allcolumns%in%"medv"]
## create string by concatenating column names with '+'
predvars <- paste(predcolumns, collapse = "+")
## prediction formula necessary for neuralnet model
predformula <- as.formula(paste("medv ~ ",predvars))

## build model on training dataset; set hidden layers of 4 & 2
model.nueral <- neuralnet(formula = predformula, data = training, linear.output = T, hidden = c(4,2))
plot(model.nueral)

## use the above model on testing dataset to predict
pred.neural <- compute(model.nueral,testing[,1:13])
str(pred.neural)

## view neural network result
pred.neural$net.result

## rescale predicted & actual values for comparison
predicted <- pred.neural$net.result * (max(testing$medv) - min(testing$medv)) + min(testing$medv)
actual <- testing$medv * (max(testing$medv) - min(testing$medv)) + min(testing$medv)

## observe errors
postResample(predicted,actual)

## plot graph of actual vs predicted values
plot(predicted, actual, col="red",main="actual vs predicted")
abline(0,1,col="black")
