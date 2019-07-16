library(MASS)
data("biopsy")
str(biopsy)

## Sequentially Choosing Observations
training <- biopsy[1:490 , ]
validation <- biopsy[491:699 , ]

prop.table(table(biopsy$class))*100
prop.table(table(training$class))*100
prop.table(table(validation$class))*100

## Choose observations using sample()
set.seed(3456)
intrain <- sample(1:699, 490, replace = F )

training <- biopsy[intrain , ]
validation <- biopsy[-intrain , ]

prop.table(table(biopsy$class))*100
prop.table(table(training$class))*100
prop.table(table(validation$class))*100

library(caTools)
intrain <- sample.split(Y=biopsy$class,SplitRatio = 0.7)
training <- biopsy[intrain , ]
validation <- biopsy[-intrain , ]

##OR

library(caret)
set.seed(3456)
intrain <- createDataPartition(y=biopsy$class, p=0.7, list = F)
training <- biopsy[intrain , ]
validation <- biopsy[-intrain , ]

prop.table(table(biopsy$class))*100
prop.table(table(training$class))*100
prop.table(table(validation$class))*100

### Regression
data("Sacramento")

set.seed(3456)
intrain <- sample(1:932, 655, replace = F )

training <- Sacramento[intrain , ]
validation <- Sacramento[-intrain , ]

sd(Sacramento$price)
sd(training$price)
sd(validation$price)

library(caret)
set.seed(3456)
intrain <- createDataPartition(y=Sacramento$price, p=0.7, list = F)
training <- Sacramento[intrain , ]
validation <- Sacramento[-intrain , ]

sd(Sacramento$price)
sd(training$price)
sd(validation$price)
#fit.LM <- lm(price~ . - zip , data = Sacramento)
