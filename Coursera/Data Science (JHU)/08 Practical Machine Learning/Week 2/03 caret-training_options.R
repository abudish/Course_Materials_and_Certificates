library(caret)
library(kernlab)
data("spam")

# data splitting
inTrain <- createDataPartition(y=spam$type,
                               p=0.75, list=FALSE)
training <- spam[inTrain, ]
testing <- spam[-inTrain, ]
modelFit <- train(type ~ ., data=training, method="glm")

help(train)
args(trainControl)


# seed example
set.seed(1235)
modelFit2 <- train(type ~ ., data = training, method="glm")
modelFit2
# Using the same seed gives exactly the same result (model)
set.seed(1235)
modelFit3 <- train(type ~ ., data = training, method="glm")
modelFit3