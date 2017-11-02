library(caret)
library(kernlab)

data("spam")

# data splitting
inTrain <- createDataPartition(y=spam$type,
                               p=0.75, list=FALSE)
training <- spam[inTrain, ]
testing <- spam[-inTrain, ]
dim(training)

# fit a model
set.seed(32343)
modelFit <- train(type ~ ., data=training, method="glm")
modelFit

# final model
modelFit$finalModel

# predicttion
predictions <- predict(modelFit, newdata=testing)
predictions

# Confusion Matrix
confusionMatrix(predictions, testing$type)
