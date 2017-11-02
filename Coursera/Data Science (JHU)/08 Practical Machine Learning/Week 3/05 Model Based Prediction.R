# Example : Iris data
data("iris")
library(ggplot2)
library(caret)
names(iris)
table(iris$Species)

# Creating training and test sets
inTrain <- createDataPartition(y=iris$Species,
                               p=0.7, list=FALSE)
training <- iris[inTrain, ]
testing <- iris[-inTrain, ]
dim(training); dim (testing)

# Build predictions

# Build model using Linear Discriminant Analysis
modlda <- train(Species ~ ., data=training, method="lda")
# Build model using Naive Bayes
install.packages("klaR")
library(klaR)
modlnb <- train(Species ~ ., data=training, method="nb")

# Predicting
plda <- predict(modlda, testing)
pnb <- predict(modlnb, testing)
table(plda, pnb)

confusionMatrix(testing$Species, plda)
confusionMatrix(testing$Species, pnb)

# Comparison of results
equalPredictions <- (plda==pnb)
qplot(Petal.Width, Sepal.Width, colour=equalPredictions, data=testing)
