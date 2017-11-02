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
dim(training)
dim(testing)
dim(iris)

# Iris petal widths/sepal width
qplot(Petal.Width, Sepal.Width, colour=Species, data=training)


modFit <- train(Species ~ ., method="rpart", data=training)
print(modFit$finalModel)
# Demostraiting why Petal.Length is a good variable for splitting
qplot(Petal.Length, Sepal.Width, colour=Species, data=training)

# Plot tree
plot(modFit$finalModel, uniform = TRUE,
     main="Classification Tree")
text(modFit$finalModel, use.n = TRUE, all=TRUE, cex=.8)

# Prettier plots
install.packages("rattle")
install.packages("rpart.plot")
library(rattle)
fancyRpartPlot(modFit$finalModel, palettes="YlGnBu")

# Predicting new values
predict(modFit, newdata = testing)
confusionMatrix(testing$Species, predict(modFit, newdata = testing))
