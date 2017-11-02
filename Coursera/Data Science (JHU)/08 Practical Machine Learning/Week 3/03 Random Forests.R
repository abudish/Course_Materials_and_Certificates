install.packages("randomForest")
data("iris")
library(ggplot2)
library(caret)

# Creating training and test sets
inTrain <- createDataPartition(y=iris$Species,
                               p=0.7, list=FALSE)
training <- iris[inTrain, ]
testing <- iris[-inTrain, ]

# Random forests
modFit <- train(Species ~ ., data=training, method="rf", prox=TRUE)
modFit

# Getting a single tree
getTree(modFit$finalModel, k=2)


# Class "centers"

# compute cluster centers
irisP <- classCenter(training[,c(3,4)], training$Species, modFit$finalModel$prox)
# convert irisP to data frame and add Species column
irisP <- as.data.frame(irisP)
irisP$Species <- rownames(irisP)
# plot data points
p <- qplot(Petal.Width, Petal.Length, col=Species,data=training)
# add the cluster centers
p + geom_point(aes(x=Petal.Width,y=Petal.Length,col=Species),size=5,shape=4,data=irisP)


# Predicting new values
pred <- predict(modFit, testing)
testing$predRight <- pred == testing$Species
table(pred, testing$Species)
confusionMatrix(testing$Species, pred)

# plot data points with the incorrect classification highlighted
qplot(Petal.Width,Petal.Length,colour=predRight,data=testing,main="newdata Predictions")
