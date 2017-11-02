# load data
library(ISLR)
library(ggplot2)
library(caret)
install.packages("gbm") # problems with user name
library(gbm) #
data(Wage)
# remove log wage variable (we are trying to predict wage)
Wage <- subset(Wage,select=-c(logwage))
# create train/test data sets
inTrain <- createDataPartition(y=Wage$wage,p=0.7, list=FALSE)
training <- Wage[inTrain, ]
testing <- Wage[-inTrain, ]
# run the gbm model
modFit <- train(wage ~ ., method="gbm",data=training,verbose=FALSE)
# print model summary
print(modFit)

qplot(predict(modFit, testing), wage, data=testing)
