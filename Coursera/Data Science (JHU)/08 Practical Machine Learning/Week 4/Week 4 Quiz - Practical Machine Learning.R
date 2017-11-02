# For this quiz we will be using several R packages. 
# R package versions change over time, the right answers have been checked using the following versions of the packages.
# 
# AppliedPredictiveModeling: v1.1.6
# 
# caret: v6.0.47
# 
# ElemStatLearn: v2012.04-0
# 
# pgmm: v1.1
# 
# rpart: v4.1.8
# 
# gbm: v2.1
# 
# lubridate: v1.3.3
# 
# forecast: v5.6
# 
# e1071: v1.6.4
# 
# If you aren't using these versions of the packages, your answers may not exactly match the right answer,
# but hopefully should be close.


# 1 #
# Load the vowel.train and vowel.test data sets:
        
library(ElemStatLearn)
library(caret)

data(vowel.train)
data(vowel.test) 

# Set the variable y to be a factor variable in both the training and test set
vowel.train$y <- as.factor(vowel.train$y)
vowel.test$y <- as.factor(vowel.test$y)

# Then set the seed to 33833. 
set.seed(33833)

#  Fit (1) a random forest predictor relating the factor variable y to the remaining variables
rf <- train(y ~ ., data=vowel.train, method="rf", prox=TRUE)

# and (2) a boosted predictor using the "gbm" method.
# Fit these both with the train() command in the caret package.
gbm <- train(y ~ ., data=vowel.train, method="gbm", verbose=FALSE)


# What are the accuracies for the two approaches on the test data set?
predRf <- predict(rf, vowel.test)
predGbm <- predict(gbm, vowel.test)

cfmRm <- confusionMatrix(vowel.test$y, predRf)$overall['Accuracy']
cfmGbm <- confusionMatrix(vowel.test$y, predGbm)$overall['Accuracy']

idx_agreed <- predRf == predGbm
cfmAgreed <- confusionMatrix(vowel.test$y[idx_agreed], predRf[idx_agreed])$overall['Accuracy']
#confusionMatrix(predRf, predGbm)$overall[1]
cfmRm
cfmGbm
cfmAgreed
# Since the packages used in these question is newer, the answers in tests
# is slightly different
# Answer: RF Accuracy = 0.6082; GBM Accuracy = 0.5152; Agreement Accuracy = 0.6361


# 2. #
# Load the Alzheimer's data using the following commands
library(caret)
library(gbm)

set.seed(3433)
library(AppliedPredictiveModeling)

data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain, ]
testing = adData[-inTrain, ]

# Set the seed to 62433
set.seed(62433)


# and predict diagnosis with all the other variables using a random forest ("rf"), 
# boosted trees ("gbm") and linear discriminant analysis ("lda") model. 

# Building three different models:
rf <- train(diagnosis ~ ., data=training, method="rf", prox=TRUE)
gbm <- train(diagnosis ~ ., data=training, method="gbm", verbose=FALSE)
lda <- train(diagnosis ~ ., data=training, method="lda")

# Predict on the testing set
predRf <- predict(rf, testing)
predGbm <- predict(gbm, testing)
predLda <- predict(lda, testing)

# Building confusion matrices to get accuracy of the models
cfmRm <- confusionMatrix(testing$diagnosis, predRf)$overall['Accuracy']
cfmGbm <- confusionMatrix(testing$diagnosis, predGbm)$overall['Accuracy']
cfmLda <- confusionMatrix(testing$diagnosis, predLda)$overall['Accuracy']

# Stack the predictions together using random forests ("rf").
predDF <- data.frame(predRf,
                     predGbm,
                     predLda,
                     diagnosis = testing$diagnosis)
combModFit <- train(diagnosis ~ ., method="rf", data=predDF)
predComb <- predict(combModFit, predDF)

# What is the resulting accuracy on the test set? 
# Is it better or worse than each of the individual predictions?
cfmRm
cfmGbm
cfmLda
cfmCombined <- confusionMatrix(testing$diagnosis, predComb)$overall['Accuracy']
cfmCombined

# Answer:Stacked Accuracy: 0.80 is better than random forests and lda and the same as boosting.


# 3 #
# Load the concrete data with the commands:
set.seed(3523)
library(AppliedPredictiveModeling)

data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain, ]
testing = concrete[-inTrain, ]

# Set the seed to 233 and fit a lasso model to predict Compressive Strength.
set.seed(233)
lasso <- train(CompressiveStrength ~ ., data=training, method="lasso")

install.packages("elasticnet")
library(elasticnet)
# Which variable is the last coefficient to be set to zero as the penalty increases?
# (Hint: it may be useful to look up ?plot.enet)
plot.enet(lasso$finalModel, xvar="penalty", use.color=TRUE)
plot(lasso$finalModel)
# Answer: Cement


# 4 #
# Load the data on the number of visitors to the instructors blog from here:
# https://d396qusza40orc.cloudfront.net/predmachlearn/gaData.csv
download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/gaData.csv", "gaData.csv")

library(lubridate) # For year() function below
dat = read.csv("gaData.csv")

training = dat[year(dat$date) < 2012, ]
testing = dat[(year(dat$date)) > 2011, ]
tstrain = ts(training$visitsTumblr)

# Fit a model using the bats() function in the forecast package to the training time series.
library(forecast)
mod_ts <- bats(tstrain)

# Then forecast this model for the remaining time points. 
# For how many of the testing points is the true value within the 95% prediction interval bounds?
fcast <- forecast(mod_ts, h = nrow(testing))

# extracting the 95% prediction boundaries
lower95 <- fcast$lower[, 2]
upper95 <- fcast$upper[, 2]

# see how many of the testing visit counts do actually match
table(
        (testing$visitsTumblr > lower95) &
        (testing$visitsTumblr < upper95)
)
# in percentages
226/nrow(testing)
#Answer: 96%



# 5 #
# Load the concrete data with the commands:
set.seed(3523)
library(AppliedPredictiveModeling)

data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain, ]
testing = concrete[-inTrain, ]

# Set the seed to 325 and fit a support vector machine using the e1071 package
# to predict Compressive Strength using the default settings. 
set.seed(325)
library(e1071)

svm <- svm(CompressiveStrength ~ ., data=training)
# Predict on the testing set. 
pred_svm <- predict(svm, testing)
# What is the RMSE?
accuracy(pred_svm, testing$CompressiveStrength)
# Answer 6.72