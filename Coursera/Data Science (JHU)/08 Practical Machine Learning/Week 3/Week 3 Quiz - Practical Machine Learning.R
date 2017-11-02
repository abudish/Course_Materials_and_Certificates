# For this quiz we will be using several R packages. R package versions change over time,
# the right answers have been checked using the following versions of the packages.
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
# If you aren't using these versions of the packages, your answers may not exactly match the right answer,
# but hopefully should be close.


# 1 #
#Load the cell segmentation data from the AppliedPredictiveModeling package using the commands:
library(AppliedPredictiveModeling)
data(segmentationOriginal)
library(caret)

# 1. Subset the data to a training set and testing set based on the Case variable in the data set.
inTrain <- createDataPartition(segmentationOriginal$Case, 
                               p = 0.7, list = FALSE)
training <- segmentationOriginal[inTrain, ]
testing <- segmentationOriginal[-inTrain, ]


# 2. Set the seed to 125 and fit a CART model with the rpart method 
# using all predictor variables and default caret settings (Prediction variable is class)
set.seed(125)
modFit <- train(Class ~ ., method="rpart", data=training)
print(modFit$finalModel)

# 3. In the final model what would be the final model prediction for cases with the following variable values:
library(rattle)
fancyRpartPlot(modFit$finalModel, palettes="YlGnBu")
# a. TotalIntench2 = 23,000; FiberWidthCh1 = 10; PerimStatusCh1=2
        # PS
# b. TotalIntench2 = 50,000; FiberWidthCh1 = 10;VarIntenCh4 = 100
        # WS
# c. TotalIntench2 = 57,000; FiberWidthCh1 = 8;VarIntenCh4 = 100
        # PS
# d. FiberWidthCh1 = 8;VarIntenCh4 = 100; PerimStatusCh1=2
        # Not possible to answer



# 2 #
# If K is small in a K-fold cross validation is the bias 
# in the estimate of out-of-sample (test set) accuracy smaller or bigger? 
# If K is small is the variance in the estimate of out-of-sample (test set) accuracy smaller or bigger.
# Is K large or small in leave one out cross validation?
# Answer: The bias is larger and the variance is smaller.
        # Under leave one out cross validation K is equal to the sample size.


# 3 #
# Load the olive oil data using the commands:
install.packages("pgmm")
library(pgmm)
data(olive)
olive = olive[,-1]

# These data contain information on 572 different Italian olive oils from multiple regions in Italy.
# Fit a classification tree where Area is the outcome variable. 
modFit <- train(Area ~ ., method="rpart", data=olive)

# Then predict the value of area for the following data frame using the tree command with all defaults
newdata <-  as.data.frame(t(colMeans(olive)))
predict(modFit, newdata)
# What is the resulting prediction? Is the resulting prediction strange? Why or why not?
# Answer: 2.783. It is strange because Area should be a qualitative variable - but tree 
        # is reporting the average value of Area as a numeric variable in the leaf predicted for newdata

# 4 #
# Load the South Africa Heart Disease Data and create training and test sets with the following code:
library(ElemStatLearn)
data(SAheart)
set.seed(8484)
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]

# Then set the seed to 13234 and fit a logistic regression model 
# (method="glm", be sure to specify family="binomial") with Coronary Heart Disease (chd) as the outcome
# and age at onset, current alcohol consumption, obesity levels, cumulative tabacco, type-A behavior,
# and low density lipoprotein cholesterol as predictors.
set.seed(13234)
modelGlmBinom <- train(chd ~ age + alcohol + obesity + tobacco + typea + ldl,
                       data=trainSA,
                       method="glm",
                       family="binomial")
# Calculate the misclassification rate for your model using this function 
# and a prediction on the "response" scale:
missClass = function(values,prediction){sum(((prediction > 0.5)*1) != values)/length(values)}

missClass(trainSA$chd, predict(modelGlmBinom, newdata = trainSA))
missClass(testSA$chd, predict(modelGlmBinom, newdata = testSA))
# Answer: Test Set Misclassification: 0.31 ; Training Set: 0.27


# 5 # 
# Load the vowel.train and vowel.test data sets:
library(ElemStatLearn)
data(vowel.train)
data(vowel.test)

# Set the variable y to be a factor variable in both the training and test set. 
vowel.train$y <- as.factor(vowel.train$y)
vowel.test$y <- as.factor(vowel.test$y)
# Then set the seed to 33833. 
set.seed(33833)
# Fit a random forest predictor relating the factor variable y to the remaining variables. 
# [NOTE: Use randomForest() specifically, not caret, as there's been some issues reported with that approach. 11/6/2016]
#modRandForest <- train(y ~ ., method="rf", data=vowel.train) # this line will result in incorrect answer
modRandForest <- randomForest(y ~ ., data=vowel.train)
# Read about variable importance in random forests here:
#         http://www.stat.berkeley.edu/~breiman/RandomForests/cc_home.htm#ooberr
# The caret package uses by default the Gini importance.


# Calculate the variable importance using the varImp function in the caret package. 
# What is the order of variable importance?
order(varImp(modRandForest), decreasing = TRUE)
# Answer: x.2, x.1, x.5, x.6, x.8, x.4, x.9, x.3, x.7,x.10