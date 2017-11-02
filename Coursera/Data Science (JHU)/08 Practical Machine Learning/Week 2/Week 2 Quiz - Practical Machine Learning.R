# 1 #

install.packages("AppliedPredictiveModeling")
# Load the Alzheimer's disease data using the commands:
library(AppliedPredictiveModeling)
data("AlzheimerDisease")

# Which of the following commands will create non-overlapping training and test sets
# with about 50% of the observations assigned to each?
# Answer:
adData = data.frame(diagnosis,predictors)
trainIndex = createDataPartition(diagnosis, p = 0.50,list=FALSE)
training = adData[trainIndex,]
testing = adData[-trainIndex,]

# Cleaning the environment
rm(list = ls())




# 2 #
# Load the cement data using the commands:
library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(1000)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]

# Make a plot of the outcome (CompressiveStrength) versus the index of the samples.
# Color by each of the variables in the data set 
# (you may find the cut2() function in the Hmisc package useful 
# for turning continuous covariates into factors). 
# What do you notice in these plots?
library(Hmisc)
plot(training$CompressiveStrength, pch=19,cex=0.5)

cutCement <- cut2(training$Cement, g=3)
cutBlast <- cut2(training$BlastFurnaceSlag, g=3)
cutFlyAsh <- cut2(training$FlyAsh, g=3)
cutWater <- cut2(training$Water, g=3)
cutSuperPlast <- cut2(training$Superplasticizer, g=3)
cutCoarse <- cut2(training$CoarseAggregate, g=3)
cutFine <- cut2(training$FineAggregate, g=3)
cutAge <- cut2(training$Age, g=3)
cutCompStrength <- cut2(training$CompressiveStrength, g=4)

plot(training$CompressiveStrength, col=cutCement,  pch=19,cex=0.5)
plot(training$CompressiveStrength, col=cutBlast,  pch=19,cex=0.5)
plot(training$CompressiveStrength, col=cutFlyAsh,  pch=19,cex=0.5)
plot(training$CompressiveStrength, col=cutWater,  pch=19,cex=0.5)
plot(training$CompressiveStrength, col=cutSuperPlast,  pch=19,cex=0.5)
plot(training$CompressiveStrength, col=cutCoarse,  pch=19,cex=0.5)
plot(training$CompressiveStrength, col=cutFine,  pch=19,cex=0.5)
plot(training$CompressiveStrength, col=cutAge,  pch=19,cex=0.5)
plot(training$CompressiveStrength, col=cutCompStrength,  pch=19,cex=0.5)

plot(CompressiveStrength~Age, data=training, col=cutAge)
plot(CompressiveStrength~FlyAsh, data=training, col=cutFlyAsh)

# Answer:
# There is a non-random pattern in the plot of the outcome versus index
# that does not appear to be perfectly explained by any predictor suggesting a variable may be missing.

# 3 #
# Load the cement data:
library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(1000)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]


# Make a histogram and confirm the SuperPlasticizer variable is skewed.
# Normally you might use the log transform to try to make the data more symmetric.
# Why would that be a poor choice for this variable?
qplot(training$Superplasticizer)
qplot(log(training$Superplasticizer + 1))
# Answer:
# There are a large number of values that are the same 
# and even if you took the log(SuperPlasticizer + 1) 
# they would still all be identical so the distribution would not be symmetric.


# 4 #
# Load the Alzheimer's disease data using the commands:
library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

# Find all the predictor variables in the training set that begin with IL
# Perform principal components on these variables with the preProcess() function from the caret package.
# Calculate the number of principal components needed to capture 80% of the variance.
# How many are there?

IL_col_idx <- grep("^[Ii][Ll].*", names(training))
preObj <- preProcess(training[, IL_col_idx], method="pca", thresh=0.8)
preObj


# 5 #
# Load the Alzheimer's disease data using the commands:
library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

# Create a training data set consisting of only the predictors 
# with variable names beginning with IL and the diagnosis.
# Build two predictive models, one using the predictors as they are 
# and one using PCA with principal components explaining 80% of the variance in the predictors.
# Use method="glm" in the train function.
# Q: What is the accuracy of each method in the test set? Which is more accurate?

# Create index with variable names beginning with IL
IL_col_idx <- grep("^[Ii][Ll].*", names(training))
# Adding index of the diagnosis column
diag_and_IL_col_idx <- c(1, IL_col_idx) # 1 is the diagnosis column

# 1.The model using all the predictors (Non-PCA)
training_new <- training[, diag_and_IL_col_idx]
testing_new <- testing[, diag_and_IL_col_idx]

ModelAll <- train(diagnosis ~ ., data = training_new, method = "glm")
confM1 <- confusionMatrix(testing_new$diagnosis, predict(ModelAll, testing_new))

# 2. The model using PCA with principal components explaining 80% of the variance in the predictors
# create preprocess object
preProc <- preProcess(training_new[, -1], method = "pca", thresh = .8)

# calculate PCs for training data
trainPC <- predict(preProc, training_new[, -1])

# run model on outcome and principle components
# !!!!IMPORTANT:  In more recent releases of caret, the authors encourage people not to use the formula style syntax
# next line won't work due to changes in caret:
ModelPCA <- train(training_new$diagnosis ~ ., method = "glm", data = trainPC) # undefined columns selected error

# use this instead
ModelPCA <- train(x = trainPC, y = training_new$diagnosis, method = "glm")
# calculate PCs for test data
testPC <- predict(preProc, testing_new[, -1])

confM2 <- confusionMatrix(testing_new$diagnosis, predict(ModelPCA, testPC))
confM1$overall
confM2$overall
# Answer: 
# Non-PCA Accuracy: 0.65
# PCA Accuracy: 0.72