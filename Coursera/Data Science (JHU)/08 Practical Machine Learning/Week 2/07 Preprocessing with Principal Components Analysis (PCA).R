# Correlated predictors
library(caret)
library(kernlab)
data("spam")

inTrain <- createDataPartition(y=spam$type,
                               p=0.75, list=FALSE)
training <- spam[inTrain, ]
testing <- spam[-inTrain, ]

M <- abs(cor(training[, -58]))

# Next line changes correlation variables with itself from 1 to 0
diag(M) <- 0
# Looking at variables with strong correlations
which(M > 0.85, arr.ind = TRUE)

names(spam)[c(34,32)]

plot(spam[,34], spam[,32])
# Next line will be relevant if we use 0.8 for correlation threshold
# pairs(~spam[,34] + spam[,32] + spam[,40] )

# We could rotate the plot
# X = 0.71*num415 + 0.71*num857
# Y = 0.71*num415 - 0.71*num857

X <- 0.71*training$num415 + 0.71*training$num857
Y <- 0.71*training$num415 - 0.71*training$num857
plot(X, Y)

# Single Value Decompostition(SVD) and PCA -> Look up in the notes for Exploratory Data Analysis

# Principal components in R - prcomp. You can do with more than 2 variables also
smallSpam <- spam[, c(34,32)]
prComp <- prcomp(smallSpam)
plot(prComp$x[, 1], prComp$x[, 2])

prComp$rotation


# PCA on SPAM data
typeColor <- ((spam$type=="spam")*1 + 1)
prComp <- prcomp(log10(spam[, -58] + 1)) # log10 + 1 is done to make data more gaussian; original data is skewed

# black dots - ham, red dots - spam
plot(prComp$x[, 1], prComp$x[, 2], col=typeColor, xlab="PC1", ylab="PC2")



# PCA with caret
preProc <- preProcess(log10(spam[, -58] + 1), method = "pca", pcaComp = 2)
spamPC <- predict(preProc, log10(spam[, -58] + 1))
plot(spamPC[, 1], spamPC[, 2], col=typeColor)


# Preprocessing with PCA - training set
preProc <- preProcess(log10(spam[, -58] + 1), method = "pca", pcaComp = 2)
trainPC <- predict(preProc, log10(training[, -58] + 1))
#!!! Next line really IMPORTANT - do not use formula, because of changes in caret, use x and y:
modelFit <- train(x = trainPC, y = training$type, method = "glm")


# Preprocessing with PCA - test set
testPC <- predict(preProc, log10(testing[, -58] + 1))
confusionMatrix(predict(modelFit, testPC), testing$type)

# Alternative - sets numbers of PCs
modelFit <- train(type ~ ., method="glm", preProcess="pca", data=training)
confusionMatrix(predict(modelFit, testing), testing$type)
