# Two levels of caovariate creation

# Level 1: From raw data to covariate
# Exapmle: email -> parse into words -> create table -> count capital letters, number of $ signs, etc.


# Level 2: Transforming tidy covariates
# Example:
library(kernlab)
data(spam)
spam$capitalAveSq <- spam$capitalAve^2



# Load example data
library(ISLR)
library(caret)
data(Wage)

inTrain <- createDataPartition(y=Wage$wage,
                               p=0.7, list=FALSE)
training <- Wage[inTrain, ]
testing <- Wage[-inTrain, ]

# Common covariates to add, dummy variables
# Basic idea: convert factor variables to indicator variables
table(training$jobclass)
dummies <- dummyVars(wage ~ jobclass, data=training)
head(predict(dummies, newdata = training))

# Removing zero covariates - identifing variables with small variance, which are not good predictors
nsv <- nearZeroVar(training, saveMetrics = TRUE)
nsv # you can use this table to through out variables that has 0 freqRatio or zeroVar = TRUE



# Spline basis - used if you want curved lines to fit the model
library(splines)
# Creating polynomial variable
bsBasis <- bs(training$age, df=3)
bsBasis # the outomes is scaled age: 1st column - age, 2nd- age^2, 3rd - age^3

# Fitting curves with splines
lm1 <- lm(wage ~ bsBasis, data=training)
plot(training$age, training$wage, pch=19, cex=0.5)
points(training$age, predict(lm1, newdata=training), col="red", pch=19, cex=0.5)

# Splines on the test set
predict(bsBasis, age=testing$age)


# Extra Bonus: Multicore Parallel Processing
# many of the algorithms in the caret package are computationally intensive
# since most of the modern machines have multiple cores on their CPUs, it is often wise to enable multicore parallel processing to expedite the computations
# doMC package is recommended to be used for caret computations (reference)
doMC::registerDoMC(cores=4) #= registers 4 cores for R to utilize
# the number of cores you should specify depends on the CPU on your computer (system information usually contains the number of cores)
# it’s also possible to find the number of cores by directly searching for your CPU model number on the Internet
# Note: once registered, you should see in your task manager/activity monitor that 4 “R Session” appear when you run your code