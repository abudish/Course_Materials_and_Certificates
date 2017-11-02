bank <- read.csv('bank.csv')

# 1 #
# What is the average age in the data set?
mean(bank$age) #1.1 39.5814


# 2 #
library(ggplot2)
ggplot(bank, aes(job, duration)) + geom_boxplot()
# Which three jobs have the longest average call durations?
sort(tapply(bank$duration, INDEX=bank$job, FUN=mean)) #2.1 housemaid, self-employed retired


# 3 #
library(dplyr)
bank_truncated <- bank %>% 
        select(emp.var.rate, cons.price.idx, cons.conf.idx, euribor3m, nr.employed)
cor(bank_truncated)
min(cor(bank_truncated))
# Which of the following statements are correct (limited to just these selected variables)?
#3.1 cons.conf.idx does NOT seem to have severe multicolinearity with the other variables,
#    cons.price.idx and cons.conf.idx have the lowest correlation between two different variables


# 4 #
set.seed(201)
library(caTools)

spl <- sample.split(bank$y, 0.7)
training <- bank[spl==TRUE, ]
testing <- bank[spl!=TRUE, ]
# Why do we use the sample.split() function to split into a training and testing set?
#4.1 It balances the dependent variable between the training and testing sets


# 5 #
bankLog1 <- glm(y ~ age + job + marital + education + default + housing + loan +
                        contact + month + day_of_week + campaign + pdays + previous + 
                        poutcome + emp.var.rate + cons.price.idx + cons.conf.idx, 
                data=training, family='binomial')
# Which of the following characteristics are statistically significantly 
# POSITIVELY (at 0.05 level) associated with an increased chance of 
# subscribing to the product?
summary(bankLog1) #5 age, month is August, month is March, poutcome is nonexistent,
                  #  cons.price.idx

# 6 #
#What is the meaning of the coefficient labeled "monthmar" in the logistic regression summary output?
(exp(1.286) - 1)*100 #6 When the month is March, the odds of subscribing to 
                     #  the product are 261.8% higher than an otherwise identical contact.



# 7 #
table(training$y)#the most common is 0 outcome - baseline model is always predict 0

TestPrediction <- predict(bankLog1, newdata=testing, type='response')
confMat <- table(testing$y, TestPrediction >= 0.5)
#7 What is the number of test set observations where the prediction from the
#  logistic regression model is different than the prediction from the baseline model?
confMat
table(testing$y, TestPrediction == 0) 
1273 + 133 - (1323 + 177) #7 94


# 8 #

library(ROCR)
ROCRpred  <-  prediction(TestPrediction, testing$y)
#8 What is the test-set AUC of the logistic regression model?
as.numeric(performance(ROCRpred, "auc")@y.values) #8 0.7507334


# 9 #
#9 What is the meaning of the AUC?
  # The proportion of the time the model can differentiate between a randomly 
  # selected client who subscribed to a term deposit and a randomly selected 
  # client who did not subscribe


# 10 #
ROCRperf <-  performance(ROCRpred, "tpr", "fpr")
plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))
#10 Which logistic regression threshold is associated with the upper-right 
#   corner of the ROC plot (true positive rate 1 and false positive rate 1)?
# A: 0


# 11 #
#11 At roughly which logistic regression cutoff does the model achieve 
#   a true positive rate of 60% and a false positive rate of 25%?
plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))


# 12 #
# Which of the following best describes how 10-fold cross-validation works when
# selecting between 4 different parameter values?
#12 40 models are trained on subsets of the training set and evaluated 
#   on a portion of the training set


# 13 #
set.seed(201)
library(caret)
library(e1071)

# Define cross-validation experiment
numFolds <- trainControl(method = "cv", number = 10)
cartGrid <- expand.grid(.cp = seq(0.001, 0.05, 0.001))

# Create factor target variable
training$blah <- as.factor(training$y)
# Perform the cross validation
train(blah ~ age + job + marital + education + default + housing + loan +
                  contact + month + day_of_week + campaign + pdays + previous + 
                  poutcome + emp.var.rate + cons.price.idx + cons.conf.idx, 
                data=training, method='rpart', trControl=numFolds, tuneGrid=cartGrid)
#13 cp=0.016



# 14 #
library(rpart)
library(rpart.plot)
CART <- rpart(blah ~ age + job + marital + education + default + housing + loan +
                contact + month + day_of_week + campaign + pdays + previous + 
                poutcome + emp.var.rate + cons.price.idx + cons.conf.idx, 
              data=training, method='class', cp=0.016)
prp(CART)


# 15 #

predTest_CART <- predict(CART, newdata=testing, type='class') 
confMat_CART <- table(testing$y, predTest_CART)
confMat_CART
sum(diag(confMat_CART))/sum(confMat_CART) #15

