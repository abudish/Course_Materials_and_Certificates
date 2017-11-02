#1
stocks <- read.csv('StocksCluster.csv')
nrow(stocks) #1.1 

mean(stocks$PositiveDec)#1.2 0.546114
cor(stocks[, -stocks$PositiveDec])#1.3 the largest correlation coefficient is 
                                  #    0.19167279, between ReturnOct and ReturnNov

library(dplyr)
stocks %>% 
  select(-PositiveDec) %>% 
  colMeans() %>% 
  which.max#1.3 April

stocks %>% 
  select(-PositiveDec) %>% 
  colMeans() %>% 
  which.min#1.3 September

#2
#stocks$PositiveDec <- as.factor(stocks$PositiveDec)
set.seed(144)
library(caTools)
spl <- sample.split(stocks$PositiveDec, SplitRatio = 0.7)
stocksTrain <- subset(stocks, spl == TRUE)
stocksTest <- subset(stocks, spl == FALSE)
StocksModel <- glm(PositiveDec ~ ., family='binomial', data=stocksTrain)
summary(StocksModel)

predTrain <- predict(StocksModel, newdata=stocksTrain, type='response')
confMat_train_glm <- table(stocksTrain$PositiveDec, predTrain > 0.5)
sum(diag(confMat_train_glm)/nrow(stocksTrain))
(990 + 3640)/(990 + 2689 + 787 + 3640) #2.1 0.5711818

predTest <- predict(StocksModel, newdata=stocksTest, type='response')
confMat_test_glm <- table(stocksTest$PositiveDec, predTest > 0.5)
sum(diag(confMat_test_glm)/nrow(stocksTest))
(417 + 1553)/(417 + 1160 + 344 + 1553) #2.2 0.5670697

table(stocksTest$PositiveDec == 1)
1897/(1897+1577) #2.3

#3
limitedTrain <- stocksTrain
limitedTrain$PositiveDec <- NULL
limitedTest <- stocksTest
limitedTest$PositiveDec <- NULL#3.1 Needing to know the dependent variable value
# to assign an observation to a cluster defeats the purpose of the methodology

library(caret)
preproc <- preProcess(limitedTrain)
normTrain <- predict(preproc, limitedTrain)
normTest <- predict(preproc, limitedTest)
mean(normTrain$ReturnJan)#3.2 2.100586e-17
mean(normTest$ReturnJan)#3.2 -0.0004185886

#3.3 The distribution of the ReturnJan variable is different in the training and testing set 

set.seed(144)
km <- kmeans(normTrain, centers=3)
table(km$cluster)#3.4 Cluster 2

library(flexclust)
km.kcca <- as.kcca(km, normTrain)
clusterTrain <- predict(km.kcca)
clusterTest <- predict(km.kcca, newdata=normTest)
table(clusterTest)#3.5 2080


#4
stocksTrain1 <- subset(stocksTrain, clusterTrain == 1)
stocksTrain2 <- subset(stocksTrain, clusterTrain == 2)
stocksTrain3 <- subset(stocksTrain, clusterTrain == 3)

stocksTest1 <- subset(stocksTest, clusterTest == 1)
stocksTest2 <- subset(stocksTest, clusterTest == 2)
stocksTest3 <- subset(stocksTest, clusterTest == 3)

mean(stocksTrain1$PositiveDec)#4.1 stocksTrain1
mean(stocksTrain2$PositiveDec)
mean(stocksTrain3$PositiveDec)

StocksModel1 <- glm(PositiveDec ~ ., family='binomial', data=stocksTrain1)
StocksModel2 <- glm(PositiveDec ~ ., family='binomial', data=stocksTrain2)
StocksModel3 <- glm(PositiveDec ~ ., family='binomial', data=stocksTrain3)

summary(StocksModel1)
summary(StocksModel2)
summary(StocksModel3)#4.2 ReturnJan, ReturnFeb, ReturnMar, ReturnJune, ReturnAug,
                     #    and ReturnOct differ in sign between the models.


PredictTest1  <- predict(StocksModel1, newdata=stocksTest1, type='response')
PredictTest2  <- predict(StocksModel2, newdata=stocksTest2, type='response')
PredictTest3  <- predict(StocksModel3, newdata=stocksTest3, type='response')

confMat_test_glm_1 <- table(stocksTest1$PositiveDec, PredictTest1 > 0.5)
confMat_test_glm_2 <- table(stocksTest2$PositiveDec, PredictTest2 > 0.5)
confMat_test_glm_3 <- table(stocksTest3$PositiveDec, PredictTest3 > 0.5)

sum(diag(confMat_test_glm_1))/nrow(stocksTest1)#4.3 0.6194145
(30 + 774)/(30 + 471 + 23 + 774) 
sum(diag(confMat_test_glm_2))/nrow(stocksTest2)#4.3 0.5504808
(388 + 757)/(388 + 626 + 309 + 757)
sum(diag(confMat_test_glm_3))/nrow(stocksTest3)#4.3 0.6458333
(49 + 13)/(49 + 13 + 21 + 13)

AllPredictions <- c(PredictTest1, PredictTest2, PredictTest3)
AllOutcomes <- c(stocksTest1$PositiveDec, stocksTest2$PositiveDec, stocksTest3$PositiveDec)

confMat_test_all <- table(AllOutcomes, AllPredictions > 0.5)
confMat_test_all
(1544 + 467)/(1544 + 467 + 1110 + 353) #4.4 0.5788716
