letters <- read.csv('letters_ABPR.csv')

# 1

letters$isB <- as.factor(letters$letter == "B")
# splitting the test set
set.seed(1000)
library(caTools)
split  <-  sample.split(letters$isB, SplitRatio = 0.5)
train <- subset(letters, split == TRUE)
test <- subset(letters, split == FALSE)
table(test$isB)
1175/(1175+383) # 1.1 0.754172

library(rpart)
CARTb <- rpart(isB ~ . - letter, data=train, method="class")
predictions_test_CARTb <- predict(CARTb, newdata=test, type='class')
confMat1 <- table(test$isB, predictions_test_CARTb)
sum(diag(confMat1))/nrow(test) # 1.2 0.9396662

library(randomForest)
set.seed(1000)
RF_b <- randomForest(isB ~ . - letter, data=train)
predictions_test_RF_b <- predict(RF_b, newdata=test)
confMat2 <- table(test$isB, predictions_test_RF_b)
confMat2
sum(diag(confMat2))/nrow(test) # 1.3 0.9878049


# 2
letters$letter <- as.factor(letters$letter)
set.seed(2000)
split2  <-  sample.split(letters$letter, SplitRatio = 0.5)
train2 <- subset(letters, split2 == TRUE)
test2 <- subset(letters, split2 == FALSE)
confMat3 <- table(test2$letter) #letter 'P' is most common class
max(confMat3)/nrow(test2) # 2.1 0.2573813

CART_letter <- rpart(letter ~ . - isB, data=train2, method="class")
pred_test_CART_letter <- predict(CART_letter, newdata=test2, type='class')
confMat4 <- table(test2$letter, pred_test_CART_letter)
confMat4
sum(diag(confMat4))/nrow(test2) # 2.2 0.8786906

set.seed(1000)
RF_letter <- randomForest(letter ~ . - isB, data=train2)
pred_test_RF_letter <- predict(RF_letter, newdata=test2)
confMat5 <- table(test2$letter, pred_test_RF_letter)
confMat5
sum(diag(confMat5))/nrow(test2) # 2.2 0.9801027

plot(RF_letter)
