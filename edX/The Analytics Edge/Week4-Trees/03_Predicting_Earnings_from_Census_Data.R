# 1
census <- read.csv('census.csv')
str(census)
set.seed(2000)
#library(caTools)
split  <-  sample.split(census$over50k, SplitRatio = 0.6)
train <- subset(census, split == TRUE)
test <- subset(census, split == FALSE)
census_LogReg1 <- glm(over50k ~ ., data=train, family='binomial')
summary(census_LogReg1) # 1.1 just name significant variables

predict_test_LogReg1 <- predict(census_LogReg1,  newdata=test, type='response')
confMat_LogReg1 <- table(test$over50k, predict_test_LogReg1 >= 0.5)
confMat_LogReg1
sum(diag(confMat_LogReg1))/nrow(test) # 1.2 0.8552107

table(test$over50k)
9713/(3078 + 9713) #1.3 0.7593621

library(ROCR)
ROCR_logReg1  <-  prediction(predict_test_LogReg1, test$over50k)
as.numeric(performance(ROCR_logReg1, "auc")@y.values) #1.4 0.9061598



#2
library(rpart)
library(rpart.plot)
census_CART <- rpart(over50k ~ ., data=train, method='class')
prp(census_CART)#2.1 4 splits total
                #2.2 1st split is on relationship
                #2.3 2nd split is on capitalgain and education

predTest_CART <- predict(census_CART, newdata=test, type='class') 
confMat_CART <- table(test$over50k, predTest_CART)
confMat_CART
sum(diag(confMat_CART))/nrow(test) #2.4 0.8473927

predTest_CART_ROC <- predict(census_CART, newdata=test)
predTest_CART_ROC_over50k <- predTest_CART_ROC[, 2]

ROCR_pred <- prediction(predTest_CART_ROC_over50k, test$over50k)
ROCRperf <-  performance(ROCR_pred, "tpr", "fpr")

# Plot ROC curve, add colors and threshold labels
plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))
# 2.5 The probabilities from the CART model take only a handful of values 
# (five, one for each end bucket/leaf of the tree);
# the changes in the ROC curve correspond to setting the threshold to one of those values

as.numeric(performance(ROCR_pred, "auc")@y.values) #2.6 0.8470256


# 3
set.seed(1)
trainSmall = train[sample(nrow(train), 2000), ]
set.seed(1)
census_RF <- randomForest(over50k ~ ., data=trainSmall)
pred_test_RF <- predict(census_RF, newdata=test)
confMat_RF <- table(test$over50k, pred_test_RF)
confMat_RF
sum(diag(confMat_RF))/nrow(test) # 3.1 0.8348839

vu = varUsed(census_RF, count=TRUE)
vusorted = sort(vu, decreasing = FALSE, index.return = TRUE)
dotchart(vusorted$x, names(census_RF$forest$xlevels[vusorted$ix])) #3.2 age

varImpPlot(census_RF) #3.3 occupation



# 4
library(caret)
library(e1071)
set.seed(2)
cartGrid <- expand.grid( .cp = seq(0.002,0.1,0.002))

# Define cross-validation experiment
numFolds = trainControl(method = "cv", number = 10)
cartGrid = expand.grid(.cp = seq(0.002,0.1,0.002))

# Perform the cross validation
train(over50k ~ ., data = train, method = "rpart", trControl = numFolds, tuneGrid = cartGrid)
        # 4.1 cp = 0.002


# Create a new CART model
census_CART2 <- rpart(over50k ~ ., data = train, method="class", cp = 0.002)

predTest_CART2 <- predict(census_CART2, newdata=test, type='class') 
confMat_CART2 <- table(test$over50k, predTest_CART2)
confMat_CART2
sum(diag(confMat_CART2))/nrow(test) #4.2 0.8612306

prp(census_CART2) # 4.3 18 splits total
