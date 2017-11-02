# !!!! Run next line - really important!!!
Sys.setlocale("LC_ALL", "C")

# 1
email <- read.csv('emails.csv', stringsAsFactors=FALSE)
nrow(email) #1.1 5728

table(email$spam) #1.2 1368

email$text[1] #1.3 subject

#1.4 Yes -- the number of times the word appears 
#    might help us differentiate spam from ham. correct

texts_length <- nchar(email$text)
max(texts_length) #1.5 43952

which.min(texts_length) #1.6 1992


#2
library(tm)
library(SnowballC)

corpus <- Corpus(VectorSource(email$text))
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeWords, stopwords('english'))
corpus <- tm_map(corpus, stemDocument)
dtm <- DocumentTermMatrix(corpus)
ncol(dtm) #2.1 28687

spdtm <- removeSparseTerms(dtm, 0.95)
ncol(spdtm) #2.2 330

emailsSparse <- as.data.frame(as.matrix(spdtm))
colnames(emailsSparse) <- make.names(colnames(emailsSparse))

word_freq <- colSums(emailsSparse)
sort(word_freq, decreasing=TRUE)[1] 
which.max(word_freq) #2.3 enron

ham_emails <- subset(emailsSparse, spam == 0,  select= - spam)
word_freq_ham <- colSums(ham_emails)
length(word_freq_ham[word_freq_ham > 5000]) #2.4 6

spam_emails <- subset(emailsSparse, spam == 1,  select= - spam)
word_freq_spam <- colSums(spam_emails)
length(word_freq_spam[word_freq_spam > 1000]) #2.5 3

#2.6 The frequencies of these most common words are likely to help differentiate between spam and ham. 

#2.7 The models we build are personalized, 
#    and would need to be further tested before being used as a spam filter for another person. 

#3 
emailsSparse$spam <- as.factor(emailsSparse$spam)
set.seed(123)
library(caTools)
spl <- sample.split(emailsSparse$spam, SplitRatio=0.7)
train <- subset(emailsSparse, spl==TRUE)
test <- subset(emailsSparse, spl==FALSE)
# 3 models:
spamLog <- glm(spam ~ ., data=train, family='binomial')
library(rpart)
library(rpart.plot)
spamCART <- rpart(spam ~ ., data=train, method='class')
library(randomForest)
set.seed(123)
spamRF <- randomForest(spam ~ ., data=train)
# predict probabilites for 3 models:
predTrain_Log <- predict(spamLog, type='response')
predTrain_CART <- predict(spamCART)[, 2]
predTrain_RF <- predict(spamRF, type='prob')[, 2]

length(predTrain_Log[predTrain_Log < 0.00001]) #3.1 3046
length(predTrain_Log[predTrain_Log > 0.99999]) #3.1 954
length(predTrain_Log[predTrain_Log >= 0.00001 & predTrain_Log <= 0.99999]) #3.1 10
# or
table(predTrain_Log < 0.00001)
table(predTrain_Log > 0.99999)
table(predTrain_Log >= 0.00001 & predTrainLog <= 0.99999)

summary(spamLog) #3.2 0

prp(spamCART)#3.3 2

confMat_Log <- table(train$spam, predTrain_Log > 0.5)
sum(diag(confMat_Log))/nrow(train) #3.4 0.9990025

library(ROCR)
predROCR_Log <- prediction(predTrain_Log, train$spam)
as.numeric(performance(predROCR_Log, "auc")@y.values) #3.5 0.9999959

confMat_CART <- table(train$spam, predTrain_CART > 0.5)
sum(diag(confMat_CART))/nrow(train) #3.6 0.942394

predROCR_CART <- prediction(predTrain_CART, train$spam)
as.numeric(performance(predROCR_CART, "auc")@y.values) #3.7 0.9696044

confMat_RF <- table(train$spam, predTrain_RF > 0.5)
sum(diag(confMat_RF))/nrow(train) #3.8 0.978803

predROCR_RF <- prediction(predTrain_RF, train$spam)
as.numeric(performance(predROCR_RF, "auc")@y.values) #3.9 0.9978502

#3.10 Logistic regression - best model in terms of accuracy and AUC on train set


#4
predTest_Log <- predict(spamLog, newdata=test, type='response')
predTest_CART <- predict(spamCART, newdata=test)[, 2]
predTest_RF <- predict(spamRF, newdata=test, type='prob')[, 2]

confMat_Test_Log <- table(test$spam, predTest_Log > 0.5)
sum(diag(confMat_Test_Log))/nrow(test) #4.1 0.9505239

predROCR_Test_Log <- prediction(predTest_Log, test$spam)
as.numeric(performance(predROCR_Test_Log, "auc")@y.values) #4.2 0.9627517

confMat_Test_CART <- table(test$spam, predTest_CART > 0.5)
sum(diag(confMat_Test_CART))/nrow(test) #4.3 0.9394645

predROCR_Test_CART <- prediction(predTest_CART, test$spam)
as.numeric(performance(predROCR_Test_CART, "auc")@y.values) #4.4 0.963176

confMat_Test_RF <- table(test$spam, predTest_RF > 0.5)
sum(diag(confMat_Test_RF))/nrow(test) #4.5 0.976135

predROCR_Test_RF <- prediction(predTest_RF, test$spam)
as.numeric(performance(predROCR_Test_RF, "auc")@y.values) #4.6 0.997506

#4.7 Random Forest - best model in terms of accuracy and AUC on test set

#4.8 Logistic Regression had the greatest degree of overfitting

