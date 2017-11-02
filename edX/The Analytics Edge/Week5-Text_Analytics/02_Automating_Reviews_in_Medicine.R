# !!!! Run next line - really important!!!
Sys.setlocale("LC_ALL", "C")

#1
trials <- read.csv('clinical_trial.csv', stringsAsFactors=FALSE)
str(trials)
max(nchar(trials$abstract)) #1.1 3708
summary(nchar(trials$abstract))

sum(nchar(trials$abstract)==0) #1.2 112

short_title_index <- which.min(nchar(trials$title))
trials$title[short_title_index] #1.3 A decade of letrozole: FACE.



# 2
library(tm)
library(SnowballC)
#1) Convert the title variable to corpusTitle and the abstract variable to corpusAbstract.
corpusTitle <- Corpus(VectorSource(trials$title))
corpusAbstract <- Corpus(VectorSource(trials$abstract))

#2) Convert corpusTitle and corpusAbstract to lowercase. 
corpusTitle <- tm_map(corpusTitle, tolower)
corpusAbstract <- tm_map(corpusAbstract, tolower)

#3) Remove the punctuation in corpusTitle and corpusAbstract.
corpusTitle <- tm_map(corpusTitle, removePunctuation)
corpusAbstract <- tm_map(corpusAbstract, removePunctuation)

#4) Remove the English language stop words from corpusTitle and corpusAbstract.
corpusTitle <- tm_map(corpusTitle, removeWords, stopwords('english'))
corpusAbstract <- tm_map(corpusAbstract, removeWords, stopwords('english'))

#5) Stem the words in corpusTitle and corpusAbstract (each stemming might take a few minutes).
corpusTitle <- tm_map(corpusTitle, stemDocument)
corpusAbstract <- tm_map(corpusAbstract, stemDocument)

#6) Build a document term matrix called dtmTitle from corpusTitle and dtmAbstract from corpusAbstract.
dtmTitle <- DocumentTermMatrix(corpusTitle)
dtmAbstract <- DocumentTermMatrix(corpusAbstract)

#7) Limit dtmTitle and dtmAbstract to terms with sparseness of at most 95% (aka terms that appear in at least 5% of documents).
dtmTitle <- removeSparseTerms(dtmTitle, 0.95)
dtmAbstract <- removeSparseTerms(dtmAbstract, 0.95)

#8) Convert dtmTitle and dtmAbstract to data frames (keep the names dtmTitle and dtmAbstract).
dtmTitle <- as.data.frame(as.matrix(dtmTitle))
dtmAbstract <- as.data.frame(as.matrix(dtmAbstract))
ncol(dtmTitle) # 2.1 31
ncol(dtmAbstract)# 2.1 335

#2.2 Abstracts tend to have many more words than titles

csAbstract <- colSums(dtmAbstract)
which.max(csAbstract) #2.3 patient

# 3 
colnames(dtmTitle) <- paste0("T", colnames(dtmTitle))
colnames(dtmAbstract) <- paste0("A", colnames(dtmAbstract))
#3.1 Adding the letter T in front of all the title variable names
#    and adding the letter A in front of all the abstract variable names.

dtm <- cbind(dtmTitle, dtmAbstract)
dtm$trial <- trials$trial
ncol(dtm) #3.2 367

library(caTools)
set.seed(144)
spl <- sample.split(dtm$trial, SplitRatio=0.7)
train <- subset(dtm, spl==TRUE)
test <- subset(dtm, spl==FALSE)
table(train$trial)
730/(572 + 730) #3.3 0.5606759

library(rpart)
library(rpart.plot)
trialCART <- rpart(trial ~ ., data=train, method='class')
prp(trialCART) #3.4 Tphase

predTrain <- predict(trialCART)[, 2]
max(predTrain) #3.5 0.8718861

# 3.6 The maximum predicted probability will likely be exactly the same in the testing set.

predTrain2 <- predict(trialCART, type='class')
confMat1 <- table(train$trial, predTrain2)
confMat1
sum(diag(confMat1))/nrow(train) # 3.7 Accuracy 0.8233487
441/(441 + 131) # 3.7 sensitivity 0.770979
631/(631 + 99) # 3.7 specificity 0.8643836


# 4
predTest <- predict(trialCART, newdata=test)[, 2]
confMat2 <- table(test$trial, predTest >= 0.5)
confMat2
sum(diag(confMat2))/nrow(test) # 4.1 0.7580645

library(ROCR)
predROCR <- prediction(predTest, test$trial)
as.numeric(performance(predROCR, "auc")@y.values) #4.2 0.8371063

# 5
# 5.1 A paper that should have been included in Set A will be missed,
#     affecting the quality of the results of Step 3

# 5.2 A paper will be mistakenly added to Set A, yielding additional work
#     in Step 2 of the process but not affecting the quality of the results of Step 3. 
#     See explanations in related png file

perfROCR <- performance(predROCR, "tpr", "fpr")
plot(perfROCR, colorize=TRUE)

# 5.3 A false negative is more costly than a false positive; the decision maker should use
#     a probability threshold less than 0.5 for the machine learning model.    
