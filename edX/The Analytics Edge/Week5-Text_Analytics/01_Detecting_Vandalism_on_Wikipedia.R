#1

wiki <- read.csv("wiki.csv", stringsAsFactors=FALSE)
wiki$Vandal <- as.factor(wiki$Vandal)
table(wiki$Vandal) #1.1 1815

# 1) Create the corpus for the Added column, and call it "corpusAdded".
library(tm)
library(SnowballC)
# !!!! Run next line - really important!!!
Sys.setlocale("LC_ALL", "C")
corpusAdded <- Corpus(VectorSource(wiki$Added))
# 2) Remove the English-language stopwords.
corpusAdded <- tm_map(corpusAdded, removeWords, stopwords('english'))
# 3) Stem the words.
corpusAdded <- tm_map(corpusAdded, stemDocument)
# 4) Build the DocumentTermMatrix, and call it dtmAdded.
dtmAdded <- DocumentTermMatrix(corpusAdded)
dtmAdded #1.2 - 6675

sparseAdded <- removeSparseTerms(dtmAdded, 0.997)
sparseAdded #1.3 166

wordsAdded <- as.data.frame(as.matrix(sparseAdded))
colnames(wordsAdded) <- paste("A", colnames(wordsAdded))

corpusRemoved <- Corpus(VectorSource(wiki$Removed))
corpusRemoved <- tm_map(corpusRemoved, removeWords, stopwords('english'))
corpusRemoved <- tm_map(corpusRemoved, stemDocument)
dtmRemoved <- DocumentTermMatrix(corpusRemoved)
sparseRemoved <- removeSparseTerms(dtmRemoved, 0.997)
wordsRemoved <- as.data.frame(as.matrix(sparseRemoved))
colnames(wordsRemoved)  <-  paste("R", colnames(wordsRemoved))
ncol(wordsRemoved) #1.4 162

wikiWords <- cbind(wordsAdded, wordsRemoved)
wikiWords$Vandal <- wiki$Vandal
library(caTools)
set.seed(123)
split <- sample.split(wikiWords$Vandal, SplitRatio = 0.7)
trainWikiWords <- subset(wikiWords, split==TRUE)
testWikiWords <- subset(wikiWords, split==FALSE)
table(testWikiWords$Vandal==0)
618/(545 + 618) #1.5 0.5313844

library(rpart)
library(rpart.plot)
wikiWordsCART <- rpart(Vandal ~ ., data=trainWikiWords, method="class")
testPredict_CART <- predict(wikiWordsCART, newdata=testWikiWords, type="class")
confMat1 <- table(testWikiWords$Vandal, testPredict_CART)
sum(diag(confMat1))/nrow(testWikiWords) #1.6 0.544282

prp(wikiWordsCART) # 1.7 correct is 2, but I got 3 - something fishy

#1.8 Although it beats the baseline, bag of words is not very predictive for this problem.


#2
wikiWords2 <- wikiWords
wikiWords2$HTTP <- ifelse(grepl("http",wiki$Added,fixed=TRUE), 1, 0)
table(wikiWords2$HTTP) #2.1 217

wikiTrain2 <- subset(wikiWords2, split==TRUE)
wikiTest2 <- subset(wikiWords2, split==FALSE)
wikiWordsCART2 <- rpart(Vandal ~ ., data=wikiTrain2, method="class")
testPredict_CART2 <- predict(wikiWordsCART2, newdata=wikiTest2, type="class")
confMat2 <- table(wikiTest2$Vandal, testPredict_CART2)
confMat2
sum(diag(confMat2))/nrow(wikiTest2) # 2.2 0.5752365, but numbers in the answers not the same
                                        # maybe new version of package

wikiWords2$NumWordsAdded <- rowSums(as.matrix(dtmAdded))
wikiWords2$NumWordsRemoved <- rowSums(as.matrix(dtmRemoved))
mean(wikiWords2$NumWordsAdded) #2.3 4.050052

wikiTrain3 <- subset(wikiWords2, split==TRUE)
wikiTest3 <- subset(wikiWords2, split==FALSE)
wikiWordsCART3 <- rpart(Vandal ~ ., data=wikiTrain3, method="class")
testPredict_CART3 <- predict(wikiWordsCART3, newdata=wikiTest3, type="class")
confMat3 <- table(wikiTest3$Vandal, testPredict_CART3)
confMat3
sum(diag(confMat3))/nrow(wikiTest3) #2.4 0.6552021



# 3
wikiWords3 <- wikiWords2
wikiWords3$Minor <- wiki$Minor
wikiWords3$Loggedin <- wiki$Loggedin

wikiTrain4 <- subset(wikiWords3, split==TRUE)
wikiTest4 <- subset(wikiWords3, split==FALSE)
wikiWordsCART4 <- rpart(Vandal ~ ., data=wikiTrain4, method="class")
testPredict_CART4 <- predict(wikiWordsCART4, newdata=wikiTest4, type="class")
confMat4 <- table(wikiTest4$Vandal, testPredict_CART4)
confMat4
sum(diag(confMat4))/nrow(wikiTest4) #3.1 0.7188306

prp(wikiWordsCART4)
