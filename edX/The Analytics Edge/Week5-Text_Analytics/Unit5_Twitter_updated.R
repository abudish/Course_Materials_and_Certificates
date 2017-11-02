# VIDEO 5: Pre-Processing in R

# Read in the data
tweets <- read.csv("tweets.csv", stringsAsFactors=FALSE)
str(tweets)

# Create dependent variable
tweets$Negative <- as.factor(tweets$Avg <= -1)
table(tweets$Negative)

# Install new packages
install.packages("tm")
library(tm)
install.packages("SnowballC")
library(SnowballC)

# Create corpus
corpus <- Corpus(VectorSource(tweets$Tweet))

# Look at corpus
corpus
corpus[[1]]$content


# Convert to lower-case
corpus <- tm_map(corpus, tolower)
corpus[[1]]$content

# THIS SECTION IS WRONG - DO NOT use it!!!!!!!!!!!!!!!
# IMPORTANT NOTE: If you are using the latest version of the tm package, 
# you will need to run the following line before continuing (it converts corpus to a Plain Text Document).
# This is a recent change having to do with the tolower function that occurred after this video was recorded.
# corpus <- tm_map(corpus, PlainTextDocument)
# corpus[[1]]$content
# THIS SECTION IS WRONG - DO NOT use it!!!!!!!!!!!!!!!

# Remove punctuation
corpus <- tm_map(corpus, removePunctuation)
corpus[[1]]$content[1]

# Look at stop words 
stopwords("english")[1:10]

# Remove stopwords and apple
corpus <- tm_map(corpus, removeWords, c("apple", stopwords("english")))
corpus[[1]]$content[1]

# Stem document
corpus <- tm_map(corpus, stemDocument)
corpus[[1]]$content[1]


# VIDEO 6: Bag of Words in R

# Create matrix
frequencies <- DocumentTermMatrix(corpus)
frequencies

# Look at matrix 
inspect(frequencies[1000:1005,505:515])

# Check for sparsity
findFreqTerms(frequencies, lowfreq=20)

# Remove sparse terms - that don't appear very often
sparse <- removeSparseTerms(frequencies, 0.995)
sparse

# Convert to a data frame
tweetsSparse <- as.data.frame(as.matrix(sparse))

# Make all variable names R-friendly
colnames(tweetsSparse) <- make.names(colnames(tweetsSparse))

# Add dependent variable
tweetsSparse$Negative <- tweets$Negative

# Split the data
library(caTools)
set.seed(123)
split <- sample.split(tweetsSparse$Negative, SplitRatio = 0.7)
trainSparse <- subset(tweetsSparse, split==TRUE)
testSparse <- subset(tweetsSparse, split==FALSE)


# Video 7: Predicting Sentiment

# Build a CART model
library(rpart)
library(rpart.plot)

tweetCART <- rpart(Negative ~ ., data=trainSparse, method="class")
prp(tweetCART)

# Evaluate the performance of the model
predictCART <- predict(tweetCART, newdata=testSparse, type="class")

table(testSparse$Negative, predictCART)

# Compute accuracy
(294+18)/(294+6+37+18) # 0.8788732

# Baseline accuracy 
table(testSparse$Negative)
300/(300+55) # 0.8450704


# Random forest model
library(randomForest)
set.seed(123)
tweetRF <- randomForest(Negative ~ ., data=trainSparse)

# Make predictions:
predictRF <- predict(tweetRF, newdata=testSparse)
table(testSparse$Negative, predictRF)

# Accuracy:
(293+21)/(293+7+34+21) # 0.884507

# Logistic Regression model
tweetLog <- glm(Negative ~ ., data=trainSparse, family='binomial')
predictLog <- predict(tweetLog, newdata=testSparse, type="response")
table(testSparse$Negative, predictLog >= 0.5)
(251 + 36)/(251 + 49 + 36+19)
# The accuracy is (254+37)/(254+46+18+37) = 0.8197183, which is worse than the baseline.
# If you were to compute the accuracy on the training set instead, you would see that
# the model does really well on the training set - this is an example of over-fitting.
# The model fits the training set really well, but does not perform well on the test set.
# A logistic regression model with a large number of variables is particularly at risk for overfitting.
