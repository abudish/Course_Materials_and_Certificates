# 1 #
# !!!! Run next line - really important!!!
Sys.setlocale("LC_ALL", "C")

tweets <- read.csv('tweets.csv', stringsAsFactors=FALSE)

install.packages("tm")
library(tm)
install.packages("SnowballC")
library(SnowballC)

corpus <- Corpus(VectorSource(tweets$Tweet))
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
dtm <- DocumentTermMatrix(corpus)
allTweets <- as.data.frame(as.matrix(dtm))
ncol(allTweets) #1.1 3780

#1.2 It will be easier to read and understand the word cloud if it includes full
# words instead of just the word stems


# 2 #
install.packages('wordcloud')
library(wordcloud)
head(colnames(allTweets)) #2.1 colnames
head(colSums(allTweets)) #2.2 colSums

?wordcloud
wordcloud(colnames(allTweets), freq=colSums(allTweets), scale=c(2, 0.25)) #2.3 apple


corpus <- Corpus(VectorSource(tweets$Tweet))
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeWords, c("apple", stopwords("english")))
dtm <- DocumentTermMatrix(corpus)
allTweets <- as.data.frame(as.matrix(dtm))
wordcloud(colnames(allTweets), freq=colSums(allTweets), scale=c(2, 0.25)) #2.4 iphone


# 3 #
negativeTweets <- subset(allTweets, tweets$Avg <= -1)
wordcloud(colnames(negativeTweets), colSums(negativeTweets), max.words=100) #3.1 Plot C 

#3.2 WordCloud A

#3.3 WordCloud B and D

#3.4 WordCloud A

#3.5 WordCloud D


# 4 #
library(RColorBrewer)
brewer.pal()
display.brewer.all() #4.1 YlOrRd
                     #4.2 Greys


wordcloud(colnames(negativeTweets), colSums(negativeTweets),
          max.words=100, colors=brewer.pal(9, "Blues"))
#4.3 brewer.pal(9, "Blues")[c(-1, -2, -3, -4)] and brewer.pal(9, "Blues")[c(5, 6, 7, 8, 9)]