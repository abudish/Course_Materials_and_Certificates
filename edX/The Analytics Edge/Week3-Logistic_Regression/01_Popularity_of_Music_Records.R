library(dplyr)
songs <- read.csv('songs.csv')
songs %>% filter(year == 2010) %>% nrow # 1.1 - 373

songs %>% filter(artistname == 'Michael Jackson') %>% nrow # 1.2 - 18

songs %>% 
        filter(artistname == 'Michael Jackson', Top10 == 1) %>% 
        select(songtitle) # 1.3 You Rock My World and You Are Not Alone
        
table(songs$timesignature) # 1.4  3 4 5 7 1 0 and 4

songs %>%
        filter(tempo == max(tempo)) %>%
        select(songtitle) # 1.5 Wanna Be Startin' Somethin'

        
        

SongsTrain <- subset(songs, year <= 2009)
SongsTest <- subset(songs, year == 2010)
nrow(SongsTrain) # 2.1 - 7201


nonvars <- c("year", "songtitle", "artistname", "songID", "artistID")
SongsTrain <- SongsTrain[ , !(names(SongsTrain) %in% nonvars) ]
SongsTest <- SongsTest[ , !(names(SongsTest) %in% nonvars) ]
SongsLog1 <- glm(Top10 ~ ., data=SongsTrain, family='binomial')
summary(SongsLog1) # 2.2 AIC: 4827.2

# 2.3 - The higher our confidence about time signature, key and tempo, the more likely the song is to be in the Top 10
# 2.4 - Mainstream listeners tend to prefer less complex songs 
# 2.5 - Mainstream listeners prefer songs with heavy instrumentation
        # and No

cor(SongsTrain$loudness, SongsTrain$energy) # 3.1 -  0.7399067

SongsLog2 <- glm(Top10 ~ .-loudness, data=SongsTrain, family='binomial')
summary(SongsLog2) # 3.2 - Model 2 suggests that songs with high energy levels tend to be more popular. This contradicts our observation in Model 1. 

SongsLog3 <- glm(Top10 ~ .-energy, data=SongsTrain, family='binomial')
summary(SongsLog3) # 3.3 - Yes

TestPrediction <- predict(SongsLog3, newdata=SongsTest, type='response')
confMat <- table(SongsTest$Top10, TestPrediction >= 0.45)
sum(diag(confMat))/sum(confMat) # 3.4 - 0.8793566


TestPrediction2 <- predict(SongsLog1, newdata=SongsTest, type='response')
confMat2 <- table(SongsTest$Top10, TestPrediction2 == 0)
sum(diag(confMat2))/sum(confMat2) # 3.5 - 0.8418231 
# or
table(SongsTest$Top10)

confMat # 4.3 - 19 and 5

library(caret)
confMat
19/(19+40) 
309/(309 + 5) # 4.4 0.3220339 and 0.9840764

#4.5 - Model 3 favors specificity over sensitivity. and
# Model 3 provides conservative predictions, and predicts that a song will make it
# to the Top 10 very rarely. So while it detects less than half of the Top 10 songs,
# we can be very confident in the songs that it does predict to be Top 10 hits.






