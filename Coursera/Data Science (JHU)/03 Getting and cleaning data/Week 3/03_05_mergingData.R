# peer review data
fileUrl1 = "https://dl.dropboxusercontent.com/u/7710864/data/reviews-apr29.csv"
fileUrl2 = "https://dl.dropboxusercontent.com/u/7710864/data/solutions-apr29.csv"
download.file(fileUrl1, destfile = "reviews.csv")
download.file(fileUrl2, destfile = "solutions.csv")
reviews <- read.csv("reviews.csv"); solutions <- read.csv("solutions.csv")
head(reviews, 2)
head(solutions, 2)

# Merging data - merge()
names(reviews)
names(solutions)

mergedData <- merge(reviews, solutions, by.x = "solution_id", by.y = "id", all = TRUE)
head(mergedData)

# Default - merge all common column names
intersect(names(solutions), names(reviews))

mergedData2 <- merge(reviews, solutions, all = TRUE)
head(mergedData2)

# Using join in the plyr package
library(plyr)

df1 <- data.frame(id=sample(1:10), x=rnorm(10))
df2 <- data.frame(id=sample(1:10), y=rnorm(10))
arrange(join(df1, df2), id)

# If you have multiple data frames
df1 <- data.frame(id=sample(1:10), x=rnorm(10))
df2 <- data.frame(id=sample(1:10), y=rnorm(10))
df3 <- data.frame(id=sample(1:10), z=rnorm(10))
dfList <- list(df1, df2, df3)
join_all(dfList)
