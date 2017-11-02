# camera data can be downloaded her:
# https://data.baltimorecity.gov/Transportation/Baltimore-Fixed-Speed-Cameras/dz54-2aru/data
cameraData <- read.csv("./cameras.csv")
names(cameraData)
tolower(names(cameraData))

# fixing character vectors - strsplit()
splitNames <- strsplit(names(cameraData), "\\.")
splitNames[[5]]
splitNames[[6]]

# quick aside - lists
myList <- list(letters = c("A", "b", "c"), numbers = 1:3, matrix(1:25, ncol = 5))
head(myList)

# fixing character vectors - sapply()
splitNames[[6]][1]

firstElement <- function(x) {x[1]}
sapply(splitNames, firstElement)

fileUrl1 = "https://dl.dropboxusercontent.com/u/7710864/data/reviews-apr29.csv"
fileUrl2 = "https://dl.dropboxusercontent.com/u/7710864/data/solutions-apr29.csv"
download.file(fileUrl1, destfile = "reviews.csv")
download.file(fileUrl2, destfile = "solutions.csv")
reviews <- read.csv("reviews.csv"); solutions <- read.csv("solutions.csv")

# Fixing character vectors - sub()
#       Important parameters: pattern, replacement, x
names(reviews)
sub("_", "", names(reviews))

# using gsub for replacing multiple characters
testName <- "this_is_a_test"
sub("_", "", testName)
gsub("_", "", testName)

# Finding values - grep(), grepl()
grep("Alameda",cameraData$intersection) 
table(grepl("Alameda", cameraData$intersection))
cameraData2 <- cameraData[!grepl("Alameda", cameraData$intersection), ]

# more on grep()
grep("Alameda", cameraData$intersection, value = TRUE)
grep("JeffStreet", cameraData$intersection)
length(grep("JeffStreet", cameraData$intersection))

# more useful string functions
library(stringr)
nchar("Jeffrey Leek")
substr("Jeffrey Leek", 1, 7)
paste("Jeffrey", "Leek")
paste0("Jeffrey", "Leek")
str_trim("Jeff         ") # trim whitespace
