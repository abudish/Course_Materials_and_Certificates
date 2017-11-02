###
# Question 1
###

#The American Community Survey distributes downloadable data about United States communities.
#Download the 2006 microdata survey about housing for the state of Idaho using download.file() from here:

fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
download.file(fileUrl, "./housing_survey.csv")

# and load the data into R. The code book, describing the variable names is here:
# https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FPUMSDataDict06.pdf

housing_survey <- read.csv("housing_survey.csv")

# Create a logical vector that identifies the households on greater than 10 acres
# who sold more than $10,000 worth of agriculture products.
#Assign that logical vector to the variable agricultureLogical.

# From codebook:
#       ACR - Lot size; 3 - House on 10 or more acres
#       AGS - sales of agriculture products; 6 - $10, 000 or more
agricultureLogical <- housing_survey$ACR == 3 & housing_survey$AGS == 6


# Apply the which() function like this to identify the rows of the data frame
# where the logical vector is TRUE.
# What are the first 3 values that result?
head(which(agricultureLogical)) # 125 238 262


### 
# Question 2
###

#Using the jpeg package read in the following picture of your instructor into R
install.packages("jpeg")
library(jpeg)
#https://d396qusza40orc.cloudfront.net/getdata%2Fjeff.jpg
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fjeff.jpg"
download.file(fileUrl, "./instructor_picture.jpg", mode = 'wb') # mode = 'wb' reads binary file,
                                                        #or else file will be read incorrectly

# Use the parameter native=TRUE. What are the 30th and 80th quantiles of the resulting data?
# (some Linux systems may produce an answer 638 different for the 30th quantile)
instructor <- readJPEG("./instructor_picture.jpg", native = TRUE)

quantile(instructor, probs = c(0.3, 0.8)) # -15259150 -10575416

###
# Question 3 - Wrong - 234 matches?
###
library(plyr)
# Load the Gross Domestic Product data for the 190 ranked countries in this data set:
#        https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv
fileUrl1 <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
download.file(fileUrl1, "./GDP.csv")
gdp <- read.csv("GDP.csv", 
                skip = 4, 
                nrows = 190, #why 190 -> after 190 there is no ranking
                colClasses = c("character", "numeric", "NULL", "character", "character",
                               rep("NULL", 5)) # NULL skips the columns
                )
# Renaming left columns
colnames(gdp) <- c("CountryCode", "Rank", "CountryName", "Total")

# Load the educational data from this data set:
#        https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv
fileUrl2 <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv"
download.file(fileUrl2, "./education.csv")
education <- read.csv("education.csv")

# Match the data based on the country shortcode. How many of the IDs match? 
head(gdp)
head(education)
names(gdp)
names(education)
mergedData <- merge(gdp, education,  by = "CountryCode")

# How many of the IDs match?
nrow(mergedData) # 189 matches

# Sort the data frame in descending order by GDP rank (so United States is last). 
sorted <- arrange(mergedData, desc(Rank))

# What is the 13th country in the resulting data frame?
sorted[13, "CountryName"] # 13th country is St. Kitts and Nevis

###
# Question 4
###

#What is the average GDP ranking for the "High income: OECD" and "High income: nonOECD" group?
library(dplyr)

mergedData %>%
        filter(Income.Group == "High income: OECD") %>%
        summarize(Rank = mean(Rank, na.rm = TRUE)) # 32.96667

mergedData %>%
        filter(Income.Group == "High income: nonOECD") %>%
        summarize(Rank = mean(Rank, na.rm = TRUE)) # 91.91304
# or even more beautiful
tapply(mergedData$Rank, mergedData$Income.Group, mean)

### 
#Question 5.Answer - 5 
# Cut the GDP ranking into 5 separate quantile groups. 
# Make a table versus Income.Group. 

library(Hmisc)
mergedData$RankGroups <- cut2(mergedData$Rank, g = 5)

# How many countries are Lower middle income but among the 38 nations with highest GDP?
a <- table(mergedData$RankGroups, mergedData$Income.Group)
a[1, "Lower middle income"]
