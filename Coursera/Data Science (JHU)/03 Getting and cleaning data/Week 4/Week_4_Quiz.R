###
# Question 1
###

# The American Community Survey distributes downloadable data about United States communities.
# Download the 2006 microdata survey about housing for the state of Idaho using download.file() from here:
# https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv
fileUrl1 <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
download.file(fileUrl1, "./housing_survey.csv")
# and load the data into R. The code book, describing the variable names is here:
# https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FPUMSDataDict06.pdf
housing <- read.csv("housing_survey.csv")

# Apply strsplit() to split all the names of the data frame on the characters "wgtp". 
splited <- strsplit(names(housing), "wgtp")
#What is the value of the 123 element of the resulting list?
splited[123] # "" "15"


###
# Question 2
###

# Load the Gross Domestic Product data for the 190 ranked countries in this data set:
# https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv
# Original data sources:
# http://data.worldbank.org/data-catalog/GDP-ranking-table
fileUrl2 <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
download.file(fileUrl2, "./gdp.csv")
gdp <- read.csv("GDP.csv", 
                skip = 4, 
                nrows = 190, #why 190 -> after 190 there is no ranking
                colClasses = c("character", "numeric", "NULL", "character", "character",
                               rep("NULL", 5)) # NULL skips the columns
)
# Renaming left columns
colnames(gdp) <- c("CountryCode", "Rank", "CountryName", "Total")

# Remove the commas from the GDP numbers in millions of dollars and average them. What is the average?
library(dplyr)
gsub(",", "", gdp$Total) %>%
        as.numeric %>%
        mean # 377652.4


###
# Question 3
###

# In the data set from Question 2 what is a regular expression that would allow you
# to count the number of countries whose name begins with "United"? 
#Assume that the variable with the country names in it is named countryNames. 
countryNames <- gdp$CountryName
grep("^United",countryNames) %>%
        length # How many countries begin with United? - 3


###
# Question 4
###

# Original data sources:
#         http://data.worldbank.org/data-catalog/GDP-ranking-table
# http://data.worldbank.org/data-catalog/ed-stats

#Load the Gross Domestic Product data for the 190 ranked countries in this data set:
        # https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv
fileUrl2 <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
download.file(fileUrl2, "./gdp.csv")
gdp <- read.csv("GDP.csv", 
                skip = 4, 
                nrows = 190, #why 190 -> after 190 there is no ranking
                colClasses = c("character", "numeric", "NULL", "character", "character",
                               rep("NULL", 5)) # NULL skips the columns
)
# Renaming left columns
colnames(gdp) <- c("CountryCode", "Rank", "CountryName", "Total")

# Load the educational data from this data set:
        # https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv
fileUrl3 <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv"
download.file(fileUrl3, "./education.csv")
education <- read.csv("education.csv")

# Match the data based on the country shortcode. 
mergedData <- merge(gdp, education,  by = "CountryCode")

# Of the countries for which the end of the fiscal year is available, 
# how many end in June? - info is in the Special.Notes column
mergedColumns <- colnames(mergedData)
grep("^Fiscal year end: June 30",mergedData$Special.Notes) %>%
        length # Answer is 13

###
# Question 5
###

# You can use the quantmod (http://www.quantmod.com/) package to get historical stock prices
# for publicly traded companies on the NASDAQ and NYSE. 
# Use the following code to download data on Amazon's stock price
# and get the times the data was sampled:
install.packages("quantmod")
library(quantmod)
amzn <- getSymbols("AMZN",auto.assign=FALSE)
sampleTimes <- index(amzn)

# How many values were collected in 2012? 
grep("^2012", sampleTimes) %>%
        length # 250
values2012 <- grep("^2012", sampleTimes, value = TRUE)

# How many values were collected on Mondays in 2012?
library(lubridate)
sapply(values2012, 
       function(x) {wday(x, label = TRUE) == "Mon"}) %>%
        sum
