# 1.How many properties are worth $1,000,000 or more?
#The code book, describing the variable names is here:
#https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FPUMSDataDict06.pdf

fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
download.file(fileURL, destfile = "./housingIdaho.csv")
list.files("./")

dateDownloaded <- date()
dateDownloaded

housing <- read.csv("housingIdaho.csv")
property_values <- housing[['VAL']]
good <- !is.na(property_values)
property_values_good <- property_values[good]
# 1.How many properties are worth $1,000,000 or more? 24 - stands for 1, 000, 000 or more
length(which(property_values_good == 24))

# 2. Use the data you loaded from Question 1. Consider the variable FES in the code book.
#Which of the "tidy data" principles does this variable violate? 
    #- Tidy data has one variable per column

# 3. Download the Excel spreadsheet on Natural Gas Aquisition Program here:
# https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FDAT.gov_NGAP.xlsx
fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FDATA.gov_NGAP.xlsx?accessType=DOWNLOAD"
download.file(fileURL, destfile = "./ngap.xlsx", mode = 'wb') # mode='wb' solves read.xlsx 
#ava.util.zip.ZipException: invalid code -- missing end-of-block Java Issue
dateDownloaded <- date()
dateDownloaded

install.packages("xlsx")
library(xlsx)

# Read rows 18-23 and columns 7-15 into R and assign the result to a variable called: dat
rowIndex <- 18:23
colIndex <- 7:15
dat <- read.xlsx(file = "./ngap.xlsx", sheetIndex = 1,
                 colIndex = colIndex, rowIndex = rowIndex)
# What is the value of:
sum(dat$Zip*dat$Ext,na.rm=T)

# 4. Read the XML data on Baltimore restaurants from here:
# https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Frestaurants.xml
install.packages('XML')
#install.packages('RCurl')
library(XML)
#library(RCurl)
#xmlTreeParse does not support https, that's why use http instead of https
fileURL <- "http://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Frestaurants.xml"
doc <- xmlTreeParse(fileURL, useInternal = TRUE)
rootNode <- xmlRoot(doc)
xmlName(rootNode)
names(rootNode)
rootNode[[1]][[1]]

# How many restaurants have zipcode 21231?
zipcodes <- xpathSApply(rootNode, "//zipcode", xmlValue)
length(which(zipcodes == "21231"))
# alternatively:
zip_table <- table(zipcodes)
zip_table[names(zip_table)=="21231"]

# 5. The American Community Survey distributes downloadable data about United States communities.
#Download the 2006 microdata survey about housing for the state of Idaho using download.file() from here:
#https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06pid.csv
fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06pid.csv"
download.file(fileURL, destfile = "./housingIdahoSurvey.csv")

# to use fread() data.table package should be installed
install.packages("data.table")
library(data.table)
DT <- fread("housingIdahoSurvey.csv")
# The following are ways to calculate the average value of the variable pwgtp15
# broken down by sex. Using the data.table package, which will deliver the fastest user time?
library(microbenchmark)
x <- runif(100)
microbenchmark(
  
  
  #rowMeans(DT)[DT$SEX==1], - Error in rowMeans(DT) : 'x' must be numeric
  #rowMeans(DT)[DT$SEX== 2],
  
  mean(DT[DT$SEX==1,]$pwgtp15),
  mean(DT[DT$SEX==2,]$pwgtp15),
  
  DT[,mean(pwgtp15), by=SEX],
  sapply(split(DT$pwgtp15,DT$SEX),mean),
  tapply(DT$pwgtp15,DT$SEX,mean),
  mean(DT$pwgtp15,by=DT$SEX)
)

# Answers: 1 - 53; 2 - Tidy data has one variable per column ; 3 - 36534720; 4- 127; 5 - ?
