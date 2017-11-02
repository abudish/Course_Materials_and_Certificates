######
#Question 1
######
#Register an application with the Github API here https://github.com/settings/applications.
#Access the API to get information on your instructors repositories
#(hint: this is the url you want "https://api.github.com/users/jtleek/repos").
#Use this data to find the time that the datasharing repo was created. 
#What time was it created?

#This tutorial may be useful (https://github.com/hadley/httr/blob/master/demo/oauth2-github.r). 
#You may also need to run the code in the base R package and not R studio.
library(httr)
library(jsonlite)

# 1. Find OAuth settings for github:
#    http://developer.github.com/v3/oauth/
oauth_endpoints("github")

# 2. To make your own application, register at at
#    https://github.com/settings/applications. Use any URL for the homepage URL
#    (http://github.com is fine) and  http://localhost:1410 as the callback url
#
#    Replace your key and secret below. - Might not work. Simply reset key in 
myapp <- oauth_app("github",
                   key = "0f3fff33a234d1800269",
                   secret = "48e42ed6b793a0d5c596c8163f85c71b748baa60")

# 3. Get OAuth credentials
github_token <- oauth2.0_token(oauth_endpoints("github"), myapp)

# 4. Use API
gtoken <- config(token = github_token)
req <- GET("https://api.github.com/users/jtleek/repos", gtoken)
stop_for_status(req)
content <- content(req)
myjson <- jsonlite::fromJSON(toJSON(content))

#Use this data to find the time that the datasharing repo was created. 
#What time was it created?
datasharing <- myjson[myjson$name == "datasharing", ]
created_at <- datasharing$created_at
created_at

######
# QUestion 2
######
#The sqldf package allows for execution of SQL commands on R data frames.
#We will use the sqldf package to practice the queries
#we might send with the dbSendQuery command in RMySQL.

#Download the American Community Survey data
download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06pid.csv", "./acs.csv")
acs <- read.csv("acs.csv")

install.packages("sqldf")
library(sqldf)
#Which of the following commands will select only the data
#for the probability weights pwgtp1 with ages less than 50?
a <- sqldf("select * from acs")
b <- sqldf("select * from acs where AGEP < 50 and pwgtp1")
c <- sqldf("select pwgtp1 from acs")
d <- sqldf("select pwgtp1 from acs where AGEP < 50") # correct answer

######
#Question 3
######
#Using the same data frame you created in the previous problem,
#what is the equivalent function to unique(acs$AGEP)
u0 <- unique(acs$AGEP)
u1 <- sqldf("select distinct pwgtp1 from acs")
u2 <- sqldf("select distinct AGEP from acs") # correct answer
u3 <- sqldf("select unique * from acs") # syntax error
u4 <- sqldf("select AGEP where unique from acs") # syntax error

######
#Question 4
######
#How many characters are in the 10th, 20th, 30th and 100th lines of HTML from this page:
        
#        http://biostat.jhsph.edu/~jleek/contact.html

#(Hint: the nchar() function in R may be helpful)
con = url("http://biostat.jhsph.edu/~jleek/contact.html")
htmlCode = readLines(con)
close(con)
nchar(htmlCode[10])
nchar(htmlCode[20])
nchar(htmlCode[30])
nchar(htmlCode[100])



######
#Question 5
######
#Read this data set into R and report the sum of the numbers in the fourth of the nine columns.

# https://d396qusza40orc.cloudfront.net/getdata%2Fwksst8110.for

#Original source of the data: http://www.cpc.ncep.noaa.gov/data/indices/wksst8110.for
#(Hint this is a fixed width file format)
download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fwksst8110.for", "./fixedwidthfile.for")
ff <- read.fwf("fixedwidthfile.for",
               skip = 4,
               widths = c(-1,9,-5,4,4,-5,4,4,-5,4,4,-5,4,4))
sum(ff$V4)
