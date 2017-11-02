# Problem 1.1 - Loading the Data
# Read the dataset mvtWeek1.csv into R, using the read.csv function, and call the data frame "mvt". Remember to navigate to the directory on your computer containing the file mvtWeek1.csv first. It may take a few minutes to read in the data, since it is pretty large. Then, use the str and summary functions to answer the following questions.
# 
# How many rows of data (observations) are in this dataset?
mvt <- read.csv("mvtWeek1.csv")
nrow(mvt) # 191641

# Problem 1.2 - Loading the Data
# 
# How many variables are in this dataset?
dim(mvt) # 11

# Problem 1.3 - Loading the Data
#
# Using the "max" function, what is the maximum value of the variable "ID"?
max(mvt$ID) # 9181151

# Problem 1.4 - Loading the Data
#
# What is the minimum value of the variable "Beat"?
min(mvt$Beat) # 111

# Problem 1.5 - Loading the Data
#
# How many observations have value TRUE in the Arrest variable (this is the number of crimes for which an arrest was made)?
summary(mvt$Arrest) # 15536

# Problem 1.6 - Loading the Data
#
# How many observations have a LocationDescription value of ALLEY?
library(dplyr)
filter(mvt, LocationDescription == "ALLEY") %>% nrow() # 2308
#or
sum(mvt$LocationDescription == "ALLEY")
#or
table(mvt$LocationDescription)



# Problem 2.1 - Understanding Dates in R
#
# In many datasets, like this one, you have a date field. Unfortunately, R does not automatically recognize entries that look like dates. We need to use a function in R to extract the date and time. Take a look at the first entry of Date (remember to use square brackets when looking at a certain entry of a variable).
# 
# In what format are the entries in the variable Date?
mvt$Date[1] # Month/Day/Year Hour:Minute

# Problem 2.2 - Understanding Dates in R
# 
# Now, let's convert these characters into a Date object in R. In your R console, type
# 
DateConvert = as.Date(strptime(mvt$Date, "%m/%d/%y %H:%M"))
# 
# This converts the variable "Date" into a Date object in R. Take a look at the variable DateConvert using the summary function.
# 
# What is the month and year of the median date in our dataset? Enter your answer as "Month Year", without the quotes. (Ex: if the answer was 2008-03-28, you would give the answer "March 2008", without the quotes.)
summary(DateConvert) # 2006-05-21 - May 2006

# Problem 2.3 - Understanding Dates in R
#
# Now, let's extract the month and the day of the week, and add these variables to our data frame mvt. We can do this with two simple functions. Type the following commands in R:
# 
mvt$Month = months(DateConvert)
 
mvt$Weekday = weekdays(DateConvert)
# 
# This creates two new variables in our data frame, Month and Weekday, and sets them equal to the month and weekday values that we can extract from the Date object. Lastly, replace the old Date variable with DateConvert by typing:
# 
mvt$Date = DateConvert
# 
# Using the table command, answer the following questions.
# 
# In which month did the fewest motor vehicle thefts occur?
sort(table(mvt$Month)) # February

# Problem 2.4 - Understanding Dates in R
# 
# On which weekday did the most motor vehicle thefts occur?
sort(table(mvt$Weekday)) # Friday

# Problem 2.5 - Understanding Dates in R
# 
# Each observation in the dataset represents a motor vehicle theft, and the Arrest variable indicates whether an arrest was later made for this theft. Which month has the largest number of motor vehicle thefts for which an arrest was made?
mvt %>%
        filter(Arrest == TRUE) %>%
        group_by(Month) %>%
        summarise(arrests = n()) %>%
        arrange(desc(arrests)) # January
# or
t1 <- table(mvt$Month, mvt$Arrest)
sort(t1[, "TRUE"]) 



# Problem 3.1 - Visualizing Crime Trends
#
# Now, let's make some plots to help us better understand how crime has changed over time in Chicago. Throughout this problem, and in general, you can save your plot to a file. For more information, this website
# https://www.stat.berkeley.edu/~s133/saving.html very clearly explains the process.
# 
# First, let's make a histogram of the variable Date. We'll add an extra argument, to specify the number of bars we want in our histogram. In your R console, type
# 
hist(mvt$Date, breaks=100)
# 
# Looking at the histogram, answer the following questions.
# 
# In general, does it look like crime increases or decreases from 2002 - 2012?
                        # Decreases
#In general, does it look like crime increases or decreases from 2005 - 2008?
                        # Decreases
#In general, does it look like crime increases or decreases from 2009 - 2011?
                        # Increases

# Problem 3.2 - Visualizing Crime Trends
#
# Now, let's see how arrests have changed over time. Create a boxplot of the variable "Date", sorted by the variable "Arrest" 
# (if you are not familiar with boxplots and would like to learn more, check out this tutorial).
# In a boxplot, the bold horizontal line is the median value of the data, the box shows the range of values between the first quartile and third quartile,
# and the whiskers (the dotted lines extending outside the box) show the minimum and maximum values, excluding any outliers (which are plotted as circles).
# Outliers are defined by first computing the difference between the first and third quartile values, or the height of the box.
# This number is called the Inter-Quartile Range (IQR).
# Any point that is greater than the third quartile plus the IQR or less than the first quartile minus the IQR is considered an outlier.
# 
# Does it look like there were more crimes for which arrests were made in the first half of the time period or the second half of the time period?
# (Note that the time period is from 2001 to 2012, so the middle of the time period is the beginning of 2007.)
boxplot(mvt$Date ~ mvt$Arrest) 
        # First Half 
        # Explanation: You can create the boxplot with the command boxplot(mvt$Date ~ mvt$Arrest). 
        # If you look at the boxplot, the one for Arrest=TRUE is definitely skewed towards the bottom of the plot, 
        # meaning that there were more crimes for which arrests were made in the first half of the time period.

# Problem 3.3 - Visualizing Crime Trends
# 
# Let's investigate this further. Use the table function for the next few questions.
# 
# For what proportion of motor vehicle thefts in 2001 was an arrest made?
# 
# Note: in this question and many others in the course, we are asking for an answer as a proportion. Therefore, your answer should take a value between 0 and 1.
mvt %>% 
        filter(Year == 2001) %>% 
        group_by(Arrest) %>% 
        summarise(n = n()) %>% 
        mutate(proportion = n/sum(n)) # 0.1041173
# or
table(mvt$Arrest, mvt$Year) # 0.1041173
        # Explanation: If you create a table using the command table(mvt$Arrest, mvt$Year),
        # the column for 2001 has 2152 observations with Arrest=TRUE and 18517 observations with Arrest=FALSE.
        # The fraction of motor vehicle thefts in 2001 for which an arrest was made is thus 2152/(2152+18517) = 0.1041173.

# Problem 3.4 - Visualizing Crime Trends
# 
# For what proportion of motor vehicle thefts in 2007 was an arrest made?
mvt %>% 
        filter(Year == 2007) %>% 
        group_by(Arrest) %>% 
        summarise(n = n()) %>% 
        mutate(proportion = n/sum(n))# 0.08487395
# or
table(mvt$Arrest, mvt$Year)
1212/(1212 + 13068) # 0.08487395

# Problem 3.5 - Visualizing Crime Trends
# 
# For what proportion of motor vehicle thefts in 2012 was an arrest made?
mvt %>% 
        filter(Year == 2012) %>% 
        group_by(Arrest) %>% 
        summarise(n = n()) %>% 
        mutate(proportion = n/sum(n))# 0.03902924
# or
table(mvt$Arrest, mvt$Year)
550/(550 + 13542) # 0.03902924
# Since there may still be open investigations for recent crimes, 
# this could explain the trend we are seeing in the data. 
# There could also be other factors at play, and this trend should be investigated further.
# However, since we don't know when the arrests were actually made,
# our detective work in this area has reached a dead end.




# Problem 4.1 - Popular Locations
# 
# Analyzing this data could be useful to the Chicago Police Department when deciding where to allocate resources. If they want to increase the number of arrests that are made for motor vehicle thefts, where should they focus their efforts?
# 
# We want to find the top five locations where motor vehicle thefts occur. If you create a table of the LocationDescription variable, it is unfortunately very hard to read since there are 78 different locations in the data set. By using the sort function, we can view this same table, but sorted by the number of observations in each category. In your R console, type:
#         
sort(table(mvt$LocationDescription))
# 
# Which locations are the top five locations for motor vehicle thefts, excluding the "Other" category? You should select 5 of the following options.
        # Street, Parking Lot/Garage (Non-Residential), Alley, Gas Station, Driveway-Residential


# Problem 4.2 - Popular Locations
# 
# Create a subset of your data, only taking observations for which the theft happened in one of these five locations, and call this new data set "Top5". To do this, you can use the | symbol. In lecture, we used the & symbol to use two criteria to make a subset of the data. To only take observations that have a certain value in one variable or the other, the | character can be used in place of the & symbol. This is also called a logical "or" operation.
# 
# Alternately, you could create five different subsets, and then merge them together into one data frame using rbind.
# 
# How many observations are in Top5?
top5Locations <- c("STREET", 
          "PARKING LOT/GARAGE(NON.RESID.)",
          "ALLEY",
          "GAS STATION",
          "DRIVEWAY - RESIDENTIAL")
mvt %>%
        filter(LocationDescription %in% top5Locations) %>%
        nrow() # 177510
# or
Top5 <- subset(mvt, LocationDescription=="STREET" | LocationDescription=="PARKING LOT/GARAGE(NON.RESID.)" | LocationDescription=="ALLEY" | LocationDescription=="GAS STATION" | LocationDescription=="DRIVEWAY - RESIDENTIAL")
nrow(Top5) # 177510

# Problem 4.3 - Popular Locations
# 
# R will remember the other categories of the LocationDescription variable from the original dataset, so running table(Top5$LocationDescription) will have a lot of unnecessary output. To make our tables a bit nicer to read, we can refresh this factor variable. In your R console, type:
#         
Top5$LocationDescription = factor(Top5$LocationDescription)
# 
# If you run the str or table function on Top5 now, you should see that LocationDescription now only has 5 values, as we expect.
str(Top5) 
# Use the Top5 data frame to answer the remaining questions.
# 
# One of the locations has a much higher arrest rate than the other locations. Which is it? Please enter the text in exactly the same way as how it looks in the answer options for Problem 4.1.
Top5 %>%
        group_by(LocationDescription, Arrest) %>% 
        summarise(n = n()) %>% 
        mutate(proportion = n/sum(n)) %>%
        filter(Arrest==TRUE) %>%
        arrange(proportion) # Gas Station
# or
# If you create a table of LocationDescription compared to Arrest, 
# table(Top5$LocationDescription, Top5$Arrest), 
# you can then compute the fraction of motor vehicle thefts that resulted in arrests at each location.
# Gas Station has by far the highest percentage of arrests, with over 20% of motor vehicle thefts resulting in an arrest.

# Problem 4.4 - Popular Locations
# 
# On which day of the week do the most motor vehicle thefts at gas stations happen?
Top5 %>%
        filter(LocationDescription == "GAS STATION") %>%
        group_by(Weekday) %>%
        summarise(n = n()) %>%
        arrange(n) # Saturday
# or
table(Top5$Weekday, Top5$LocationDescription)

# Problem 4.5 - Popular Locations
# 
# On which day of the week do the fewest motor vehicle thefts in residential driveways happen?
Top5 %>%
        filter(LocationDescription == "DRIVEWAY - RESIDENTIAL") %>%
        group_by(Weekday) %>%
        summarise(n = n()) %>%
        arrange(n) # Saturday
# or
table(Top5$Weekday, Top5$LocationDescription)
