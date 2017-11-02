# Reading Test Scores
# 
# The Programme for International Student Assessment (PISA) is a test given every three years 
# to 15-year-old students from around the world to evaluate their performance in mathematics,
# reading, and science. This test provides a quantitative way to compare the performance of students
# from different parts of the world. In this homework assignment, we will predict the reading scores
# of students from the United States of America on the 2009 PISA exam.
# 
# The datasets pisa2009train.csv and pisa2009test.csv contain information about the demographics and
# schools for American students taking the exam, derived from 2009 PISA Public-Use Data Files 
# distributed by the United States National Center for Education Statistics (NCES).
# While the datasets are not supposed to contain identifying information about students taking the test,
# by using the data you are bound by the NCES data use agreement, which prohibits any attempt to
# determine the identity of any student in the datasets.
# 
# Each row in the datasets pisa2009train.csv and pisa2009test.csv represents one student taking the exam.
# The datasets have the following variables:
        # grade: The grade in school of the student (most 15-year-olds in America are in 10th grade)
        # 
        # male: Whether the student is male (1/0)
        # 
        # raceeth: The race/ethnicity composite of the student
        # 
        # preschool: Whether the student attended preschool (1/0)
        # 
        # expectBachelors: Whether the student expects to obtain a bachelor's degree (1/0)
        # 
        # motherHS: Whether the student's mother completed high school (1/0)
        # 
        # motherBachelors: Whether the student's mother obtained a bachelor's degree (1/0)
        # 
        # motherWork: Whether the student's mother has part-time or full-time work (1/0)
        # 
        # fatherHS: Whether the student's father completed high school (1/0)
        # 
        # fatherBachelors: Whether the student's father obtained a bachelor's degree (1/0)
        # 
        # fatherWork: Whether the student's father has part-time or full-time work (1/0)
        # 
        # selfBornUS: Whether the student was born in the United States of America (1/0)
        # 
        # motherBornUS: Whether the student's mother was born in the United States of America (1/0)
        # 
        # fatherBornUS: Whether the student's father was born in the United States of America (1/0)
        # 
        # englishAtHome: Whether the student speaks English at home (1/0)
        # 
        # computerForSchoolwork: Whether the student has access to a computer for schoolwork (1/0)
        # 
        # read30MinsADay: Whether the student reads for pleasure for 30 minutes/day (1/0)
        # 
        # minutesPerWeekEnglish: The number of minutes per week the student spend in English class
        # 
        # studentsInEnglish: The number of students in this student's English class at school
        # 
        # schoolHasLibrary: Whether this student's school has a library (1/0)
        # 
        # publicSchool: Whether this student attends a public school (1/0)
        # 
        # urban: Whether this student's school is in an urban area (1/0)
        # 
        # schoolSize: The number of students in this student's school
        # 
        # readingScore: The student's reading score, on a 1000-point scale

# Problem 1.1 - Dataset size
#
# Load the training and testing sets using the read.csv() function, and save them as variables with the names pisaTrain and pisaTest.
pisaTrain <- read.csv('pisa2009train.csv')
pisaTest <- read.csv('pisa2009test.csv ')
# How many students are there in the training set?
str(pisaTrain)
        #A: 3663

# Problem 1.2 - Summarizing the dataset
# 
# Using tapply() on pisaTrain, what is the average reading test score of males?
tapply(pisaTrain$readingScore, pisaTrain$male, mean)
        # A: 483.5325
# 
# Of females?
        # A: 512.9406

# Problem 1.3 - Locating missing values
#
# Which variables are missing data in at least one observation in the training set?
# Select all that apply.
summary(pisaTrain)
      # A:raceeth
        # preschool
        # expectBachelors
        # motherHS
        # motherBachelors
        # motherWork
        # fatherHS
        # fatherBachelors
        # fatherWork
        # selfBornUS
        # motherBornUS
        # fatherBornUS
        # englishAtHome
        # computerForSchoolwork
        # read30MinsADay
        # minutesPerWeekEnglish
        # studentsInEnglish
        # schoolHasLibrary
        # schoolSize

# Problem 1.4 - Removing missing values
#
# Linear regression discards observations with missing data, so we will remove all such observations
# from the training and testing sets. Later in the course, we will learn about imputation,
# which deals with missing data by filling in missing values with plausible information.
# 
# Type the following commands into your R console to remove observations with any missing value
# from pisaTrain and pisaTest:
pisaTrain <-  na.omit(pisaTrain)
pisaTest <-  na.omit(pisaTest)
# How many observations are now in the training set?
nrow(pisaTrain)
        #A: 2414
#
# How many observations are now in the testing set?
nrow(pisaTest)
        # A: 990


# Problem 2.1 - Factor variables
# 
# Factor variables are variables that take on a discrete set of values, like the "Region" variable
# in the WHO dataset from the second lecture of Unit 1. This is an unordered factor because
# there isn't any natural ordering between the levels. An ordered factor has a natural ordering
# between the levels (an example would be the classifications "large," "medium," and "small").
# 
# Which of the following variables is an unordered factor with at least 3 levels?
# (Select all that apply.)
unique(pisaTrain$grade)
unique(pisaTrain$male)
unique(pisaTrain$raceeth)
        # A: raceeth
# Which of the following variables is an ordered factor with at least 3 levels? 
# (Select all that apply.)
        # A: grade

# Problem 2.2 - Unordered factors in regression models
#
# To include unordered factors in a linear regression model, we define one level as the 
# "reference level" and add a binary variable for each of the remaining levels.
# In this way, a factor with n levels is replaced by n-1 binary variables.
# The reference level is typically selected to be the most frequently occurring level in the dataset.
# 
# As an example, consider the unordered factor variable "color", with levels "red", "green",
# and "blue". If "green" were the reference level, then we would add binary variables 
# "colorred" and "colorblue" to a linear regression problem. All red examples would have
# colorred=1 and colorblue=0. All blue examples would have colorred=0 and colorblue=1.
# All green examples would have colorred=0 and colorblue=0.
# 
# Now, consider the variable "raceeth" in our problem, which has levels 
#"American Indian/Alaska Native", "Asian", "Black", "Hispanic", "More than one race", 
#"Native Hawaiian/Other Pacific Islander", and "White". Because it is the most common 
# in our population, we will select White as the reference level.
# 
# Which binary variables will be included in the regression model? (Select all that apply.)
      #A: raceethAmerican Indian/Alaska Native
        # raceethAsian
        # raceethBlack
        # raceethHispanic
        # raceethMore than one race
        # raceethNative Hawaiian/Other Pacific Islander

# Problem 2.3 - Example unordered factors
#
# Consider again adding our unordered factor race to the regression model with 
# reference level "White".
# 
# For a student who is Asian, which binary variables would be set to 0? 
# All remaining variables will be set to 1. (Select all that apply.)
     # A: raceethAmerican Indian/Alaska Native
        # raceethBlack
        # raceethHispanic
        # raceethMore than one race
        # raceethNative Hawaiian/Other Pacific Islander

# For a student who is white, which binary variables would be set to 0? 
# All remaining variables will be set to 1. (Select all that apply.)
     # A: raceethAmerican Indian/Alaska Native
        # raceethAsian
        # raceethBlack
        # raceethHispanic
        # raceethMore than one race
        # raceethNative Hawaiian/Other Pacific Islander



# Problem 3.1 - Building a model
#
# Because the race variable takes on text values, it was loaded as a factor variable when
# we read in the dataset with read.csv() -- you can see this when you run str(pisaTrain)
# or str(pisaTest). However, by default R selects the first level alphabetically 
# ("American Indian/Alaska Native") as the reference level of our factor instead of the most
# common level ("White"). Set the reference level of the factor by typing the following 
# two lines in your R console:
pisaTrain$raceeth = relevel(pisaTrain$raceeth, "White")
pisaTest$raceeth = relevel(pisaTest$raceeth, "White")

# Now, build a linear regression model (call it lmScore) using the training set to predict
# readingScore using all the remaining variables.
# 
# It would be time-consuming to type all the variables, but R provides the shorthand notation
# "readingScore ~ ." to mean "predict readingScore using all the other variables in the data frame."
# The period is used to replace listing out all of the independent variables.
# As an example, if your dependent variable is called "Y", your independent variables
# are called "X1", "X2", and "X3", and your training data set is called "Train",
# instead of the regular notation:
#         
#         LinReg = lm(Y ~ X1 + X2 + X3, data = Train)
# 
# You would use the following command to build your model:
#         
#         LinReg = lm(Y ~ ., data = Train)

# What is the Multiple R-squared value of lmScore on the training set?
lmScore <- lm(readingScore ~ ., data=pisaTrain)
summary(lmScore)
        # A: 0.3251

# Problem 3.2 - Computing the root-mean squared error of the model
# 
# What is the training-set root-mean squared error (RMSE) of lmScore?
rScorePredictions <- predict(lmScore, pisaTrain)

SSE <- sum((rScorePredictions - pisaTrain$readingScore)^2)
N <- nrow(pisaTrain)
RMSE <- sqrt(SSE / N) 
RMSE
        # A: 73.36555

# Problem 3.3 - Comparing predictions for similar students
# 
# Consider two students A and B. They have all variable values the same, except that 
# student A is in grade 11 and student B is in grade 9. 
# What is the predicted reading score of student A 
# minus the predicted reading score of student B?
s <- summary(lmScore)
s$coefficients
        # A: 59.09
        # Explanation
        # The coefficient 29.54 on grade is the difference in reading score between 
        # two students who are identical other than having a difference in grade of 1.
        # Because A and B have a difference in grade of 2, the model predicts 
        # that student A has a reading score that is 2*29.54 larger.

# Problem 3.4 - Interpreting model coefficients
# 
# What is the meaning of the coefficient associated with variable raceethAsian?
        # A: Predicted difference in the reading score between an Asian student and
        #    a white student who is otherwise identical

# Problem 3.5 - Identifying variables lacking statistical significance
#
# Based on the significance codes, which variables are candidates for 
# removal from the model? 
# Select all that apply. 
# (We'll assume that the factor variable raceeth should only be removed if none
# of its levels are significant.)
summary(lmScore)
        # A:      preschool
                # motherHS
                # motherWork
                # fatherHS
                # fatherWork
                # selfBornUS
                # motherBornUS
                # fatherBornUS
                # englishAtHome
                # minutesPerWeekEnglish
                # studentsInEnglish
                # schoolHasLibrary
                # urban


# Problem 4.1 - Predicting on unseen data
# 
# Using the "predict" function and supplying the "newdata" argument, use the 
# lmScore model to predict the reading scores of students in pisaTest.
# Call this vector of predictions "predTest". Do not change the variables in the model
# (for example, do not remove variables that we found were not significant in the previous part
# of this problem). Use the summary function to describe the test set predictions.
predTest <- predict(lmScore, newdata=pisaTest)
summary(predTest)
# What is the range between the maximum and minimum predicted reading score on the test set?
max(predTest) - min(predTest)
        # A: 284.4683

# Problem 4.2 - Test set SSE and RMSE
#
# What is the sum of squared errors (SSE) of lmScore on the testing set?
SSE <- sum((predTest - pisaTest$readingScore)^2)
SSE
        # A: 5762082
# What is the root-mean squared error (RMSE) of lmScore on the testing set?
N <- nrow(pisaTest)
RMSE <- sqrt(SSE / N) 
RMSE
        # A: 76.29079

# Problem 4.3 - Baseline prediction and test-set SSE
# 
# What is the predicted test score used in the baseline model? 
# Remember to compute this value using the training set and not the test set.
mean(pisaTrain$readingScore)
        # A: 517.9629
# What is the sum of squared errors of the baseline model on the testing set?
# HINT: We call the sum of squared errors for the baseline model the total sum of squares (SST).
SST <- sum((mean(pisaTrain$readingScore) - pisaTest$readingScore)^2)
SST
        # A: 7802354

# Problem 4.4 - Test-set R-squared
#
# What is the test-set R-squared value of lmScore?
R_squared <- 1 - SSE/SST
R_squared
