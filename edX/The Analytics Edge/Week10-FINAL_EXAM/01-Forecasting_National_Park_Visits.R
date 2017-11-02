visits <- read.csv('park_visits.csv')

# 1 #
library(dplyr)
visits2016jul <- visits %>% 
        filter(Year==2016, Month==7)
#Which park type has the most number of parks?
visits2016jul %>% 
        select(ParkType, ParkName) %>% 
        group_by(ParkType) %>% 
        summarise(n = n())
which.max(summary(visits2016jul$ParkType)) #1.1 National Historic Site

#Which specific park has the most number of visitors?
visits2016jul %>% 
        filter(ParkName !('Other')) %>% nrow
max(visits2016jul$logVisits)
visits2016jul %>% 
        select(ParkName, logVisits) %>% 
        arrange(logVisits) #1.2 Great Smoky Mountains NP



# 2 #
#2.1 Which region has the highest average log visits in July 2016? - Pacific West
visits2016jul %>% 
        select(Region, logVisits) %>% 
        group_by(Region) %>% 
        summarise(avg = mean(logVisits)) %>% 
        arrange(avg)
#2.2 What is the average log visits for the region in July 2016 with:
        #1. the highest average log visits? - 10.767849
        #2. the lowest average log visits? - 9.374157
# alternatively
sort(tapply(visits2016jul$logVisits, visits2016jul$Region, FUN=mean))



# 3 #
# What is the correlation between entrance fee (the variable cost) and the log visits in July 2016?
cor(visits2016jul$cost, visits2016jul$logVisits)



# 4 #
# Let's now look at the time dimension of the data. 
# Subset the original data (visits) to "Yellowstone NP" only and save as ys. 
ys <- visits %>% 
        filter(ParkName=='Yellowstone NP')
ys_ts=ts(ys$logVisits,start=c(2010,1),freq=12)
plot(ys_ts)
# What observations do you make?
        #4.1 Between the years, the shapes are largely similar;
        #    the log visits are highly cyclical, with the peaks in the summer time



# 5 #
summary(visits)
colSums(is.na(visits))
#5.1 Why do we have NA's in the laglogVisits and laglogVisitsYear? 
#    These variables were created by lagging the log visits by a month or by a year.
        # These are lagged variables and the earlier data is not available for the first months.
visits = visits[rowSums(is.na(visits)) == 0, ]
#5.2 How many observations are there in visits now?
str(visits) # 21855


# 6 #
visits$Month <- as.factor(visits$Month)
train <- subset(visits, Year %in% c(2010, 2011, 2012, 2013, 2014))
test <- subset(visits, Year %in% c(2015, 2016))

mod <- lm(logVisits ~ laglogVisits, data=train)
summary(mod)
predTest <- predict(mod, test)
SSE <- sum((predTest - test$logVisits)^2)
SSE
SST <- sum((mean(train$logVisits) - test$logVisits)^2)
SST
#6.1 What's the out-of-sample R^2 in the testing set for this simple model?
R_squared <- 1 - SSE/SST
R_squared #6.1 0.8975923



# 7 #
mod2 <- lm(logVisits ~ laglogVisits + laglogVisitsYear + Year + Month + Region + ParkType + cost, data=train)
summary(mod2)
# Looking at the model summary, which of the following statements are correct (significance at 0.05 level)?
        # 7.1 Both the log visits from last month and last year are significant and are positively associated with the current log visits
        #     None of the park types are significant from the baseline park type (National Battlefield)



# 8 #

predTest2 <- predict(mod2, test)
SSE2 <- sum((predTest2 - test$logVisits)^2)
SSE2
SST2 <- sum((mean(train$logVisits) - test$logVisits)^2)
SST2
#8.1 In the new model, what's the out-of-sample R2 in the testing set?
R_squared2 <- 1 - SSE2/SST2
R_squared2 #8.1 0.937253


# 9 #
library(rpart)
library(rpart.plot)
CARTmodel <- rpart(logVisits ~ laglogVisits + laglogVisitsYear + Year + Month + Region + ParkType + cost, data=train, cp=0.05)
#9.1 Looking at the plot of the tree, how many different predicted values are there?
prp(CARTmodel) # 4

predTest_CART <- predict(CARTmodel, test)
SSE_CART <- sum((predTest_CART - test$logVisits)^2)
SSE_CART
SST_CART <- sum((mean(train$logVisits) - test$logVisits)^2)
SST_CART
#9.2 What is the out-of-sample R2 on the testing set?
R_squared_CART <- 1 - SSE_CART/SST_CART
R_squared_CART # 0.7864307


# 10 #
library(caret)
library(e1071)
set.seed(201)

# Define cross-validation experiment
numFolds = trainControl(method = "cv", number = 10)
cartGrid = expand.grid(.cp = seq(0.0001, 0.005, 0.0001))
# Perform the cross validation
#10.1 What is optimal cp value on this grid?
train(logVisits ~ laglogVisits + laglogVisitsYear + Year + Month + Region + ParkType + cost, data=train, 
      method = "rpart", trControl = numFolds, tuneGrid = cartGrid) #10.1 0.0001 or 1e-04
#10.2 Looking at the validation R2 versus the cp value, we can further refine the cp range. In what direction should it change?
        # smaller values of cp


# 11 #
CARTmodel2 <- rpart(logVisits ~ laglogVisits + laglogVisitsYear + Year + Month + Region + ParkType + cost, data=train, cp=0.0001)

predTest_CART2 <- predict(CARTmodel2, test)
SSE_CART2 <- sum((predTest_CART2 - test$logVisits)^2)
SSE_CART2
SST_CART2 <- sum((mean(train$logVisits) - test$logVisits)^2)
SST_CART2
#11.1 What is the out-of-sample R2 in the testing set?
R_squared_CART2 <- 1 - SSE_CART2/SST_CART2
R_squared_CART2 # 11.1 0.937113



# 12 #
library(randomForest)
set.seed(201)
RF <- randomForest(logVisits ~ laglogVisits + laglogVisitsYear + Year + Month + Region + ParkType + cost, data=train)

predTest_RF <- predict(RF, test)
SSE_RF <- sum((predTest_RF - test$logVisits)^2)
SSE_RF
SST_RF <- sum((mean(train$logVisits) - test$logVisits)^2)
SST_RF
#12.1 What is the R2 on the testing set for the random forest model?
R_squared_RF <- 1 - SSE_RF/SST_RF
R_squared_RF # 12.1 0.947239
