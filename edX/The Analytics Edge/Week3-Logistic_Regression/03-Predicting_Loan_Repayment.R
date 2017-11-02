# 1
loans <- read.csv('loans.csv')
str(loans)
table(loans$not.fully.paid)
1533/(1533 + 8045)# 0.1601

summary(loans)# log.annual.inc, days.with.cr.line, revol.util, inq.last.6mths, delinq.2yrs, pub.rec

missing <- subset(loans, is.na(log.annual.inc) | is.na(days.with.cr.line) | is.na(revol.util) | is.na(inq.last.6mths) |
               is.na(delinq.2yrs) | is.na(pub.rec))
nrow(missing)
nrow(loans)
table(missing$not.fully.paid)
12/(12 + 50) # We want to be able to predict risk for all borrowers, instead of just the ones with all data reported



library(mice)
set.seed(144)
vars.for.imputation = setdiff(names(loans), "not.fully.paid")
imputed = complete(mice(loans[vars.for.imputation]))
loans[vars.for.imputation] = imputed #We predicted missing variable values using the available independent variables
                                        #for each observation

loans <- read.csv('loans_imputed.csv')



# 2
set.seed(144)
split <- sample.split(loans$not.fully.paid, SplitRatio=0.7)
train <- subset(loans, split == TRUE)
test <- subset(loans, split == FALSE)


mod1 <- glm(not.fully.paid ~ ., data=train, family='binomial')
summary(mod1)# significant coeffs

-9.317e-03*700 - (-9.317e-03*710) # 0.09317
exp(-9.317e-03* 700 )/exp(-9.317e-03 * 710)
# or
exp(0.09317)#1.097648

predicted.risk <- predict(mod1, newdata=test, type='response')
test$predicted.risk <- predicted.risk
table(test$not.fully.paid, predicted.risk > 0.5)
(3 + 2400)/(3 + 2400 + 457 + 13)# 0.8364079
table(test$not.fully.paid, predicted.risk == 0)
2413/(2413 + 460) # 0.8398886

library(ROCR)
ROCRpred  <-  prediction(test$predicted.risk, test$not.fully.paid)
as.numeric(performance(ROCRpred, "auc")@y.values) # 0.6720995

bivariate <- glm(not.fully.paid ~ int.rate, data=train, family='binomial')
summary(bivariate)
cor(train$int.rate, train$fico)
        # int.rate is correlated with other risk-related variables, and therefore does not
        # incrementally improve the model when those other variables are included.


#3
bivariate_predictions <- predict(bivariate, newdata=test, type='response')
max(bivariate_predictions) # 0.426624
summary(bivariate_predictions) # 0 - no loans would be flagged at a logistic regression cutoff of 0.5

ROCRpred_bivar  <-  prediction(bivariate_predictions, test$not.fully.paid)
as.numeric(performance(ROCRpred_bivar, "auc")@y.values) # 0.6239081


#4
c <- 10
r <- 0.06
t <- 3
c*exp(r*t) # 11.97217

c * exp(r*t) - c # profit 1.972174

-c# borrower didn't return money


#5
test$profit <- exp(test$int.rate*3) - 1
test$profit[test$not.fully.paid == 1] <- -1
10*max(test$profit) # 8.895


#6
highInterest <- subset(test, int.rate >= 0.15)
mean(highInterest$profit) # 0.2251015
table(highInterest$not.fully.paid)
350/(350 + 1064) #0.2475248

cutoff <- sort(highInterest$predicted.risk, decreasing=FALSE)[100]
selectedLoans <- subset(highInterest, predicted.risk <= cutoff)
nrow(selectedLoans)
sum(selectedLoans$profit) # 31.27825
table(selectedLoans$not.fully.paid) # 19
