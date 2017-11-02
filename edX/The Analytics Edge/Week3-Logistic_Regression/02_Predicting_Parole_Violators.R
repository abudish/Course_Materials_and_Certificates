# 1
library(dplyr)
parole <- read.csv('parole.csv')
str(parole) # 675

parole %>% filter(violator == 1) %>% nrow # 78
# or
table(parole$violator)


# 2
# crime, state - unordered with at least 3 levels

parole$crime <- as.factor(parole$crime)
parole$state <- as.factor(parole$state)
summary(parole) #The output becomes similar to that of the table() function 
                        # applied to that variable

set.seed(144)
library(caTools)
split = sample.split(parole$violator, SplitRatio = 0.7)
train = subset(parole, split == TRUE)
test = subset(parole, split == FALSE)
nrow(train)/(nrow(train) + nrow(test))
nrow(test)/(nrow(train) + nrow(test)) # 70% to the training set, 30% to the testing set

# 3.2
# The exact same training/testing set split as the first execution of [1]-[5]
# A different training/testing set split from the first execution of [1]-[5]
# A different training/testing set split from the first execution of [1]-[5]


# 4

mdl1 <- glm(violator ~ ., data=train, family='binomial')
summary(mdl1) # race, state4, multiple.offenses

exp(1)^1.6119919 # Our model predicts that a parolee who committed multiple offenses
                 # has 5.01 times higher odds of being a violator than a parolee
                 # who did not commit multiple offenses but is otherwise identical.

un_parolee  <- c(1, 1, 50.0, 1, 3.0, 12, 0,2, 0)
names(un_parolee) <- names(train)
predict(mdl1, newdata=un_parolee, type='response')
log_odds <- -4.2411574 + 1* 0.3869904 + 1*0.8867192 - 0.0001756 * 50  -0.1238867*3 + 12*0.0802954 + 0.6837143
exp(log_odds) # 0.1825687
P_violator <- 1/(1 + exp(-log_odds))
P_violator # 0.1543832

predictionsTest <- predict(mdl1, newdata=test, type='response')
max(predictionsTest)# 0.9072791

table(test$violator, predictionsTest > 0.5)
12/(12+11) # sensitivity 0.5217391
167/(167+12) # specificity 0.9329609
(167+12)/(167+12+12+11) # accuracy 0.8861386

table(test$violator, predictionsTest == 0)
179/(179 + 23) # 0.8861386

# The board assigns more cost to a false negative than a false positive,
# and should therefore use a logistic regression cutoff less than 0.5.

table(test$violator, predictionsTest > 0.1)
#The model is likely of value to the board, and using 
# a different logistic regression cutoff is likely to improve the model's value.

library(ROCR)
ROCRpred  <-  prediction(predictionsTest, test$violator)
as.numeric(performance(ROCRpred, "auc")@y.values)
# AUC meaning: The probability the model can correctly differentiate between 
# a randomly selected parole violator and a randomly selected parole non-violator.

# Avoid selection bias:
# We should use a dataset tracking a group of parolees from the start of their parole
# until either they violated parole or they completed their term.