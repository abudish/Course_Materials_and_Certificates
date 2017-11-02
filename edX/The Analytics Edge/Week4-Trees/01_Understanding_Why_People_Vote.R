#1

gerber <- read.csv('gerber.csv')
str(gerber)
table(gerber$voting)
nrow(gerber$voting == 1)/nrow(gerber) # 0.3158996

library(dplyr)

tapply(gerber$voting, gerber$civicduty, mean)
tapply(gerber$voting, gerber$hawthorne, mean)
tapply(gerber$voting, gerber$self, mean)
tapply(gerber$voting, gerber$neighbors, mean) # neighbors
# or
gerber %>% filter(civicduty == 1) %>% group_by(voting) %>% summarise(n = n()) %>% mutate(prop = n/sum(n))
gerber %>% filter(hawthorne == 1) %>% group_by(voting) %>% summarise(n = n()) %>% mutate(prop = n/sum(n))
gerber %>% filter(self == 1) %>% group_by(voting) %>% summarise(n = n()) %>% mutate(prop = n/sum(n))
gerber %>% filter(neighbors == 1) %>% group_by(voting) %>% summarise(n = n()) %>% mutate(prop = n/sum(n))

logReg1 <- glm(voting ~ civicduty + hawthorne + self + neighbors, data=gerber, family='binomial')
summary(logReg1) # all 4 independant variables are significant

voting_predictions1 <- predict(logReg1, type='response')
confMat1 <- table(gerber$voting, voting_predictions1 > 0.3)
confMat1
sum(diag(confMat1))/sum(confMat1) # 0.5419578
# and
confMat2 <- table(gerber$voting, voting_predictions1 > 0.5)
confMat2
sum(diag(confMat2))/sum(confMat2) # 0.6841004

confMat_baseline <- table(gerber$voting==0)
confMat_baseline
235388/(108696 + 235388) # 0.6841004 - the same accuracy
library(ROCR)
ROCRpred1  <-  prediction(voting_predictions1, gerber$voting)
as.numeric(performance(ROCRpred1, "auc")@y.values) # 0.5308461 is almost like guessing 0.5
# Even though all of the variables are significant, this is a weak predictive model.




# 2
library(rpart)
library(rpart.plot)
CARTmodel <-  rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber)
prp(CARTmodel) # No variables are used (the tree is only a root node) - 
               # none of the variables make a big enough effect to be split on.

CARTmodel2 = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber, cp=0.0)
prp(CARTmodel2) # Neighbor is the first split, civic duty is the last.

#Using only the CART tree plot, determine what fraction (a number between 0 and 1) of "Civic Duty" people voted:
        # A: You can find this answer by reading the tree - the people in the civic duty group correspond to 
        #    the bottom right split, which has value 0.31 in the leaf.

CARTmodel3 = rpart(voting ~ civicduty + hawthorne + self + neighbors + sex, data=gerber, cp=0.0)
prp(CARTmodel3)
summary(CARTmodel3)

# In the control group, which gender is more likely to vote? - Men(0)
# In the "Civic Duty" group, which gender is more likely to vote? - Men(0)

CARTmodel4 = rpart(voting ~ control, data=gerber, cp=0.0)
prp(CARTmodel4, digits=6 )
abs(0.296638 - 0.34)# 0.043362

CARTmodel5 = rpart(voting ~ control + sex, data=gerber, cp=0.0)
prp(CARTmodel5, digits=6 ) 

abs(0.2904558 - 0.3341757) # 0.0437199
abs(0.3027947 - 0.3458183) # 0.0430236
abs(0.290456 - 0.302795) # 0.012339
abs(0.334176 - 0.345818) # 0.011642
# A: They are affected about the same (change in probability within 0.001 of each other).

logReg2 <- glm(voting ~ sex + control, data=gerber, family='binomial')
summary(logReg2) # If you look at the summary of the model, you can see that
                #  the coefficient for the "sex" variable is -0.055791. 
                #  This means that women are less likely to vote

#3.4
Possibilities = data.frame(sex=c(0,0,1,1),control=c(0,1,0,1))
predict(logReg2, newdata=Possibilities, type="response")
round(abs(0.2908065 - 0.290456), 5) # 0.00035

# 3.5
LogModel2 = glm(voting ~ sex + control + sex:control, data=gerber, family="binomial")
summary(LogModel2) # If a person is a woman and in the control group,
                   # the chance that she voted goes down

# 3.6
predict(LogModel2, newdata=Possibilities, type="response")
round(abs(0.2904558 - 0.290456), 5) # 0

