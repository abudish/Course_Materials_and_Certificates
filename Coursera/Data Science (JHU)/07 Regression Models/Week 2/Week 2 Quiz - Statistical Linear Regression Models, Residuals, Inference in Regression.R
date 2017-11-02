# 1 #

# Consider the following data with x as the predictor and y as as the outcome.
x <- c(0.61, 0.93, 0.83, 0.35, 0.54, 0.16, 0.91, 0.62, 0.62)
y <- c(0.67, 0.84, 0.6, 0.18, 0.85, 0.47, 1.1, 0.65, 0.36)

# Give a P-value for the two sided hypothesis test of whether β1 from a linear regression model is 0 or not.
fit <- lm(y ~ x)
sumFit <- summary(fit) 
sumFit # 0.05296


# 2 #
# Consider the previous problem, give the estimate of the residual standard deviation.
sumFit # Residual standard error: 0.223 on 7 degrees of freedom



# 3 #
# In the mtcars data set, fit a linear regression model of weight (predictor) on mpg (outcome).
# Get a 95% confidence interval for the expected mpg at the average weight.
# What is the lower endpoint?
x <- mtcars$wt
y <- mtcars$mpg
fit <- lm(y ~ x)

# read ?predict.lm
predict(fit,data.frame(x=mean(x)), interval="confidence", level = 0.95) # 18.991



# 4 #
# Refer to the previous question. Read the help file for mtcars.
# What is the weight coefficient interpreted as?
?mtcars # The estimated expected change in mpg per 1,000 lb increase in weight.
p1<-predict(fit,data.frame(x), interval="confidence")
plot(x,y,xlab='Weight (1000lb)',ylab='MPG')
abline(fit,col="red")
lines(x,p1[,2],col="purple")
lines(x,p1[,3],col="purple")


# 5 #
# Consider again the mtcars data set and a linear regression model with mpg as predicted by weight (1,000 lbs).
# A new car is coming weighing 3000 pounds. 
# Construct a 95% prediction interval for its mpg.
# What is the upper endpoint?

predict(fit,data.frame(x=3), interval="prediction", level = 0.95) # 27.57355



# 6 #
# Consider again the mtcars data set and a linear regression model with mpg as predicted by weight (in 1,000 lbs). 
# A “short” ton is defined as 2,000 lbs. 
# Construct a 95% confidence interval for the expected change in mpg per 1 short ton increase in weight. 
# Give the lower endpoint.

fit2<-lm(y~I(x/2)) # why x/2 -> 1000lbs/2000lbs per 1 short ton.
tbl2<-summary(fit2)$coefficients
mn<-tbl2[2,1]      #mean is the estimated slope
std_err<-tbl2[2,2] #standard error
deg_fr<-fit2$df    #degrees of freedom

#Two sides T-Tests
mn + c(-1,1) * qt(0.975,df=deg_fr) * std_err # ANSWER: lower end -12.973

# 1000lb v.s. short ton (2000lb per short ton):
par(mfrow=c(1,2))
plot(x,y)
abline(fit,col="red")
plot(x/2,y)
abline(fit2,col="red")




# 7 #
# If my X from a linear regression is measured in centimeters and
# I convert it to meters what would happen to the slope coefficient?

# As we know, multiplication of X by a factor f, results in dividing
# the coefficient by a factor f
# centimeters to meters -> X mulitply by factor f= 0.01
# so coefficient should by divided by 0.01 or coefficients*100

summary(fit)$coefficients
fit3 <- lm(y~I(x/100))
summary(fit3)$coefficients


# 8 #
# I have an outcome, Y, and a predictor, X and fit a linear regression model with Y=β0+β1X+ϵ to obtain β^0 and β^1.
# What would be the consequence to the subsequent slope and intercept 
# if I were to refit the model with a new regressor, X+c for some constant, c?

c<-5
cf1<-summary(fit)$coefficients
cf1

fit4<-lm(y~I(x+c)) # add some constant c
cf2<-summary(fit4)$coefficients
cf2
intercept <- cf2[1,1]

b0_hat<-cf1[1,1]
b1_hat<-cf1[2,1]
c(b0_hat,b1_hat) # 37.285126 -5.344472

# ANSWER: The new intercept would be β0_hat−c*β1_hat
b0_hat - c*b1_hat
intercept == b0_hat - c*b1_hat



# 9 #
# Refer back to the mtcars data set with mpg as an outcome and weight (wt) as the predictor.
# About what is the ratio of the the sum of the squared errors, ∑(Yi−Yi_hat)^2 
# when comparing a model with just an intercept (denominator) to the model with the intercept and slope (numerator)?

# A model with intercept and slope
fit <- lm(y ~ x)

# A model with only intercept
fit5<-lm(y ~ 1)

# A model with only  slope
fit6<-lm(y ~ x - 1)

plot(x,y)

abline(fit,col="red")
abline(fit5,col="blue")
abline(fit6,col="green")


anova(fit) # the sum of the squared errors is 278
anova(fit5) # the sum of the squared errors is 1126

278/1126 # ANSWER 0.2469



# 10 #

#Do the residuals always have to sum to 0 in linear regression?

#both intercept and slope
sum(resid(fit))   # -1.638e-15

#only intercept
sum(resid(fit5)) # -5.995e-15

#only slope
sum(resid(fit6)) # 98.12

# ANSWER: If an intercept is included, then they will sum to 0.

#How can we measure which one is the best model? Use Sigma or R^2

#both intercept and slope
summary(fit)$sigma   # 3.046

#only intercept
summary(fit5)$sigma  # 6.027

#only slope
summary(fit6)$sigma # 11.27
