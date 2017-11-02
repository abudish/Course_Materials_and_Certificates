# 1 #


# Explained answer to this question can be found here:
# http://stackoverflow.com/questions/24271483/plot-and-solve-an-equation-in-r

# Question: Consider the data set given below
x <- c(0.18, -1.54, 0.42, 0.95)
# And weights given by
w <- c(2, 1, 3, 1)

# Give the value of ?? that minimizes the least squares equation
  # ???wi*(xi?????)^2

# What these weights imply is that these show how many times you see a particular value.
# Using your example, you would see 0.18 2 times, -1.54 1 time, 0.42 3 times and 0.95 1 time.
sumValues <- 0.18*2 + (-1.54)*1 + 0.42*3 + 0.95*1
numberOfValues <- 2 + 1 + 3 + 1
mu <- sumValues/numberOfValues
mu # 0.1471429

# Alternatively:
mu <- sum(x*w)/sum(w)
# Or use build in R function optimize():
optimize( function(mu){ sum(w*(x-mu)^2) }, interval=c(-100,100))



# 2 #

#Consider the following data set

x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
y <- c(1.39, 0.72, 1.55, 0.48, 1.19, -1.59, 1.23, -0.65, 1.49, 0.05)

# Fit the regression through the origin 
# and get the slope treating y as the outcome and x as the regressor(predictor)
# (Hint, do not center the data since we want regression through the origin,
# not through the means of the data.)

# NOTE: from ?formula:
# The - operator removes the specified terms, 
# so that (a+b+c)^2 - a:b is identical to a + b + c + b:c + a:c. 

# It can also used to remove the intercept term: 
# when fitting a linear model y ~ x - 1 specifies a line through the origin.
# A model with no intercept can be also specified as y ~ x + 0 or y ~ 0 + x.
lm(y ~ x - 1) # 0.8263


# 3 #

# Do data(mtcars) from the datasets package and fit the regression model
# with mpg as the outcome and weight as the predictor.
# Give the slope coefficient.
regr_line <- lm(mpg ~ wt, mtcars)
coef(regr_line) # -5.344


# 4 #

# Consider data with an outcome (Y) and a predictor (X).
# The standard deviation of the predictor is 
# one half that of the outcome. 
# The correlation between the two variables is .5.
# What value would the slope coefficient for the regression model
# with Y as the outcome and X as the predictor?
sdx = sdy * 0.5
sdy/sdx = 2
cor_(x,y) = 0.5
# Slope formula:
beta1 = cor_(x,y)*(sdy/sdx)
beta1 <- 0.5 * 2


# 5 #
# Students were given two hard tests and scores were normalized
# to have empirical mean 0 and variance 1.
# The correlation between the scores on the two tests was 0.4.
# What would be the expected score on Quiz 2 
# for a student who had a normalized score of 1.5 on Quiz 1?

# When the data are normalized, the slope of the regression line
# equals correlstion between the outcome and predictor.
# beta1 = cor(y, x)
# y - is the outcome, score on Quiz 2
# x - is the predictor, score on Quiz 1
# y = beta1 * x
x <- 1.5
y <- 0.4 * x
y # 0.6


# 6 #

# Consider the data given by the following
x <- c(8.58, 10.46, 9.01, 9.64, 8.86)
# What is the value of the first measurement
# if x were normalized (to have mean 0 and variance 1)?
xnorm <- (x-mean(x))/sd(x)
xnorm[1] # -0.9718658


# 7 #
#Consider the following data set (used above as well).
x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
y <- c(1.39, 0.72, 1.55, 0.48, 1.19, -1.59, 1.23, -0.65, 1.49, 0.05)
# What is the intercept for fitting the model
# with x as the predictor and y as the outcome?
coef(lm(y ~ x)) # 1.567461 


# 8 #
# You know that both the predictor and response(outcome) have mean 0.
# What can be said about the intercept when you fit a linear regression?

# formula for intercept
# beta0 = mean(y) - beta1*mean(x)
# when both mean(y) and mean(x) are 0, the intercept = 0, is in the origin - (0,0)


# 9 #
# Consider the data given by
x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
# What value minimizes the sum of the squared distances between these points and itself?
# Answer: it's the mean.
mean(x) # 0.573

# 10 #
# Let the slope having fit Y as the outcome and X as the predictor be denoted as ??1.
# Let the slope from fitting X as the outcome and Y as the predictor be denoted as ??1.
# Suppose that you divide ??1 by ??1; in other words consider ??1/??1. 
# What is this ratio always equal to?

# beta1 = cor(y,x)* sd(y)/sd(x)
# gamma1 = cor(y,x)* sd(x)/sd(y)
# beta1/gamma1 =cor(y,x)/cor(y,x) * sd(y)/sd(x) / (sd(x)/sd(y))
# beta/gamma1 = sd(y)^2/sd(x)^2 = var(y)/var(x)
