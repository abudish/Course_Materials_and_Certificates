# 1 #
# Consider the mtcars data set. Fit a model with mpg as the outcome 
# that includes number of cylinders as a factor variable and weight as confounder.
# Give the adjusted estimate for the expected change in mpg comparing 8 cylinders to 4.
fit <- lm(mpg ~ as.factor(cyl) + wt, mtcars)
summary(fit)$coef # (Intercept) means as.factor(cyl)4) 
# Answer: -6.071
plot(fit)


# 2 #
# Consider the mtcars data set. Fit a model with mpg as the outcome 
# that includes number of cylinders as a factor variable and weight as a possible confounding variable.
# Compare the effect of 8 versus 4 cylinders on mpg for the adjusted and unadjusted by weight models.
# Here, adjusted means including the weight variable as a term in the regression model 
# and unadjusted means the model without weight included. 
# What can be said about the effect comparing 8 and 4 cylinders 
# after looking at models with and without weight included?.

# Adjusted by weight models
fit1 <- lm(mpg ~ as.factor(cyl) + wt, mtcars)
summary(fit1)$coef[,1][3]

# Unadjusted by weight models or in other words holding weight constant
fit2 <- lm(mpg ~ as.factor(cyl), mtcars)
summary(fit2)$coef[,1][3]
# Answer: so we just compare two betas and conclude that
# Holding weight constant, cylinder appears to have
# less of an impact on mpg than if weight is disregarded.


# 3 #
# Consider the mtcars data set. Fit a model with mpg as the outcome
# that considers number of cylinders as a factor variable and weight as confounder.
# Now fit a second model with mpg as the outcome model that considers 
# the interaction between number of cylinders (as a factor variable) and weight.
# Give the P-value for the likelihood ratio test comparing the two models
# and suggest a model using 0.05 as a type I error rate significance benchmark.

fit3 <- lm(mpg ~ as.factor(cyl) + wt, mtcars)
fit4 <- lm(mpg ~ as.factor(cyl) + wt + as.factor(cyl)*wt, mtcars)
anova(fit3, fit4) # Pr(>F) 0.1239

# Answer: The P-value is larger than 0.05. So, according to our criterion, we would fail to reject,
# which suggests that the interaction terms may not be necessary.


# 4 #
# Consider the mtcars data set. Fit a model with mpg as the outcome 
# that includes number of cylinders as a factor variable and weight inlcuded in the model as
lm(mpg ~ I(wt * 0.5) + factor(cyl), data = mtcars)
# How is the wt coefficient interpretted?
fit5 <- lm(mpg ~ I(wt * 0.5) + factor(cyl), data = mtcars)

# Answer: The estimated expected change in MPG per one ton increase in weight for a specific number of cylinders (4, 6, 8).

# 5 #
# Consider the following data set
x <- c(0.586, 0.166, -0.042, -0.614, 11.72)
y <- c(0.549, -0.026, -0.127, -0.751, 1.344)
# Give the hat diagonal for the most influential point
fit6 <- lm(y~x)
hatvalues(fit6)
# Answer: 0.9945734

# 6 #
# Consider the following data set
x <- c(0.586, 0.166, -0.042, -0.614, 11.72)
y <- c(0.549, -0.026, -0.127, -0.751, 1.344)
# Give the slope dfbeta for the point with the highest hat value.
fit7 <- lm(y~x)
influence.measures(fit7)
# dfbetas(fit7)
# Answer: highest hat is 0.995, corresponding dfb.x is -1.34e + 02 = -134

# 7 #
# Consider a regression relationship between Y and X with and without adjustment for a third variable Z.
# Which of the following is true about comparing the regression coefficient
# between Y and X with and without adjustment for Z.

# Answer: It is possible for the coefficient to reverse sign after adjustment. 
# For example, it can be strongly significant and positive before adjustment
# and strongly significant and negative after adjustment.