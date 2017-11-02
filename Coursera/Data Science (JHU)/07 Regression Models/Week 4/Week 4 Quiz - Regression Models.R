# 1 #
# Consider the space shuttle data ?shuttle in the MASS library.
# Consider modeling the use of the autolander as the outcome (variable name use).
# Fit a logistic regression model with autolander (variable auto) use (labeled as "auto" 1)
# versus not (0) as predicted by wind sign (variable wind).
# Give the estimated odds ratio for autolander
# use comparing head winds, labeled as "head" in the variable headwind (numerator)
# to tail winds (denominator).

library(MASS)
shuttle$use.binom <- as.numeric(shuttle$use == "auto")

# Removing intercept from a model.
# Why? to recieves betas for head and tail in absolute values
# Later they will be used to calculate odds ratios
mdl1 <- glm(use.binom ~ wind - 1, family = "binomial", shuttle)
coefs <- mdl1$coefficients

beta_windHead <- coefs['windhead']
beta_windTail <- coefs['windtail']

odds_autoland_when_windHead <- exp(beta_windHead)
odds_autoland_when_windTail <- exp(beta_windTail)
odds_autoland_when_windHead/odds_autoland_when_windTail 
# Answer: 0.9686888


# 2 #
# Consider the previous problem. Give the estimated odds ratio for autolander use comparing
# head winds (numerator) to tail winds (denominator) adjusting for wind strength from the variable magn.
mdl2 <- glm(use.binom ~ wind + magn - 1, family = "binomial", shuttle)
coefs <- mdl2$coefficients

beta_windHead <- coefs['windhead']
beta_windTail <- coefs['windtail']

odds_autoland_when_windHead <- exp(beta_windHead)
odds_autoland_when_windTail <- exp(beta_windTail)
odds_autoland_when_windHead/odds_autoland_when_windTail 
#Answer 0.9684981 


# 3 #
# If you fit a logistic regression model to a binary variable, for example use of the autolander,
# then fit a logistic regression model for one minus the outcome (not using the autolander)
# what happens to the coefficients?
mdl3 <- glm(I(1 - use.binom) ~ wind - 1, family = "binomial", shuttle)
rbind(mdl1$coefficients, mdl3$coefficients)
# Answer: The coefficients reverse their signs.

# 4 #
# Consider the insect spray data InsectSprays. 
# Fit a Poisson model using spray as a factor level. 
# Report the estimated relative rate comapring spray A (numerator) to spray B (denominator).
mdl4 <- glm(count ~ spray - 1, data = InsectSprays, family = "poisson")
summary(mdl4)
cf4 <- exp(coef(mdl4))
cf4[1]/cf4[2]
# Answer 0.9456522


# 5 #
# Consider a Poisson glm with an offset, t. 
# So, for example, a model of the form glm(count ~ x + offset(t), family = poisson)
# where x is a factor variable comparing a treatment (1) to a control (0)
# and t is the natural log of a monitoring time.
# What is impact of the coefficient for x if we fit the model
# glm(count ~ x + offset(t2), family = poisson)  where t2 <- log(10) + t?
# In other words, what happens to the coefficients if we change the units of the offset variable.
# (Note, adding log(10) on the log scale is multiplying by 10 on the original scale.)
mdl5 <- glm(count ~ factor(spray), data = InsectSprays, family = "poisson", offset = (log(count + 1)))
mdl6 <- glm(count ~ factor(spray), data = InsectSprays, family = "poisson", offset = (log(count + 1) + log(10)))
mdl5$coefficients
mdl6$coefficients

mdl5$coefficients[1] - log(10)
mdl6$coefficients[1]

# Answer: The coefficient estimate is unchanged.
# Note, the coeffcients are unchanged, except the intercept, which is shifted by log(10).
# Recall that, except the intercept, all of the coefficients are interpretted as log relative
# rates when holding the other variables of offset constant. Thus, a unit change in the
# offset would cancel out. This is not true of the intercept, which is interperted as the log
# rate (not relative rate) with all of the covariates set to 0.

# 6 #
x <- -5:5
y <- c(5.12, 3.93, 2.67, 1.87, 0.52, 0.08, 0.93, 2.05, 2.54, 3.87, 4.97)

# Using a knot point at 0, fit a linear model that looks like a hockey stick with two lines meeting at x=0.
# Include an intercept term, x and the knot point term. What is the estimated slope of the line after 0?
knots <- 0
splineTerms <- sapply(knots, function(knot) (x > knot) * (x - knot))
xMat <- cbind(1, x, splineTerms)
xMat

fit6 <- lm(y ~ xMat - 1)
fit6

yhat <- predict(fit6)
plot(x, y, frame = FALSE, pch = 21, bg = "lightblue", cex = 2)
lines(x, yhat, col = "red", lwd = 2)

fit6$coef[2] + fit6$coef[3]

# Answer: 1.013