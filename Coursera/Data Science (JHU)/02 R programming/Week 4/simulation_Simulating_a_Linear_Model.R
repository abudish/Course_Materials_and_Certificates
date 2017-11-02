# suppose we want to simulate from the following linear model:
# y = beta_0 + beta_1*x + e
# where e ~ N(0,2^2). Assume x ~ N(0,1^2), beta_0 = 0.5, and beta_1 = 2
set.seed(20)
x <- rnorm(100, mean = 0, sd = 1)
e <- rnorm(100, mean = 0, sd = 2)
y <- 0.5 + 2 * x + e
summary(y)
plot(x, y)

# What if x is binary?
set.seed(10)
x <- rbinom(100, 1, 0.5)
e <- rnorm(100, mean = 0, sd = 2)
y <- 0.5 + 2 * x + e
summary(y)
plot(x, y)

# Suppose we want to simulate from a Poisson model where
# Y ~ Poisson(mu)
# log(mu) = beta_0 + beta_1 * x
# and beta_0 = 0.5 and beta_1 = 0.3. We need to use the rpois function for this
set.seed(1)
x <- rnorm(100)
log.mu <- 0.5 + 0.3 * x
y <- rpois(100, exp(log.mu))
summary(y)
plot(x, y)
