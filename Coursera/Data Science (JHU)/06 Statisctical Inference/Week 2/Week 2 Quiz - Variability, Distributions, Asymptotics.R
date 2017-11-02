# 1 #

#What is the variance of the distribution of the average an IID draw of n observations from a population with mean μ and variance σ2.
#Answer: σ2/n


# 2 #

# Suppose that diastolic blood pressures (DBPs) for men aged 35-44 are normally distributed
# with a mean of 80 (mm Hg) and a standard deviation of 10. 
# About what is the probability that a random 35-44 year old has a DBP less than 70?
pnorm(70, mean = 80, sd = 10) # 0.1586553 or approximately 16%


# 3 #
# Brain volume for adult women is normally distributed with a mean of about 1,100 cc
# for women with a standard deviation of 75 cc. 
# What brain volume represents the 95th percentile?

# The formula is X = mu + Z*sigma, where Z is is standart normal
# for 95th percentile Z= 1.645
1100 + 1.645*75 # 1223.375 
#or
q <- 0.95
mu <- 1100
sigma <- 75
qnorm(q, mean = mu, sd = sigma)



# 4 #
# Refer to the previous question. 
# Brain volume for adult women is about 1,100 cc for women with a standard deviation of 75 cc.
# Consider the sample mean of 100 random adult women from this population. 
# What is the 95th percentile of the distribution of that sample mean?

# X_ = mu + Z*sigma/sqrt(n)
1100 + 1.645*75/sqrt(100) # 1112.338

#or
q <- 0.95
mu <- 1100
sigma <- 75
n <- 100
SE <- sigma/sqrt(n)

qnorm(q, mean = mu, sd = SE)


# 5 #
#You flip a fair coin 5 times, about what's the probability of getting 4 or 5 heads?
p <- 0.5
n <- 5
quantile <- 3 # 4 or 5 out of 5, with lower
probPercentage1 <- round(pbinom(quantile, size=n, prob=p, lower.tail = FALSE) * 100)
probPercentage1 # 19

# 6 #
# The respiratory disturbance index (RDI), a measure of sleep disturbance,
# for a specific population has a mean of 15 (sleep events per hour) and
# a standard deviation of 10. They are not normally distributed.
# Give your best estimate of the probability that a sample mean RDI of 100 people is between 14 and 16 events per hour?

# The Central Limit Theorem (CLT) states that for a large enough sample size n,
# the distribution of the sample mean x_ will approach a normal distribution.
mu <-  15
sigma <- 10
n <- 100
SE <- sigma/sqrt(n)

left <- 14
right <- 16

percentageLeft <- pnorm(left, mean = mu, sd = SE) * 100
percentageRight <- pnorm(right, mean = mu, sd = SE) * 100

probPercentage <- round(percentageRight - percentageLeft)
probPercentage # 68


# 7 #
# Consider a standard uniform density. The mean for this density is .5 and the variance is 1 / 12.
# You sample 1,000 observations from this distribution and take the sample mean,
# what value would you expect it to be near?

# Intuitevily it should be 0.5
q <- 0.5
mu <- .5
n <- 1000
SE <- 1/sqrt(12*n)
qnorm(q, mean = mu, sd = SE) # 0.5



# 8 #
# The number of people showing up at a bus stop is assumed to be
# Poisson with a mean of 5 people per hour. You watch the bus stop for 3 hours.
# About what's the probability of viewing 10 or fewer people?
x <- 5
t <- 3
quantile <- 10
ppois(quantile, lambda = x * t) # 0.12 or 12%
