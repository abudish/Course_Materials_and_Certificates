# 1 #

# A pharmaceutical company is interested in testing 
# a potential blood pressure lowering medication.
# Their first examination considers only subjects
# that received the medication at baseline then two weeks later.
# The data are as follows (SBP in mmHg)

# Subject	Baseline	Week 2
# 1	        140	        132
# 2	        138	        135
# 3	        150	        151
# 4	        148	        146
# 5	        135	        130

# Consider testing the hypothesis that there was a mean reduction in blood pressure?
# Give the P-value for the associated two sided T test.
# (Hint, consider that the observations are paired.)
y <- c(132, 135, 151, 146, 130)
x <- c(140, 138, 150, 148, 135)

# H_a: mu_a < mu_0
t.test(y, x, paired = TRUE, alternative = "two.sided")$p.value 
#Answer: 0.087



# 2 #
# A sample of 9 men yielded a sample average brain volume of 1,100cc
# and a standard deviation of 30cc.
# What is the complete set of values of μ0 
# that a test of H0:μ=μ0 would fail to reject the null hypothesis
# in a two sided 5% Students t-test?
n <- 9
mu <- 1100
s <- 30
alpha <- .05
q <- (1 - alpha/2) # .975 

confidenceInterval <-  mu + c(-1, 1) * qt(q, df=n-1) * s / sqrt(n)
confidenceInterval 
#Answer 1076.94 1123.06


# 3 #
# Researchers conducted a blind taste test of Coke versus Pepsi.
# Each of four people was asked which of two blinded drinks given in random order that they preferred.
# The data was such that 3 of the 4 people chose Coke.
# Assuming that this sample is representative, 
# report a P-value for a test of the hypothesis that Coke is preferred to Pepsi using a one sided exact test.

# Let X be a binary random variable indicating whether Coke (1) or Pepsi (0) is preferred.
# X follows a binomial distribution. 
# H_0: neither Coke or Pepsi is preferred to the other, i.e., p=0.5.
# H_a: p>0.5, Coke is more preferred.

# 1st way
# probability that 3 or more people prefer Coke
# WHY q=2? because we use lower.tail= FALSE,
# that means that we looking to the left from 2, 
# or what is the probability of getting 3 or more out of 4
pbinom(q=2, size=4, prob=0.5, lower.tail=FALSE)
#Answer: 0.3125

# 2nd way
binom.test(x=3, n=4, p=0.5, alt='greater')$p.value # 0.3125



# 4 #
# Infection rates at a hospital above 1 infection per 100 person days at risk
# are believed to be too high and are used as a benchmark.
# A hospital that had previously been above the benchmark
# recently had 10 infections over the last 1,787 person days at risk.
# About what is the one sided P-value for the relevant test of whether the hospital is *below* the standard?

# Poisson case, because we have rate
rate <- 1/100
x <- 10
t <- 1787
test <- poisson.test(x, t, rate, alternative = "less")
test$p.value
# Answer 0.032



# 5 #
# Suppose that 18 obese subjects were randomized, 9 each, to a new diet pill and a placebo.
# Subjects’ body mass indices (BMIs) were measured at a baseline and again
# after having received the treatment or placebo for four weeks. The average difference
# from follow-up to the baseline (followup - baseline) was −3 kg/m2 for the treated group
# and 1 kg/m2 for the placebo group.
# The corresponding standard deviations of the differences was 1.5 kg/m2 for the treatment group
# and 1.8 kg/m2 for the placebo group.
# Does the change in BMI appear to differ between the treated and placebo groups?
# Assuming normality of the underlying data and a common population variance, give a pvalue for a two sided t test.

n_treated <- 9
n_placebo <- 9
BMI_treated <-  -3
BMI_placebo <- 1
s_treated <- 1.5
s_placebo <- 1.8

# Assuming a common population variance means, using independant t-test with equal variances
s_pooled <- sqrt( ((n_treated - 1)*s_treated^2 + (n_placebo-1)*s_placebo^2) / (n_treated + n_placebo - 2) )

# Confidence interval in this case is
# BMI_treated - BMI_placebo + c(-1,1)*t_statistic * s_pooled * (1/n_treated + 1/n_placebo)^.5

# Common formulat for calculating t-statistic is
# t_statistic = (Y_ - mu_0)/s*n^.5
# s is s_pooled = sqrt( ((n_x -1)*S_x^2 + (n_y - 1)*S_y^2)/(n_x+n_y-2) )
# In our case instead of n we use (1/n_x + 1/n_y)^.5

# so if we derive, t_statistic would be
t_statistic <- (BMI_treated - BMI_placebo)/(s_pooled * (1/n_treated + 1/n_placebo)^.5)
pval <- pt(q=t_statistic, df= n_treated + n_placebo - 2)
pval # Less than 0.01, 5.125872e-05


# 6 #
# Brain volumes for 9 men yielded a 90% confidence interval of 1,077 cc to 1,123 cc.
# Would you reject in a two sided 5% hypothesis test of H0:μ=1,078?

# we don't have to do any calculations because the 95% confidence region contains the 90% confidence interval.
# We also know that since H0: µ = 1,078 falls within the region it is plausible
# and we cannot reject the null because 1,078 is a plausible value of the population parameter.
# Answer: No you wouldn't reject.



# 7 #
# Researchers would like to conduct a study of 100 healthy adults 
# to detect a four year mean brain volume loss of .01 mm3.
# Assume that the standard deviation of four year volume loss in this population is .04 mm3.
# About what would be the power of the study for a 5% one sided test versus a null hypothesis of no volume loss?
n <- 100
d <- .01 # delta
s <- .04
alpha <- .05

power.t.test(n = 100, delta = .01 , sd = .04, type = "one.sample", alt = "one.sided", sig.level = .05)$power
# Answer: 0.7989855


# 8 #
# Researchers would like to conduct a study of n healthy adults
# to detect a four year mean brain volume loss of .01 mm3.
# Assume that the standard deviation of four year volume loss in this population is .04 mm3.
# About what would be the value of n needed for 90% power of type one error rate
# of 5% one sided test versus a null hypothesis of no volume loss?

power.t.test(power = .9 , delta = .01 , sd = .04, type = "one.sample", alt = "one.sided", sig.level = .05)$n
# Answer 138.4 or if we round -> 139



# 9 #
# As you increase the type one error rate, α, what happens to power?
# Answer: power goes up