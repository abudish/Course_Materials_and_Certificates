# 1 #
# In a population of interest, a sample of 9 men yielded a sample average brain volume of 1,100cc
# and a standard deviation of 30cc. 
# What is a 95% Student's T confidence interval for the mean brain volume in this new population?

mn <- 1100
n <- 9
s <- 30
mn + c(-1,1) * qt(.975, n-1) * s/sqrt(n) # 1076.94 1123.06



# 2 #
# A diet pill is given to 9 subjects over six weeks.
# The average difference in weight (follow up - baseline) is -2 pounds.
# What would the standard deviation of the difference in weight have to be
# for the upper endpoint of the 95% T confidence interval to touch 0?
n <- 9
mn <- -2
t_q <- qt(.975, n-1)

# Steps for solving the equation:
# mn + t_q * s/sqrt(n) = 0
# t_q * s/sqrt(n) = - mn
# s/sqrt(n) = -mn/t_q
s <- -mn*sqrt(n)/t_q 
s # 2.601903


# 3 #
# In an effort to improve running performance, 5 runners were either given a protein supplement or placebo.
# Then, after a suitable washout period, they were given the opposite treatment.
# Their mile times were recorded under both the treatment and placebo,
# yielding 10 measurements with 2 per subject. 
# The researchers intend to use a T test and interval to investigate the treatment. 
# Should they use a paired or independent group T test and interval?
# Answer: Paired, it is the same group of people. Just calculate difference between the same person
# given the supplement and placebo



# 4 #
# In a study of emergency room waiting times, investigators consider a new and the standard triage systems.
# To test the systems, administrators selected 20 nights and randomly assigned the new triage system
# to be used on 10 nights and the standard system on the remaining 10 nights.
# They calculated the nightly median waiting time (MWT) to see a physician.
# The average MWT for the new system was 3 hours with a variance of 0.60 
# while the average MWT for the old system was 5 hours with a variance of 0.68.
# Consider the 95% confidence interval estimate for the differences of the mean MWT
# associated with the new system. 
# Assume a constant variance. 
# What is the interval? Subtract in this order (New System - Old System).

# Here we will be using independat t-test with equal variances formulas (Note: Assume a constant variance. )
n <- 20
n_new <- 10
n_old <- 10
MWT_new <- 3
s_new <- sqrt(0.6) # var = s^2 -> s = sqrt(var)
MWT_old <- 5
s_old <- sqrt(0.68)

s_pooled <- sqrt( ((n_old - 1)*s_old^2 + (n_new-1)*s_new^2) / (n_old + n_new - 2) )
MWT_new - MWT_old + c(-1,1)*qt(.975, df = n - 2) * s_pooled * (1/n_new + 1/n_old)^.5 
#Answer: -2.751649 -1.248351



# 5 #
# Suppose that you create a 95% T confidence interval. 
# You then create a 90% interval using the same data.
# What can be said about the 90% interval with respect to the 95% interval?

# Answer: the interval will be narrower.




# 6 #
# To further test the hospital triage system, administrators selected 200 nights
# and randomly assigned a new triage system to be used on 100 nights
# and a standard system on the remaining 100 nights.
# They calculated the nightly median waiting time (MWT) to see a physician.
# The average MWT for the new system was 4 hours with a standard deviation of 0.5 hours
# while the average MWT for the old system was 6 hours with a standard deviation of 2 hours.
# Consider the hypothesis of a decrease in the mean MWT associated with the new treatment.

# What does the 95% independent group confidence interval with unequal variances suggest vis a vis this hypothesis?
# (Because there's so many observations per group, just use the Z quantile instead of the T.)

n <- 200
n_new <- 100
n_old <- 100
MWT_new <- 4
s_new <- sqrt(0.5) # var = s^2 -> s = sqrt(var)
MWT_old <- 6
s_old <- sqrt(2)

# H_0 -> MWT_new - MWT_old = 0
# H_a -> MWT_old > MWT_new or MWT_old - MWT_new > 0

# Here we will be using independat t-test with unequal variances formulas
# and instead of t-static will be using normal z-statistic (qnorm)
MWT_old - MWT_new + c(-1,1)*qnorm(.975)*(s_new^2/n_new + s_old^2/n_old)^.5 # 1.690102 2.309898
# Answer: When subtracting (old - new) the interval is entirely above zero. The new system appears to be effective.



# 7 #
# Suppose that 18 obese subjects were randomized, 9 each, to a new diet pill and a placebo.
# Subjects’ body mass indices (BMIs) were measured at a baseline 
# and again after having received the treatment or placebo for four weeks.
# The average difference from follow-up to the baseline (followup - baseline) was −3 kg/m2 for the treated group
# and 1 kg/m2 for the placebo group.
# The corresponding standard deviations of the differences was 1.5 kg/m2 for the treatment group
# and 1.8 kg/m2 for the placebo group.
# Does the change in BMI over the four week period appear to differ between the treated and placebo groups?
# Assuming normality of the underlying data and a common population variance, calculate the relevant *90%* t confidence interval.
# Subtract in the order of (Treated - Placebo) with the smaller (more negative) number first.


quantile = 0.95 # is 90% with 5% on both sides of the range

n <- 18
n_treated <- 9
n_placebo <- 9
BMI_treated <-  -3
BMI_placebo <- 1
s_treated <- 1.5
s_placebo <- 1.8

# Assuming a common population variance means, using independant t-test with equal variances
s_pooled <- sqrt( ((n_treated - 1)*s_treated^2 + (n_placebo-1)*s_placebo^2) / (n_treated + n_placebo - 2) )
BMI_treated - BMI_placebo + c(-1,1)*qt(quantile, df = n - 2) * s_pooled * (1/n_treated + 1/n_placebo)^.5
# Answer: -5.363579 -2.636421
