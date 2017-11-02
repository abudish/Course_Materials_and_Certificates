# generating random numbers
x <- rnorm(10)
x

x <- rnorm(10, 20, 2)
x
summary(x)

# set.seed ensures reproducability
set.seed(1)
rnorm(5)
rnorm(5)
set.seed(1)
rnorm(5) # just like the first one rnorm(5) !
# !!!always set the random number seed when conducting a simlation!!!

# Generating Poisson data
rpois(10, 1)
rpois(10, 2)
rpois(10, 20)

# Cumulative distribution
ppois(2, 2) # Pr(x <=2)
ppois(4, 2) # Pr(x <=4)
ppois(6, 2) # Pr(x <=6)