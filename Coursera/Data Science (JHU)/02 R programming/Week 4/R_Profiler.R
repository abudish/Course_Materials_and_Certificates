# using system.time()
        # user time - time charged to the CPU(s) for this expression
        # elapsed tim: "Wall clock" time

# Elapsed time > user time
system.time(readLines("http://www.jhsph.edu"))

# Elapsed time < user time
hilbert <- function(n) {
        i <- 1:n
        1/outer(i - 1, i, "+")
}
x <- hilbert(1000)
system.time(svd(x)) # svd might not work as fast on Windows - Mac is better

# Timing longer expressions
system.time({
        n <- 1000
        r <- numeric(n)
        for (i in 1:n) {
                x <- rnorm(n)
                r[i] <- mean(x)
        }
})

# system.time() assumes that you know where to look
# if you don't know - use Rprof() or the descriptive version summaryRprof()

x <- rnorm(1000)
y <- rnorm(1000)
lm(x ~ y)
Rprof()
summaryRprof()
