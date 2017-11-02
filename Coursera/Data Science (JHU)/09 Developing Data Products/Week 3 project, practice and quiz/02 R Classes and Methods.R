# The code for implementing S4 classes/methods in R is the methods package
library(methods)

# Class
setClass() # defines a class

# Objects
new() # creates an object, instance of a class

# Things to look up
?Classes
?Methods
?setClass
?setMethod
?setGeneric

# Every object has a class in R
class("foo")
class(NA)
x <- rnorm(100)
y <- x + rnorm(100)
fit <- lm(y ~ x)
class(fit)

# Generic funcitons
mean
print


# S3 methods
methods("mean")


# S4 function
show

# S4 methods
showMethods("show")


# See the code for S3 method
# getS3method(<generic>, <signature>)

# For S4:
# getMethod(<generic>, <signature>)

# S3 Class/Method: Example 1
head(getS3method("mean", "default"), 10)
tail(getS3method("mean", "default"), 10)


# Creating new class
library(methods)
setClass("polygon",
         representation(x = "numeric",
                        y = "numeric"))

# slots for S4 object can be accessed with the @ operator

# For setMethod you need to specify a generic function(plot), and a signature
setMethod("plot", "polygon",
          function(x, y, ...) {
                  plot(x@x, x@y, type = "n", ...)
                  xp <- c(x@x, x@y[1])
                  yp <- c(x@y, x@y[1])
                  lines(xp, yp)
          })

showMethods("plot")

# Using plot on polygon object uses newly created method
p <- new("polygon", x = c(1,2,3,4), y = c(1,2,3,1))
plot(p)
