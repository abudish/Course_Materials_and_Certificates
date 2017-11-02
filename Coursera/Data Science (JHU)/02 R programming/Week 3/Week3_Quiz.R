library(datasets)
data(iris)
# 1 In this dataset,
#what is the mean of 'Sepal.Length' for the species virginica? 
irisbyspecies <- split(iris, iris$Species)
virginica <- irisbyspecies[[3]]
mean(virginica$Sepal.Length)
#or
apply(virginica[, 1:4], 2, mean)
tapply(iris$Sepal.Length, iris$Species, mean)

# 2 Continuing with the 'iris' dataset from the previous Question,
#what R code returns a vector of the means of the variables
#'Sepal.Length', 'Sepal.Width', 'Petal.Length', and 'Petal.Width'?
colMeans(iris)
apply(iris, 2, mean)
rowMeans(iris[, 1:4])
apply(iris, 1, mean)
apply(iris[, 1:4], 1, mean)
apply(iris[, 1:4], 2, mean) # correct

data(mtcars)
# 3 How can one calculate the average miles per gallon (mpg) 
#by number of cylinders in the car (cyl)? Select all that apply.
mean(mtcars$mpg, mtcars$cyl)
sapply(split(mtcars$mpg, mtcars$cyl), mean) # correct
tapply(mtcars$cyl, mtcars$mpg, mean)
apply(mtcars, 2, mean)
with(mtcars, tapply(mpg, cyl, mean)) # correct
split(mtcars, mtcars$cyl)
tapply(mtcars$mpg, mtcars$cyl, mean) # correct
lapply(mtcars, mean)
sapply(mtcars, cyl, mean)

# 4 Continuing with the 'mtcars' dataset from the previous Question,
#what is the absolute(module) difference between the average horsepower
#of 4-cylinder cars and the average horsepower of 8-cylinder cars?
average_hoursepower <- tapply(mtcars$hp, mtcars$cyl, mean)
round(abs(average_hoursepower["4"] - average_hoursepower["8"])) 

# 5 If you run
debug(ls)
# what happens when you next call the 'ls' function?
ls()
# Answer - Execution of 'ls' will suspend at the beginning of the function
# and you will be in the browser.