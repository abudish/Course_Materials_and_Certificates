library(reshape2)
head(mtcars)

# Melting data frames
mtcars$carname <- rownames(mtcars)

# reshape so for each mpg and hp values there is one row
carMelt <- melt(mtcars, id=c("carname", "gear", "cyl"), measure.vars=c("mpg", "hp")) 
head(carMelt, n=3)
tail(carMelt, n=3)

# Casting data frames
cylData <- dcast(carMelt, cyl ~ variable)
cylData

cylData <- dcast(carMelt, cyl ~ variable, mean)
cylData

# Averaging values
head(InsectSprays)
tapply(InsectSprays$count, InsectSprays$spray, sum)

# Another way - split
spins <- split(InsectSprays$count, InsectSprays$spray)
spins

# Another way - apply
sprCount <- lapply(spins, sum)
sprCount

# Another way - combine
unlist(sprCOunt)
sapply(spins, sum)

# Another way - plyr package
ddply(InsectSprays,.(spray), summarize, sum=sum(count))

# Creating a new variable
spraySums <- ddply(InsectSprays,.(spray), summarize, sum=ave(count, FUN=sum))
dim(spraySums)
head(spraySums)
