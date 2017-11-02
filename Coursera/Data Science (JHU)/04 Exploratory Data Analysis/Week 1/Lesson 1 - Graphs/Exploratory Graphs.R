# First, we can read the data into R with read.csv().
# This dataset contains the annual mean PM2.5 averaged over the period 2008 through 2010

class <- c("numeric", "character", "factor", "numeric", "numeric")
pollution <- read.csv("data/avgpm25.csv", colClasses = class)
head(pollution)

summary(pollution$pm25)

# Boxplot
boxplot(pollution$pm25, col = "blue")

# Histogram
hist(pollution$pm25, col = "green")
rug(pollution$pm25)

hist(pollution$pm25, col = "green", breaks = 100)
rug(pollution$pm25)

# Overlaying features
boxplot(pollution$pm25, col = "blue")
abline(h = 12)

hist(pollution$pm25, col = "green")
abline(v = 12, lwd =2)
abline(v = median(pollution$pm25), col = "magenta", lwd = 4)

# Barplot
barplot(table(pollution$region), col = "wheat", main = "Number of Counies in Each Region")



# Multiple Boxplots
boxplot(pm25 ~ region, data = pollution, col = "red")

# Multiple histograms
par(mfrow = c(2,1), mar = c(4, 4, 2, 1))
hist(subset(pollution, region == "east")$pm25, col = "green")
hist(subset(pollution, region == "west")$pm25, col = "green")

# Scatterplot
with(pollution, plot(latitude, pm25))
abline(h = 12, lwd = 2, lty = 2)

# Scatterplot - using color
with(pollution, plot(latitude, pm25, col = region))
abline(h = 12, lwd = 2, lty = 2)

# Multiple Scatterplots
par(mfrow = c(1, 2), mar = c(5, 4, 2, 1))
with(subset(pollution, region == "west"), plot(latitude, pm25, main = "West"))
with(subset(pollution, region == "east"), plot(latitude, pm25, main = "East"))