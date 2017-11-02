# base plot
library(datasets)
data("cars")
with(cars, plot(speed, dist))

# Lattice Plot
library(lattice)
state <- data.frame(state.x77, region = state.region)
xyplot(Life.Exp ~ Income | region, data = state, layout = c(4,1))

# ggplot2
library(ggplot2)
data("mpg")
qplot(displ, hwy, data = mpg)



# simple Base graphics: histogram
library(datasets)
hist(airquality$Ozone)

# scatterplot
with(airquality, plot(Wind, Ozone))

# boxplot
airquality <- transform(airquality, Month = factor(Month))
boxplot(Ozone ~ Month, airquality, xlab = "Month", ylab = "Ozone (ppb)")

# par() is used for global graphic parameters
par("lty")
par("col")
par("pch")
par("bg")
par("mar")
par("mfrow")

# Base Plot with Annotation
library(datasets)
with(airquality, plot(Wind, Ozone))
title(main = "Ozone and Wind in New York City")

with(airquality, plot(Wind, Ozone, main = "Ozone and Wind in New York City"))
with(subset(airquality, Month == 5), points(Wind, Ozone, col = "blue"))

# adding color and legend
with(airquality, plot(Wind, Ozone, main = "Ozone and Wind in New York City",
                      type = "n"))
with(subset(airquality, Month == 5), points(Wind, Ozone, col = "blue"))
with(subset(airquality, Month != 5), points(Wind, Ozone, col = "red"))
legend("topright", pch = 1, col = (c("blue", "red")), legend = c("May", "Other Months"))

# base plot with Regression Line
with(airquality, plot(Wind, Ozone, main = "Ozone and Wind in New York City", 
                      pch = 20))
model <- lm(Ozone ~ Wind, airquality)
abline(model, lwd = 2)

# Multiple base plots
par(mfrow = c(1, 2))
with(airquality, {
        plot(Wind, Ozone, main = "Ozone and Wind")
        plot(Solar.R, Ozone, main = "Ozone and Solar Radiation")
})

par(mfrow = c(1, 3), mar = c(4, 4, 2, 1), oma = c(0, 0, 2, 0))
with(airquality, {
        plot(Wind, Ozone, main = "Ozone and Wind")
        plot(Solar.R, Ozone, main = "Ozone and Solar Radiation")
        plot(Temp, Ozone, main = "Ozone and Temperature")
        mtext("Ozone and Weather in NewYork City", outer = TRUE)
})