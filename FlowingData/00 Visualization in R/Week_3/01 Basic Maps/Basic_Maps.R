# Load location data
costcos <- read.csv("http://projects.flowingdata.com/tut/data/costcos-geocoded.csv", sep=",")

head(costcos)

# Symbols plot
plot(costcos$Longitude, costcos$Latitude)

# Install package if needed
# install.packages("maps", dependencies=TRUE)
 
# Load maps package
library(maps)

# Map of points
map(database="state", col="#cccccc")

symbols(costcos$Longitude, costcos$Latitude, bg="#e2373f", fg="#ffffff", lwd=0.5, circles=rep(1, length(costcos$Longitude)), inches=0.03, add=TRUE)

# Useless connecting lines
map(database="state", col="#cccccc")
costcos.o <- costcos[order(costcos$Zip.Code, decreasing=FALSE),]
lines(costcos.o$Longitude, costcos.o$Latitude, lwd=0.3, col="red")
points(costcos.o$Longitude, costcos.o$Latitude, pch=21, col="red", cex=0.2)


