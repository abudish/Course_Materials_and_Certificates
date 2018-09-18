# Load location data and default plot
costcos <- read.csv("http://projects.flowingdata.com/tut/data/costcos-geocoded.csv", sep=",")
plot(costcos$Longitude, costcos$Latitude)

projected_coords <- mapproject(costcos$Longitude, costcos$Latitude, projection="albers", parameters=c(39,45))

head(projected_coords$x)

head(projected_coords$y)

head(costcos[,c("Longitude", "Latitude")])

?mapproject