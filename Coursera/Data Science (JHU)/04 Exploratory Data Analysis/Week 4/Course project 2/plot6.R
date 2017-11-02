# Reading data
if(!exists("NEI")){
        NEI <- readRDS("summarySCC_PM25.rds")      
}

# As I understood from text on 
# https://www.epa.gov/air-emissions-inventories/national-emissions-inventory-nei
# NEI onroad sources seems to be motor vehicle sources
baltimore_and_LA_onroad <- subset(NEI, (fips == "24510" | fips == "06037")  & type == "ON-ROAD")
baltimore_and_LA_motor <- aggregate(Emissions ~ year + fips, baltimore_and_LA_onroad, sum)

# change fips to factor variable
baltimore_and_LA_motor$fips <- as.factor(as.character(baltimore_and_LA_motor$fips))

# change fips numbers to the names of places
levels(baltimore_and_LA_motor$fips) <- c("LA county", "Baltimore City")
# rename column 'fips' to 'place'
names(baltimore_and_LA_motor)[names(baltimore_and_LA_motor) == 'fips'] <- 'place'

library(ggplot2)
png(file = "plot6.png")

# Compare emissions from motor vehicle sources in Baltimore City
# with emissions from motor vehicle sources in Los Angeles County, California (fips == "06037").
# Which city has seen greater changes over time in motor vehicle emissions?
g <- ggplot(baltimore_and_LA_motor, aes(factor(year), Emissions))
g <- g + geom_bar(stat="identity") +
        xlab("Years") +
        ylab("Amount of PM2.5 emitted, in tons") +
        ggtitle("Total Emissions from motor sources in the LA County and Baltimore City\n from 1999 to 2008") +
        facet_grid(.~ place)
print(g)

dev.off()
