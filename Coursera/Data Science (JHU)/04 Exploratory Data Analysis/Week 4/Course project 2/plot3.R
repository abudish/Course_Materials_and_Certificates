# Reading data
if(!exists("NEI")){
        NEI <- readRDS("summarySCC_PM25.rds")      
}

baltimore <- subset(NEI, fips == "24510")

library(ggplot2)
png(file = "plot3.png")

# Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable,
# which of these four sources have seen decreases
# in emissions from 1999–2008 for Baltimore City?
# Which have seen increases in emissions from 1999–2008?
total_by_year_and_type <- aggregate(Emissions ~ year + type, baltimore, sum)
g <- ggplot(total_by_year_and_type, aes(year, Emissions, color = type))
g <- g + geom_line() +
        xlab("year") +
        ylab("Amount of PM2.5 emitted, in tons") +
        ggtitle('Total Emissions in the Baltimore City by source type from 1999 to 2008')
print(g)

dev.off()