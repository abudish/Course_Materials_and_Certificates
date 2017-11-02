# Reading data
if(!exists("NEI")){
        NEI <- readRDS("summarySCC_PM25.rds")      
}

# As I understood from text on 
# https://www.epa.gov/air-emissions-inventories/national-emissions-inventory-nei
# NEI onroad sources seems to be motor vehicle sources
baltimore_onroad <- subset(NEI, fips == "24510" & type == "ON-ROAD")

baltimoremotor <- aggregate(Emissions ~ year, baltimore_onroad, sum)

library(ggplot2)
png(file = "plot5.png")

# How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?
g <- ggplot(baltimoremotor, aes(factor(year), Emissions))
g <- g + geom_bar(stat="identity") +
        xlab("Years") +
        ylab("Amount of PM2.5 emitted, in tons") +
        ggtitle("Total Emissions from motor sources in the Baltimore City from 1999 to 2008")
print(g)

dev.off()