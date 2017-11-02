# Reading data
if(!exists("NEI")){
        NEI <- readRDS("summarySCC_PM25.rds")      
}

if(!exists("SCC")){
        SCC <- readRDS("Source_Classification_Code.rds")
}

# Merge datasets
merged <- merge(NEI, SCC, by="SCC")

# find and use all the records with 'coal' in the Short.Name(SCC)
coalLogical <- grepl("coal", merged$Short.Name, ignore.case = T)
coal <- merged[coalLogical, ]
totalcoal <- aggregate(Emissions ~ year, coal, sum)

library(ggplot2)

png(file = "plot4.png")

# Across the United States, 
# how have emissions from coal combustion-related sources changed from 1999–2008?
g <- ggplot(totalcoal, aes(factor(year), Emissions))
g <- g + geom_bar(stat="identity") +
        xlab("Years") +
        ylab("Amount of PM2.5 emitted, in tons") +
        ggtitle("Total Emissions from coal sources from 1999 to 2008")
print(g)

dev.off()


