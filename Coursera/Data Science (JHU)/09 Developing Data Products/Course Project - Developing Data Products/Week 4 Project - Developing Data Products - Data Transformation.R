# source - https://www.gapminder.org/data/
library(ggplot2)
library(reshape2)
library(readxl)
library(ggthemes)

df <- read_xlsx("Working hours per week (1985 - 2007).xlsx")
df <- melt(df, id="Working hours per week")
colnames(df) <- c("country", "year", "hours_per_week")

# Drawing timeseries for only one country
c <- "Australia"
ggplot(data=subset(df, (year %in% year_range) & country=="Australia"), aes(x=year, y=hours_per_week, group=1)) + 
        geom_line() +
        geom_point( size=4, shape=21, fill="white") +
        labs(title="Working hours per week")


# Drawing timeseries for all countries
ggplot(data=molten, aes(x=year, y=hours_per_week, group=country, colour=country)) +
        geom_line() +
        geom_point( size=1, shape=21, fill="white") +
        theme_pander()

# Drawing timeseries for two countries
year_range <- 1991:2000
selected_countries <- subset(df, (year %in% year_range) & (country=="Australia" | country=="Canada"))
ggplot(data=selected_countries,
       aes(x=year, y=hours_per_week, colour=country, group=country)) + 
  geom_line(size=1.5) +
  geom_point(size=2, shape=21, fill="red") +
  ggtitle("Working hours per week, Australia and Canada") +
  theme_hc() +
  scale_colour_hc() +
  theme(plot.title = element_text(hjust = 0.5, size = 22),
        text = element_text(size = 15)) +
  labs(y= "Working hours per week", x="Years")

