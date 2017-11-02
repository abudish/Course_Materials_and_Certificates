library(ggplot2)
str(mpg)

qplot(displ, hwy, data = mpg)

# Modifying aesthetics
qplot(displ, hwy, data = mpg, color = drv)

# Adding a geom
qplot(displ, hwy, data = mpg, geom = c("point", "smooth"))

# Histograms
qplot(hwy, data = mpg, fill = drv)

# Facet
qplot(displ, hwy, data = mpg, facets = .~drv) # facets = row_variable ~ column_variable
qplot(hwy, data = mpg, facets = drv~., binwidth = 2)
