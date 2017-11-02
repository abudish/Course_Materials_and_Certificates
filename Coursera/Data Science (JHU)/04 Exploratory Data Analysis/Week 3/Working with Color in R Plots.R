#colorRamp
pal <- colorRamp(c("red", "blue"))
pal(0) # red
pal(1) # blue
pal(0.5) # half red, half blue

pal(seq(0, 1, len = 10))


#colorRampPalette
pal <- colorRampPalette(c("red", "yellow"))
pal(2) # returns two colors, red and yellow
pal(10) # returns ten colors between red and yellow

# brewer
library(RColorBrewer)
cols <- brewer.pal(3, "BuGn")
cols
pal <- colorRampPalette(cols)
image(volcano, col = pal(20))

# smoothScatter
x <- rnorm(10000)
y <- rnorm(10000)
smoothScatter(x, y)

# using rgb
x <- rnorm(1000)
y <- rnorm(1000)
plot(x, y, col = rgb(0,0,0, 0.2), pch=19)
