# Reading 1999 data 
pm0 <- read.table("RD_501_88101_1999-0.txt", comment.char = "#", 
                  header = FALSE, sep = "|", na.strings = "")
dim(pm0)
head(pm0)

cnames <- readLines("RD_501_88101_1999-0.txt", 1)
cnames
cnames <- strsplit(cnames, "|", fixed = TRUE)
cnames
names(pm0) <- cnames[[1]]
head(pm0)
# make valid names
names(pm0) <- make.names(cnames[[1]])
head(pm0)

# grab PM.25 variables
x0 <- pm0$Sample.Value
class(x0)
str(x0)
summary(x0)
mean(is.na(x0)) # about 11 % of missing values
# Are missing values is something to warry about?


# Reading 2012 data 
pm1 <- read.table("RD_501_88101_2012-0.txt", comment.char = "#", 
                  header = FALSE, sep = "|", na.strings = "")
dim(pm1)
head(pm1)
names(pm1) <- make.names(cnames[[1]])
head(pm1)
# grab PM.25 variables
x1 <- pm1$Sample.Value
str(x1)
summary(x1)
mean(is.na(x1)) # about 5 %

# Compare 1999 and 2012 values
summary(x1)
summary(x0)

boxplot(x0, x1)
boxplot(log10(x0), log10(x1))

# Why we have negative values?
summary(x1)
negative <- x1 < 0
str(negative)
sum(negative, na.rm = TRUE)
mean(negative, na.rm = TRUE) # aobut 2 % are negative values

# Maybe negative values happen on a certain time of the year?
dates <- pm1$Date
str(dates)
dates <- as.Date(as.character(dates), "%Y%m%d")
str(dates)
hist(dates, breaks =   "month")
hist(dates[negative], breaks =  "month")

# Let's see how pollution changes only for the one state - NY
site0 <- unique(subset(pm0, State.Code == 36, c(County.Code, Site.ID)))
site1 <- unique(subset(pm1, State.Code == 36, c(County.Code, Site.ID)))
head(site0)
site0 <- paste(site0[,1], site0[,2], sep = ".")
site1 <- paste(site1[,1], site1[,2], sep = ".")
str(site0)
str(site1)
# finding counties in both monitors
both <- intersect(site0, site1)
both

pm0$county.site <- with(pm0, paste(County.Code, Site.ID, sep = "."))
pm1$county.site <- with(pm1, paste(County.Code, Site.ID, sep = "."))
cnt0 <- subset(pm0, State.Code == 36 & county.site %in% both)
cnt1 <- subset(pm1, State.Code == 36 & county.site %in% both)
head(cnt0)

sapply(split(cnt0, cnt0$county.site), nrow)
sapply(split(cnt1, cnt1$county.site), nrow)

pm0sub <- subset(pm0, State.Code == 36 & County.Code == 63 & Site.ID ==2008)
pm1sub <- subset(pm1, State.Code == 36 & County.Code == 63 & Site.ID ==2008)
dim(pm0sub)
dim(pm1sub)

dates0 <- pm0sub$Date
x0sub <- pm0sub$Sample.Value
dates0 <- as.Date(as.character(dates0), "%Y%m%d")
plot(dates0, x0sub)

dates1 <- pm1sub$Date
x1sub <- pm1sub$Sample.Value
dates1 <- as.Date(as.character(dates1), "%Y%m%d")
plot(dates1, x1sub)

# build a panel with two plots side by side
par(mfrow = c(1,2), mar = c(4,4,2,1))
# 1999
plot(dates0, x0sub, pch=20)
abline(h = median(x0sub, na.rm = T))
# 2012 data
plot(dates1, x1sub, pch=20)
abline(h = median(x1sub, na.rm = T))
range(x0sub, x1sub, na.rm = T)
rng <- range(x0sub, x1sub, na.rm = T)

# remake the plots so they have the same range
plot(dates0, x0sub, pch=20, ylim=rng)
abline(h = median(x0sub, na.rm = T))
# 2012 data
plot(dates1, x1sub, pch=20, ylim=rng)
abline(h = median(x1sub, na.rm = T))


## Exploring Change at the State Level
# 1999 data
head(pm0)
mn0 <- with(pm0, tapply(Sample.Value, State.Code, mean, na.rm=T))
str(mn0)
summary(mn0)

# 2012 data
mn1 <- with(pm1, tapply(Sample.Value, State.Code, mean, na.rm=T))
str(mn1)
summary(mn1)

d0 <- data.frame(state = names(mn0), mean = mn0)
d1 <- data.frame(state = names(mn1), mean = mn1)
head(d0)
head(d1)

mrg <- merge(d0, d1, by="state")
dim(mrg)
head(mrg)

par(mfrow = c(1,1))
with(mrg, plot(rep(1999, 52), mrg[, 2], xlim = c(1998, 2013)))
with(mrg, points(rep(2012, 52), mrg[, 3]))
# connecting the dots
segments(rep(1999, 52), mrg[, 2], rep(2012, 52), mrg[, 3])
         