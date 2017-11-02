d1 <- date()
d1
class(d1)

# Date class()
d2 <- Sys.Date()
d2
class(d2)

# formatting dates

# %d = day as number(0-31), %a = abbreviated weekday, %A = unabbreviated weekday,
# %m = month (00-12), %b = abbreviated month, %B = unabbreviated month,
# %y = 2 digit year, %Y = four digit year
format(d2, "%A %B %d %Y")

# Creating dates
x <- c("1jan1960", "2jan1960", "31mar1960", "30jul1960"); z <- as.Date(x, "%d%b%Y")
z
z[1] - z[2]
as.numeric(z[1] - z[2])

# converting to julian
weekdays(d2)
months(d2)
julian(d2)

# Lubridate
library(lubridate)
ymd("20140108")
mdy("08 04/2013")
dmy("03-04*2013")

# dealing with times
ymd_hms("2011-09-03 10:15:03")
ymd_hms("2011-09-03 10:15:03", tz="Europe/Helsinki")
Sys.timezone() # tz

# Some functions has slightly different syntax
x = dmy(c("1jan1960", "2jan1960", "31mar1960", "30jul1960"))
wday(x[1])
wday(x[1], label = TRUE)
