install.packages("quantmod")

# Google data
library(quantmod)
from_dat <- as.Date("01/01/08", format="%m/%d/%y")
to_dat <- as.Date("12/31/13", format="%m/%d/%y")
getSymbols("GOOG", src="google", from=from_dat, to=to_dat)

head(GOOG)


# Summarize monthly and store as time series

# mGoog <- to.monthly(GOOG) - doesn't work, old version of to.monthly() method
mGoog <- apply.monthly(GOOG, mean)
# mGoog <- to.monthly(GOOG[, 1:4])  - this line also works
googOpen <- Op(mGoog)
ts1 <- ts(googOpen, frequency = 12)
plot(ts1, xlab="Years+1", ylab="GOOG")


# Decompose a time series into parts
plot(decompose(ts1), xlab="Years+1")

# Training and test sets
ts1Train <- window(ts1, start=1, end=5)
ts1Test <- window(ts1, start=5, end=(7-0.01))
ts1Train


# Simple moving average
# forecast package is needed to use ma() function - moving average smoother
install.packages("forecast")
library(forecast)

plot(ts1Train)
lines(ma(ts1Train, order=3), col="red")


# Exponential smoothing
ets1 <- ets(ts1Train, model="MMM")
fcast <- forecast(ets1)

# Get the accuracy
accuracy(fcast, ts1Test)
plot(fcast)
lines(ts1Test, col="red")