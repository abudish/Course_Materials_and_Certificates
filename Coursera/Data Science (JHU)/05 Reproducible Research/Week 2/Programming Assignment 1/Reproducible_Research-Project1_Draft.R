download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip", destfile = "activity.zip")

activity <- read.csv(unz("activity.zip", "activity.csv"))
activity$date <- as.Date(activity$date, format = "%Y-%m-%d")


totalsteps <- with(activity, tapply(steps, date, sum, na.rm = TRUE))

averages <- aggregate(x = list(steps=activity$steps),
                      by = list(interval=activity$interval),
                      FUN = mean, 
                      na.rm = TRUE)


impute <- function(steps, interval) {
        imputed_value <- NA
        if (is.na(steps)) {
                imputed_value <- averages[averages$interval == interval, "steps"]
        }
        else {
                imputed_value <- c(steps)
        }
        imputed_value
}

activity_filled <- activity
activity_filled$steps <- mapply(impute, activity_filled$steps, activity_filled$interval)



weekday_or_weekend <- function(date) {
        day <- weekdays(date)
        if (day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")) 
                return("weekday") 
        else 
                return("weekend")
}

activity_filled$daytype <- sapply(activity_filled$date, weekday_or_weekend)

averages_by_type <- aggregate(activity_filled$steps,
                              by=list(activity_filled$daytype,
                                      activity_filled$interval),
                              mean)
names(averages_by_type) <- c("daytype", "interval", "steps")

xyplot(steps ~ interval | daytype, averages_by_type, 
       type="l", 
       lwd=1, 
       xlab="Interval", 
       ylab="Number of steps", 
       layout=c(1,2))