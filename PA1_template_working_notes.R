library(dplyr)
setwd("C:/Users/Nicholas/Dropbox/Coding/GitHub/RepData_PeerAssessment1/")
getwd();
activity_data <- read.csv("activity.csv")

activity_by_date <- group_by(activity_data, date)
steps_by_date <- summarise(activity_by_date, steps=sum(steps))

activity_by_interval <- group_by(activity_data, interval)
steps_by_interval <- summarise(activity_by_interval, steps=mean(steps, na.rm=TRUE))

hist(steps_by_date$steps, freq=TRUE, label=TRUE, main="Frequency of steps over 60 days", xlab="Steps", ylab="Count of Days", col="darkred")
mean(steps_by_date$steps, na.rm = TRUE)
sum(steps_by_date$steps, na.rm = TRUE)
median(steps_by_date$steps, na.rm = TRUE)
plot(steps_by_interval$interval,steps_by_interval$steps,type="l", main="Average number of steps over 5 minutes intervals", xlab="Interval", ylab="Steps", col="darkred")
which.max(steps_by_interval$steps)

sum(is.na(activity_data$steps))

activity_data_no_na <- activity_data
## loop through all rows
for(i in 1:nrow(activity_data_no_na)){
  ## check if the number of steps is missing
  if(is.na(activity_data_no_na[i,1])) {
    ## replace the number of steps by the average number of steps
    ## by looking up the interval in the steps_by_interval data frame (which has average steps per interval)
    activity_data_no_na[i,1] <- steps_by_interval$steps[steps_by_interval$interval==activity_data_no_na[i,3]]
  }
}
## double check that NA values are replaced
sum(is.na(activity_data_no_na$steps))

activity_data_no_na$date <- as.Date(activity_data_no_na$date)
week_days <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')
activity_data_no_na$wDay <- factor((weekdays(activity_data_no_na$date) %in% week_days),levels=c(FALSE, TRUE), labels=c('weekend', 'weekday'))
activity_by_wday <- group_by(activity_data_no_na, wDay, interval)
steps_by_wday <- summarise(activity_by_wday, steps=mean(steps))


plot(steps_by_wday$interval[steps_by_wday$wDay=="weekend"],steps_by_wday$steps[steps_by_wday$wDay=="weekend"],type="l", main="Weekends - Average number of steps over 5 minutes intervals", xlab="Interval", ylab="Steps", col=c("darkred","darkblue"))
plot(steps_by_wday$interval[steps_by_wday$wDay=="weekday"],steps_by_wday$steps[steps_by_wday$wDay=="weekday"],type="l", main="Weekdays - Average number of steps over 5 minutes intervals", xlab="Interval", ylab="Steps", col=c("darkred","darkblue"))

?xyplot
library(lattice) 
xyplot(steps_by_wday$steps~steps_by_wday$interval|steps_by_wday$wDay,type="l",
       main="Average number of steps over 5 minutes intervals",
       xlab="Interval", ylab="Steps")

steps_by_wday
#
library("ggplot2")
ggplot(steps_by_wday, aes(x = interval, y = steps, group = wDay))



steps_by_wday
