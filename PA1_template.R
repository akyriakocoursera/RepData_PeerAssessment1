library(ggplot2)
activity <- read.csv("activity.csv", colClasses = c("integer", "Date", "numeric"))

complete_cases <- activity[complete.cases(activity),]
cc_sum <-aggregate(steps~date, data=complete_cases, FUN="sum")

bplot <- ggplot(cc_sum, aes(date, steps))
bplot + geom_bar(stat="identity") + labs(title = "Total number of steps per day", x = "Date", y = "Steps (Total)")

steps_mean <- mean(cc_sum$steps)
steps_median <- median(cc_sum$steps)

steps_average <- aggregate(complete_cases$steps, list(interval = complete_cases$interval), FUN="mean")
tsplot <- ggplot(steps_average, aes(interval, steps_average$x))
tsplot + geom_line() + labs(title = "Time series plot of average steps over 5mins Inteval", x = "Interval (5min)", y = "Steps (Average)")

interval_contain_max <- steps_average[steps_average$x == max(steps_average$x), ]

missing_cases_count <- sum(is.na(activity)) 

filled_cases <- activity
steps_average_per_day <- aggregate(steps~date, data=complete_cases, FUN="mean")

for (caseIdx in 1:nrow(filled_cases))
{
  if(is.na(filled_cases$steps[caseIdx]))
  {
      day_average <- steps_average_per_day[steps_average_per_day$date == activity$date[caseIdx], "steps"]
            
      if(length(day_average) == 0)
      {
        day_average <- 0
      }
      
      filled_cases$steps[caseIdx] <- day_average
  }  
}

bplot2 <- ggplot(filled_cases, aes(date, steps))
bplot2 + geom_bar(stat="identity") + labs(title = "Total number of steps per day - Missing Date filled with Day average", x = "Date", y = "Steps (Total)")

cc_sum2 <-aggregate(steps~date, data=filled_cases, FUN="sum")
steps_mean2 <- mean(cc_sum2$steps)
steps_median2 <- median(cc_sum2$steps)

filled_cases$isweekday <- factor(weekdays(filled_cases$date))
levels(filled_cases$isweekday) <- list(weekday = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"), weekend = c("Saturday", "Sunday"))

steps_average2 <- aggregate(filled_cases$steps, list(interval = filled_cases$interval, filled_cases$isweekday), FUN="mean")
names(steps_average2)[3] <- "steps"
names(steps_average2)[2] <- "isweekday"

tsplot2 <- ggplot(filled_cases, aes(interval , steps))
tsplot2 + geom_line() + facet_grid(isweekday ~ .) + labs(x = "Interval", y = "Number of steps")