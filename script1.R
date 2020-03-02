table1 <- read.csv("activity.csv")
daytotal <- tapply(table1$step, table1$date, sum, na.rm = T)
df_daytotal <- data.frame(daytotal)
hist(df_daytotal$daytotal, breaks = 20, xlab = "total steps per day")
summary(df_daytotal$daytotal)
median(df_daytotal$daytotal)
mean(df_daytotal$daytotal)

library(dplyr)
interval_mean <- table1 %>% group_by(interval) %>% summarise(mean.interval = mean(steps, na.rm = T))
plot(interval_mean$interval, interval_mean$mean.interval, type="l")
interval_mean[interval_mean$mean.interval == max(interval_mean$mean.interval),]$interval

for (n in seq(1, nrow(table1),1)) {
  if (is.na(table1[[n,"steps"]])) {
    table1[[n, "steps"]] <- interval_mean[interval_mean["interval"]==table1[[n,"interval"]],"mean.interval"][[1]]
  }
}
daytotal <- tapply(table1$step, table1$date, sum, na.rm = T)
df_daytotal <- data.frame(daytotal)
hist(df_daytotal$daytotal, breaks = 20, xlab = "total steps per day")
summary(df_daytotal$daytotal)


library(lattice)
table1$date2 <- as.Date(table1$date, "%Y-%m-%d")
table1$weekday <- weekdays(table1$date2)
table1$weekdayCheck <- ifelse(table1$weekday %in% c("Saturday", "Sunday"), "Weekend", "Weekday")

table1 <- transform(table1, weekdayCheck = as.factor(weekdayCheck))
table1$interval.weekday <- paste(table1$interval, table1$weekdayCheck)
interval_wday_mean <- table1 %>% group_by(interval.weekday) %>%
  summarise(mean.wday.interval = mean(steps), weekdayCheck = unique(weekdayCheck),
            interval = mean(interval))

xyplot(mean.wday.interval ~ interval | weekdayCheck, data = interval_wday_mean, 
       layout = c(1,2), ylab = "number of steps")






