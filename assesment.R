## Loading and preprocessing the data
library(ggplot2)
library(dplyr)
dataFile <- 'activity.zip'
unzip(dataFile, overwrite = FALSE)
#just because we know the file extension, we can use grep
#full data
activities <- read.csv(list.files()[grep('.csv', list.files())]) %>%
  mutate(day = ifelse(strftime(date, '%w') %in% c(0, 6), 'weekend', 'weekday'))
#data without NA's
completeActivities <- activities[complete.cases(activities$steps), ]
#total number of steps taken per day
dailyTotal <- aggregate(activities$steps, by = list(activities$date), FUN = sum)
names(dailyTotal) <- c('date', 'total.steps')
#average number of steps by period divided into weekday and weekend
dailyMean <- aggregate(completeActivities$steps, by = list(completeActivities$interval,
                                                         completeActivities$day), FUN = mean)
names(dailyMean) <- c('interval', 'day', 'mean.steps')

#average daily activity pattern
intervalMean <- aggregate(completeActivities$steps, 
                          by = list(completeActivities$interval), FUN = mean)
names(intervalMean) <- c('interval', 'mean.steps')
#average daily with NA's replaced by mean
filled <- activities
filled$steps[is.na(filled$steps)] <-
  with(filled, ave(steps, interval,
                   FUN = function(x) replace(x, is.na(x), mean(x, na.rm = T))))
dailyTotalFilled <- aggregate(filled$steps, by = list(filled$date), FUN = sum)
names(dailyTotalFilled) <- c('date', 'total.steps')

## What is mean total number of steps taken per day?
plot1 <- ggplot(dailyTotal, aes(x = total.steps)) +
  geom_histogram(color = 'black', fill = 'white') +
  xlab('Total steps') +
  ggtitle('Distribution of mean steps per day') +
  theme(plot.title = element_text(hjust = 0.5))
print(mean(dailyTotal$total.steps, na.rm = TRUE))
print(median(dailyTotal$total.steps, na.rm = TRUE))
print(plot1)

## What is the average daily activity pattern?
plot2 <- ggplot(intervalMean, aes(x = interval, y = mean.steps)) +
  geom_line() +
  xlab('Interval') +
  ylab('Number of steps') +
  ggtitle('Average daily activity pattern') +
  theme(plot.title = element_text(hjust = 0.5))
print(intervalMean$interval[which.max(intervalMean$mean.steps)])
print(plot2)
       

## Imputing missing values
print(sum(is.na(activities)))
plot3 <- ggplot(dailyTotalFilled, aes(x = total.steps)) +
  geom_histogram(color = 'black', fill = 'white') +
  xlab('Total steps') +
  ggtitle("Distribution of mean steps per day (with NA's filled by mean)") +
  theme(plot.title = element_text(hjust = 0.5))
print(mean(dailyTotalFilled$total.steps))
print(median(dailyTotalFilled$total.steps))
print(plot3)

## Are there differences in activity patterns between weekdays and weekends?
plot4 <- ggplot(dailyMean, aes(x = interval, y = mean.steps)) +
  geom_line() +
  xlab('Interval') +
  ylab('Number of steps') +
  facet_grid(rows = vars(day)) +
  ggtitle('Average daily activity pattern by day') +
  theme(plot.title = element_text(hjust = 0.5))
print(plot4)

#Calculate and plot the difference itself
dailyMean2 <- data.frame(unique(dailyMean$interval), 
                         dailyMean$mean.steps[which(dailyMean$day == 'weekend')] -
                           dailyMean$mean.steps[which(dailyMean$day == 'weekday')])
names(dailyMean2) <- c('interval', 'difference')

plot5 <- ggplot(dailyMean2, aes(x = interval, y = difference)) +
  geom_line() +
  xlab('Interval') +
  ylab('Difference between weekend and weekday') +
  ggtitle('Difference between patterns') +
  theme(plot.title = element_text(hjust = 0.5))
print(plot5)