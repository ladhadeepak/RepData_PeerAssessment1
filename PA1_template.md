# Reproducible Research - Assignment 1

### Load packages

```r
library(knitr)
library(lubridate)
library(dplyr)
library(ggplot2)
library(lattice)
```

## Loading and preprocessing the data

```r
activity <- read.csv("./activity.csv", colClasses = c("integer", "character", "integer"))
str(activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : chr  "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```


```r
activity$date <- ymd(activity$date)
```

## What is mean total number of steps taken per day?
1.Calculate the total number of steps taken per day


```r
stepsperday <- activity %>% filter(!is.na(steps)) %>% group_by(date) %>% summarise(steps = sum(steps))
stepsperday
```

```
## Source: local data frame [53 x 2]
## 
##          date steps
##        (date) (int)
## 1  2012-10-02   126
## 2  2012-10-03 11352
## 3  2012-10-04 12116
## 4  2012-10-05 13294
## 5  2012-10-06 15420
## 6  2012-10-07 11015
## 7  2012-10-09 12811
## 8  2012-10-10  9900
## 9  2012-10-11 10304
## 10 2012-10-12 17382
## ..        ...   ...
```

2.Make a histogram of the total number of steps taken each day

```r
qplot(steps, data=stepsperday, bins=30)
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png)

3.Calculate and report the mean and median of the total number of steps taken per day

```r
mean_steps <- mean(stepsperday$steps)
median_steps <- median(stepsperday$steps)
mean_steps
```

```
## [1] 10766.19
```

```r
median_steps
```

```
## [1] 10765
```

## What is the average daily activity pattern?
1.Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
avgstepsperinterval <- activity %>% filter(!is.na(steps)) %>% group_by(interval) %>% summarise(steps = mean(steps))
avgstepsperinterval
```

```
## Source: local data frame [288 x 2]
## 
##    interval     steps
##       (int)     (dbl)
## 1         0 1.7169811
## 2         5 0.3396226
## 3        10 0.1320755
## 4        15 0.1509434
## 5        20 0.0754717
## 6        25 2.0943396
## 7        30 0.5283019
## 8        35 0.8679245
## 9        40 0.0000000
## 10       45 1.4716981
## ..      ...       ...
```

```r
plot(avgstepsperinterval$interval, avgstepsperinterval$steps, xlab = "Interval", ylab = "Average Steps", type = "l")
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png)

2.Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
avgstepsperinterval[which.max(avgstepsperinterval$steps),]
```

```
## Source: local data frame [1 x 2]
## 
##   interval    steps
##      (int)    (dbl)
## 1      835 206.1698
```

## Imputing missing values
1.Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
sum(is.na(activity$steps))
```

```
## [1] 2304
```

2.Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

Lets fill in the missing values with mean for that 5-minute interval

3.Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
activity_new <- activity
nasteps <- is.na(activity_new$steps)
table(nasteps)
```

```
## nasteps
## FALSE  TRUE 
## 15264  2304
```

```r
avgintervalarr <- tapply(activity_new$steps, activity_new$interval, mean, na.rm=TRUE)
activity_new$steps[nasteps] <- avgintervalarr[as.character(activity_new$interval[nasteps])]

sum(is.na(activity_new$steps))
```

```
## [1] 0
```

4.Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
stepsperday_new <- activity_new %>% filter(!is.na(steps)) %>% group_by(date) %>% summarise(steps = sum(steps))
stepsperday_new
```

```
## Source: local data frame [61 x 2]
## 
##          date    steps
##        (date)    (dbl)
## 1  2012-10-01 10766.19
## 2  2012-10-02   126.00
## 3  2012-10-03 11352.00
## 4  2012-10-04 12116.00
## 5  2012-10-05 13294.00
## 6  2012-10-06 15420.00
## 7  2012-10-07 11015.00
## 8  2012-10-08 10766.19
## 9  2012-10-09 12811.00
## 10 2012-10-10  9900.00
## ..        ...      ...
```

```r
qplot(steps, data=stepsperday_new, bins=30)
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11-1.png)

```r
mean_steps_new <- mean(stepsperday_new$steps)
median_steps_new <- median(stepsperday_new$steps)
mean_steps_new
```

```
## [1] 10766.19
```

```r
median_steps_new
```

```
## [1] 10766.19
```

With impute in place, now mean and median values are same.

## Are there differences in activity patterns between weekdays and weekends?
1.Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
activity_new <- mutate(activity_new, daytype = ifelse(weekdays(activity_new$date) == "Saturday" | weekdays(activity_new$date) == "Sunday", "weekend", "weekday"))
activity_new$daytype <- as.factor(activity_new$daytype)
head(activity_new)
```

```
##       steps       date interval daytype
## 1 1.7169811 2012-10-01        0 weekday
## 2 0.3396226 2012-10-01        5 weekday
## 3 0.1320755 2012-10-01       10 weekday
## 4 0.1509434 2012-10-01       15 weekday
## 5 0.0754717 2012-10-01       20 weekday
## 6 2.0943396 2012-10-01       25 weekday
```

2.Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
avgstepsperinterval_new <- activity_new %>% group_by(interval, daytype) %>% summarise(steps = mean(steps))
avgstepsperinterval_new
```

```
## Source: local data frame [576 x 3]
## Groups: interval [?]
## 
##    interval daytype       steps
##       (int)  (fctr)       (dbl)
## 1         0 weekday 2.251153040
## 2         0 weekend 0.214622642
## 3         5 weekday 0.445283019
## 4         5 weekend 0.042452830
## 5        10 weekday 0.173165618
## 6        10 weekend 0.016509434
## 7        15 weekday 0.197903564
## 8        15 weekend 0.018867925
## 9        20 weekday 0.098951782
## 10       20 weekend 0.009433962
## ..      ...     ...         ...
```

```r
xyplot(steps ~ interval | daytype, data=avgstepsperinterval_new, layout=c(1,2), type="l")
```

![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-13-1.png)
