# Peer Assessment 1 for Reproducible Research (Author: JYue)

## Loading and preprocessing the data

Show any code that is needed to
    1. Load the data (i.e. read.csv())
    2. Process/transform the data (if necessary) into a format suitable for your analysis
    

```
## 
## Attaching package: 'dplyr'
## 
## The following objects are masked from 'package:stats':
## 
##     filter, lag
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```
## Warning: package 'ggplot2' was built under R version 3.2.2
```



# What is mean total number of steps taken per day?
For this part of the assignment, you can ignore the missing values in the dataset.
    1. Calculate the total number of steps taken per day
    2. If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day
    3. Calculate and report the mean and median of the total number of steps taken per day
    



```r
activity <- read.csv("./PA1/activity.csv")
steps <- group_by(activity, date)

steps <- mutate(steps, total = sum(steps, na.rm = TRUE))
steps <- mutate(steps, mean = mean(steps, na.rm = TRUE))
# steps <- mutate(steps, median = median(steps, na.rm = TRUE))

mn <- summarize(steps, mean = mean(total, na.rm = TRUE))
md <- summarize(steps, median = median(total, na.rm = TRUE))
mean_steps <- summarize(mn, mean = mean(mean, na.rm = TRUE))
median_steps <- summarize(md, median = median(median, na.rm = TRUE))

df <- data.frame(c(mean_steps, median_steps))
```
Average total steps per day is 9354.2295082
Median total steps per day is 1.0395\times 10^{4}


```r
qplot(total, data = steps, fill = date, xlab = "Total number of steps per day") + 
        geom_vline(aes(xintercept = mean), data = df, colour = "black") +
        geom_vline(aes(xintercept = median), data = df, colour = "red")
```

```
## stat_bin: binwidth defaulted to range/30. Use 'binwidth = x' to adjust this.
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

# What is the average daily activity pattern?
    1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis).
    2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
int <- group_by(activity, interval)
int <- mutate(int, average = mean(steps, na.rm = TRUE))

max.step <- int[which.max(int$average),]
```

Maximum 5-minute interval is NA, 2012-10-01, 835, 206.1698113


```r
qplot(interval, average, data = int) + xlab("5-minute intervals") + geom_line() +
        ylab("Average Steps Taken Per Day") + geom_vline(aes(xintercept = interval), data = max.step, colour = "red")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 

# Imputing missing values
Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.
    1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs).
    2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
    3. Create a new dataset that is equal to the original dataset but with the missing data filled in.
    4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
NA.steps <- sum(is.na(steps$steps))
NA.date <- sum(is.na(steps$date))
NA.interval <- sum(is.na(steps$interval))
```

Total number of missing values is 2304


```r
steps.filled <- steps

k <- which(is.na(steps.filled$steps), arr.ind=TRUE)
steps.filled$steps[k] <- 0

steps.filled <- mutate(steps.filled, total = sum(steps, na.rm = TRUE))
steps.filled <- mutate(steps.filled, mean = mean(steps, na.rm = TRUE))
# steps.filled <- mutate(steps.filled, median = median(steps, na.rm = TRUE))

mn.filled <- summarize(steps.filled, mean = mean(total, na.rm = TRUE))
md.filled <- summarize(steps.filled, median = median(total, na.rm = TRUE))
mean_steps.filled <- summarize(mn.filled, mean = mean(mean, na.rm = TRUE))
median_steps.filled <- summarize(md.filled, median = median(median, na.rm = TRUE))

df.filled <- data.frame(c(mean_steps.filled, median_steps.filled))
```

Mean number of steps per day is 9354.2295082
Median number of steps per day is 1.0395\times 10^{4}


```r
qplot(total, data = steps.filled, fill = date, xlab = "Total number of steps per day") + 
        geom_vline(aes(xintercept = mean), data = df.filled, colour = "black") +
        geom_vline(aes(xintercept = median), data = df.filled, colour = "red")
```

```
## stat_bin: binwidth defaulted to range/30. Use 'binwidth = x' to adjust this.
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png) 

# Are there differences in activity patterns between weekdays and weekends?
For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.
    1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.
    2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
steps.filled$date <- as.Date(steps.filled$date)
weekday <- weekdays(steps.filled$date)
steps.filled <- cbind(steps.filled, weekday)

weekdays1 <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')

wDay <- factor((weekday %in% weekdays1), 
                   levels=c(FALSE, TRUE), labels=c('weekend', 'weekday'))

steps.filled <- cbind(steps.filled, wDay)

steps.filled <- dplyr::select(steps.filled, -mean)

steps.filled <- dplyr::group_by(steps.filled, interval, wDay)

steps.filled <- dplyr::mutate(steps.filled, average = mean(steps, na.rm = TRUE))
```


```r
qplot(interval, average, data = steps.filled) + 
        facet_grid(.~wDay) + 
        geom_line() +
        labs(x = expression("5-minute intervals")) + 
        labs(y = expression("Average Steps Taken Per Day")) +
        labs(title = "Average Steps Taken Per Day for Each 5-min Interval on Weekends and Weekdays")
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png) 
