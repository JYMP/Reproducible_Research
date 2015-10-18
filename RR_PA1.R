# set working directory
wd <- setwd("C:/My Documents/Training/Coursera/R/Reproducible_Research")

# Check if a directory called PA1 exists.  If not create a directory
if (!file.exists("PA1")){
        dir.create("PA1")
        
        # Download zipped file from url and unzip it into working directory
        fileurl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
        download.file(fileurl, destfile = "./PA1/Activity_Monitoring.zip")
        unzip("./PA1/Activity_Monitoring.zip", exdir = "./PA1")
}

#load dplyr package
library(dplyr)

# Read in the file
activity <- read.csv("./PA1/activity.csv")

# Categorize the data frame by date
steps <- group_by(activity, date)

# Calculate the total number of steps, mean and median over each day
steps <- mutate(steps, total = sum(steps, na.rm = TRUE))
steps <- mutate(steps, mean = mean(steps, na.rm = TRUE))
steps <- mutate(steps, median = median(steps, na.rm = TRUE))

# Load ggplot2
library(ggplot2)

# Calculate the mean and median total steps taken per day
mn <- summarize(steps, mean = mean(total, na.rm = TRUE))
md <- summarize(steps, median = median(total, na.rm = TRUE))
mean_steps <- summarize(mn, mean = mean(mean, na.rm = TRUE))
median_steps <- summarize(md, median = median(median, na.rm = TRUE))

# Do a histogram plot of the total number of steps taken per day as well as plot the mean
# median lines
# First create a dataframe of mean_steps and median_steps
df <- data.frame(c(mean_steps, median_steps))
qplot(total, data = steps, fill = date, xlab = "Total number of steps per day") + 
        geom_vline(aes(xintercept = mean), data = df, colour = "black") +
        geom_vline(aes(xintercept = median), data = df, colour = "red")

# Categorize the data frame by 5-minute intervals
int <- group_by(activity, interval)

# Calculate the total number of steps, mean and median over each day
int <- mutate(int, average = mean(steps, na.rm = TRUE))

# Perform time series plot of Average Steps per 5-minute interval across all days
# Calcuate the 5-minute interval with the maximum average number of steps
max.step <- int[which.max(int$average),]
qplot(interval, average, data = int) + geom_line(linetype = 'l') + xlab("5-minute intervals") + 
        ylab("Average Steps Taken Per Day") + geom_vline(aes(xintercept = interval), data = max.step, colour = "red")

# Calculate the number of missing values in steps
NA.steps <- sum(is.na(steps$steps))
NA.date <- sum(is.na(steps$date))
NA.interval <- sum(is.na(steps$interval))

# Assign steps to a new dataset called steps.filled
steps.filled <- steps

# Calculate which row in steps.filled are NA and assign values of 0 to replace the NA values
k <- which(is.na(steps.filled$steps), arr.ind=TRUE)
steps.filled$steps[k] <- 0

# Calculate the total number of steps, mean and median over each day
steps.filled <- mutate(steps.filled, total = sum(steps.filled, na.rm = TRUE))
steps.filled <- mutate(steps.filled, mean = mean(steps.filled, na.rm = TRUE))
steps.filled <- mutate(steps.filled, median = median(steps.filled, na.rm = TRUE))

# Calculate the mean and median total steps taken per day of the new dataset
mn.filled <- summarize(steps.filled, mean = mean(total, na.rm = TRUE))
md.filled <- summarize(steps.filled, median = median(total, na.rm = TRUE))
mean_steps.filled <- summarize(mn.filled, mean = mean(mean, na.rm = TRUE))
median_steps.filled <- summarize(md.filled, median = median(median, na.rm = TRUE))

# Do a histogram plot of the total number of steps taken per day as well as plot the mean
# median lines
# First create a dataframe of mean_steps and median_steps
df.filled <- data.frame(c(mean_steps.filled, median_steps.filled))
qplot(total, data = steps.filled, fill = date, xlab = "Total number of steps per day") + 
        geom_vline(aes(xintercept = mean), data = df.filled, colour = "black") +
        geom_vline(aes(xintercept = median), data = df.filled, colour = "red")

# The old mean_steps is equal to the new mean_steps.filled
# The old median_steps is equal to the new median_steps.filled
# This is because we eradicated the effect of the NA values by replacing them with zeroes

# Convert the date column in steps.filled to Date format
# Create a new vector called weekday for each of the dates
# Column bind weekday to steps.filled
steps.filled$date <- as.Date(steps.filled$date)
weekday <- weekdays(steps.filled$date)
steps.filled <- cbind(steps.filled, weekday)

# Create a factor variable with levels 'weekday' and 'weekend'
# First create a character vector of weekdays
weekdays1 <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')
#Use `%in%` and the weekday vector to create a logical vector
#convert to `factor` and specify the `levels/labels`
wDay <- factor((weekday %in% weekdays1), 
                   levels=c(FALSE, TRUE), labels=c('weekend', 'weekday'))
# Column bind wDay to steps.filled
steps.filled <- cbind(steps.filled, wDay)

# Remove the column mean
steps.filled <- select(steps.filled, -mean)

# Categorize the data frame by 5-minute intervals and wDay
steps.filled <- group_by(steps.filled, interval, wDay)

# Calculate the total number of steps, mean and median over each day
steps.filled <- mutate(steps.filled, average = mean(steps, na.rm = TRUE))

# Perform time series plot of Average Steps per 5-minute interval across all days
qplot(interval, average, data = steps.filled) + 
        facet_grid(.~wDay) +
        labs(x = expression("5-minute intervals")) + 
        labs(y = expression("Average Steps Taken Per Day")) +
        labs(title = "Average Steps Taken Per Day for Each 5-min Interval on Weekends and Weekdays")