---
title: "Assignment 1"
author: "Hein"
date: "2026-06-28"
output: 
        html_document:
                keep_md: true
---



# Loading and preprocessing the data


``` r
if (!dir.exists("data")) {
        dir.create("data")
}

url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
zip_path <- "data/activity.zip"

download.file(url,
              destfile = zip_path,
              method = "curl")

unzip(zip_path,
      exdir = "data")

data <- read.csv("data/activity.csv")
data$date <- as.Date(data$date)

steps <- aggregate(steps ~ date,
                   data,
                   sum,
                   na.rm = TRUE)
head(steps)
```

```
##         date steps
## 1 2012-10-02   126
## 2 2012-10-03 11352
## 3 2012-10-04 12116
## 4 2012-10-05 13294
## 5 2012-10-06 15420
## 6 2012-10-07 11015
```

# What is mean total number of steps taken per day?
## Calculate the total number of steps taken per day.


``` r
steps <- aggregate(steps ~ date,
                   data,
                   sum,
                   na.rm = TRUE)
head(steps)
```

```
##         date steps
## 1 2012-10-02   126
## 2 2012-10-03 11352
## 3 2012-10-04 12116
## 4 2012-10-05 13294
## 5 2012-10-06 15420
## 6 2012-10-07 11015
```

## Make a histogram of the total number of steps taken each day.


``` r
hist(steps$steps, 
     main = "Histogram of Total Steps Per Day",
     xlab = "Number of Steps", 
     col = "wheat")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

## Calculate and report the mean and median of the total number of steps taken per day.

``` r
mean_steps <- mean(steps$steps)
median_steps <- median(steps$steps)
```
mean of the total number of steps taken per day is **1.0766189\times 10^{4}**.
median of the total number of steps taken per day is **10765**.
# What is the average daily activity pattern?


## Make a time series plot (i.e. type = "l"type ) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


``` r
intervalSteps <- aggregate (steps ~ interval,
                            data,
                            mean,
                            na.rm = TRUE)
head(intervalSteps)
```

```
##   interval     steps
## 1        0 1.7169811
## 2        5 0.3396226
## 3       10 0.1320755
## 4       15 0.1509434
## 5       20 0.0754717
## 6       25 2.0943396
```

``` r
library(ggplot2)
ggplot(intervalSteps,
       aes(x = interval, y = steps)) +
        geom_line() +
        theme_bw() +
        labs(
                title = "Average Daily Activity Pattern",
                x = "5-minute interval",
                y = "The average number of steps taken across all days"
        )
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

## Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

``` r
maxInterval <- intervalSteps[which.max(intervalSteps$steps),]$interval
maxNumSteps <- intervalSteps[which.max(intervalSteps$steps),]$steps
```
5-minute interval **835** contains **206.1698113** as the maximum number steps across all the days in the dataset,.

# Imputing missing values

## Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NA)

``` r
colSums(is.na(data))
```

```
##    steps     date interval 
##     2304        0        0
```

``` r
rows_NAs <- colSums(is.na(data))[["steps"]]
```
The total number of rows with NA is **2304**.

## Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc. [used - the mean for that 5-minute interval]
## Create a new dataset that is equal to the original dataset but with the missing data filled in.

``` r
imputed_data <- data

imputed_data$steps <- ifelse(is.na(imputed_data$steps), 
                             intervalSteps$steps[match(imputed_data$interval, intervalSteps$interval)], 
                             imputed_data$steps)
```

## Make a histogram of the total number of steps taken each day.

``` r
imputed_steps <- aggregate(steps ~ date,
                           imputed_data,
                           sum)
hist(imputed_steps$steps, 
     main = "Histogram of Total Steps Per Day",
     xlab = "Number of Steps", 
     col = "wheat")
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png)<!-- -->
## Calculate and report the mean and median total number of steps taken per day.

``` r
mean_imsteps <- mean(imputed_steps$steps)
median_imsteps <- median(imputed_steps$steps)
```
## Do these values differ from the estimates from the first part of the assignment?

``` r
# Put your existing variables into a simple comparison grid
data.frame(
        Original   = c(mean_steps, median_steps),
        Imputed    = c(mean_imsteps, median_imsteps),
        Difference = c(mean_imsteps - mean_steps, median_imsteps - median_steps),
        row.names  = c("Mean", "Median")
)
```

```
##        Original  Imputed Difference
## Mean   10766.19 10766.19   0.000000
## Median 10765.00 10766.19   1.188679
```

## What is the impact of imputing missing data on the estimates of the total daily number of steps?

the impact of imputing missing data on the estimates of the total daily number of steps is negligible.

# Are there differences in activity patterns between weekdays and weekends?

## Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

``` r
imputed_data$date <- as.Date(imputed_data$date)
days <- weekdays(imputed_data$date)
imputed_data$weekdays_end <- ifelse(days %in% c("Saturday", "Sunday"),
                                "weekend",
                                "weekday")
imputed_data$weekdays_end <- as.factor(imputed_data$weekdays_end)
table(imputed_data$weekdays_end)
```

```
## 
## weekday weekend 
##   12960    4608
```

## Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

``` r
week_de_average <- aggregate(steps ~ interval + weekdays_end,
                             imputed_data,
                             mean)
library(lattice)
xyplot(steps ~ interval | weekdays_end,
       week_de_average,
       type = "l",
       layout = c(1,2),
       xlab = "5-Minute Interval",
       ylab = "Average number of Steps",
       main = "Weekday vs. Weekend Patterns")
```

![](PA1_template_files/figure-html/unnamed-chunk-13-1.png)<!-- -->








