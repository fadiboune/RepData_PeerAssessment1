# Reproducible Research: Peer Assessment 1
## Introduction
It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the "quantified self" movement - a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

The data for this assignment were downloaded from <https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip> on 05/04/2016 22:00.

The variables included in this dataset are:

* steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)
* date: The date on which the measurement was taken in YYYY-MM-DD format
* interval: Identifier for the 5-minute interval in which measurement was taken

## Loading and preprocessing the data

*load the required library for future plots*

```r
library(ggplot2)
```

first unzipping the file, then reading the data

```r
unzip("activity.zip", files="activity.csv")
actData<- read.csv("activity.csv")
```


## What is mean total number of steps taken per day?
The total number of steps taken per day is calculated below and plotted as an histogram.

```r
sumData<- aggregate(steps ~ date, actData, sum, na.rm=T)
hist(sumData$steps, breaks=50, col="violetred")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)

The mean and median value are:

```r
print(paste0("mean = ",round(mean(sumData$steps)), " steps"))
```

```
## [1] "mean = 10766 steps"
```

```r
print(paste0("mean = ",round(median(sumData$steps)), " steps"))
```

```
## [1] "mean = 10765 steps"
```

## What is the average daily activity pattern?



## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?