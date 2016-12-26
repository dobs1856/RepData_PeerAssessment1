---
output: pdf_document
---
---
title: "Reproducible Research Course Project 1"
author: "Dobs"
date: "December 26, 2016"
output: html_document

## Loading and preprocessing the data


```r
library(knitr)
opts_chunk$set(echo = TRUE)
setwd("C:/Users/Una/RepData_PeerAssessment1")
unzip(zipfile="activity.zip")
activity_data <- read.csv("activity.csv")
str(activity_data)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```
## What is mean total number of steps taken per day?


```r
library(ggplot2)
library(plyr)
```

```
## Warning: package 'plyr' was built under R version 3.3.2
```

```r
totals <- ddply(activity_data, .(date), summarise, steps_per_date = sum(steps))
summary(totals)
```

```
##          date    steps_per_date 
##  2012-10-01: 1   Min.   :   41  
##  2012-10-02: 1   1st Qu.: 8841  
##  2012-10-03: 1   Median :10765  
##  2012-10-04: 1   Mean   :10766  
##  2012-10-05: 1   3rd Qu.:13294  
##  2012-10-06: 1   Max.   :21194  
##  (Other)   :55   NA's   :8
```

```r
hist(totals$steps_per_date, main = "Summary Activity Report", col="gray", xlab="Total Num of Steps taken each day")
```

![](PA1_template_files/figure-latex/unnamed-chunk-1-1.pdf)<!-- --> 

```r
activity_mean <-mean(totals$steps_per_date, na.rm=TRUE)
activity_median <- median(totals$steps_per_date, na.rm=TRUE)
activity_mean
```

```
## [1] 10766.19
```

```r
activity_median
```

```
## [1] 10765
```

## What is the average daily activity pattern?

```r
avg_steps_per_interval <- aggregate(steps ~ interval, activity_data, FUN = mean, na.rm = TRUE)

plot(avg_steps_per_interval$interval,avg_steps_per_interval$steps, type="l", xlab="Total 5 min Interval", ylab="Average num of steps",main="Average Daily Activity Pattern")
```

![](PA1_template_files/figure-latex/unnamed-chunk-2-1.pdf)<!-- --> 

```r
max_interval <- avg_steps_per_interval[which.max(avg_steps_per_interval$steps),]
max_interval
```

```
##     interval    steps
## 104      835 206.1698
```
## Imputing missing values

```r
colSums_na <-colSums(is.na(activity_data))
colSums_na
```

```
##    steps     date interval 
##     2304        0        0
```

```r
get_steps_value <- function(steps, interval) {
     if (is.na(steps))
         new_steps_value <- (avg_steps_per_interval[avg_steps_per_interval$interval==interval, "steps"])
     else
         new_steps_value <- c(steps)
     return(new_steps_value)
 }

activity_data_enrich <- activity_data

activity_data_enrich$steps <- mapply(get_steps_value, activity_data_enrich$steps, activity_data_enrich$interval)


totals.enrich <- ddply(activity_data_enrich, .(date), summarise, steps_per_date = sum(steps))
totals.enrich$date_dt = as.factor(as.Date(totals.enrich$date))

head(totals.enrich)
```

```
##         date steps_per_date    date_dt
## 1 2012-10-01       10766.19 2012-10-01
## 2 2012-10-02         126.00 2012-10-02
## 3 2012-10-03       11352.00 2012-10-03
## 4 2012-10-04       12116.00 2012-10-04
## 5 2012-10-05       13294.00 2012-10-05
## 6 2012-10-06       15420.00 2012-10-06
```

```r
hist(totals.enrich$steps_per_date, main = "Summary Activity Report", col="green", xlab="Total Num of Steps taken each day")
```

![](PA1_template_files/figure-latex/unnamed-chunk-3-1.pdf)<!-- --> 

```r
activity_enr_mean <-mean(totals.enrich$steps_per_date, na.rm=TRUE)
activity_enr_median <- median(totals.enrich$steps_per_date, na.rm=TRUE)

activity_enr_mean
```

```
## [1] 10766.19
```

```r
activity_enr_median
```

```
## [1] 10766.19
```
What is the impact of imputing missing data on the estimates of the total daily number of steps?
-> the mean and the median are equal

## Are there differences in activity patterns between weekdays and weekends?

```r
weekdays.str <- c("Mon", "Tue", "Wed", "Thu", "Fri")

totals.enrich$weekend_ind = as.factor(ifelse(is.element(weekdays(as.Date(totals.enrich$date), abbr = TRUE ),weekdays.str), "0", "1"))

activity_data_enrich$weekend_ind = as.factor(ifelse(is.element(weekdays(as.Date(activity_data_enrich$date), abbr = TRUE ),weekdays.str), "0", "1"))

avg_steps_per_interval.enrich <- aggregate(steps ~ interval + weekend_ind, activity_data_enrich, FUN = mean, na.rm = TRUE)

avg_steps_per_interval.enrich_0 <- subset(avg_steps_per_interval.enrich, weekend_ind == '0')
avg_steps_per_interval.enrich_1 <- subset(avg_steps_per_interval.enrich, weekend_ind == '1')

par(mfrow=c(1,2))
plot(avg_steps_per_interval.enrich_0$interval,avg_steps_per_interval.enrich_0$steps, type="l", xlab="Total 5 min Interval", ylab="Average num of steps",main="Daily Activity Weekdays")

plot(avg_steps_per_interval.enrich_1$interval,avg_steps_per_interval.enrich_1$steps, type="l", xlab="Total 5 min Interval", ylab="Average num of steps",main="Daily Activity Weekends")
```

![](PA1_template_files/figure-latex/unnamed-chunk-4-1.pdf)<!-- --> 


## People in average are more active on weekends than on weekdays


