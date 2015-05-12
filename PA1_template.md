# Reproducible Research: Peer Assessment 1

## Set default options and load required libraries

```r
suppressWarnings({
    require(knitr); # needed for opts_chunk
    require(stringr);
    require(lubridate);
    require(dplyr);
    require(ggplot2);
    require(xtable);
})
```

```
## Loading required package: knitr
## Loading required package: stringr
## Loading required package: lubridate
## Loading required package: dplyr
## 
## Attaching package: 'dplyr'
## 
## The following objects are masked from 'package:lubridate':
## 
##     intersect, setdiff, union
## 
## The following object is masked from 'package:stats':
## 
##     filter
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
## 
## Loading required package: ggplot2
## Loading required package: xtable
```

```r
opts_chunk$set(echo=T)
```

## Loading and preprocessing the data

```r
activities <- read.csv(unz("activity.zip", "activity.csv"))
str(activities)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
# Use dplyr to mutate the dataset to add a datetime field combining "date"
# and "interval"

## First create a character vector of dates
dates <- as.character(activities$date)

## Now create a character vector build from "interval" in the "hhmmss" format
## stringr::str_sub() is used for simpler RIGHT substrings
times <- paste0(str_sub(paste0("0000", activities$interval), -4), "00")

## Use dplyr::mutate() and lubridate::ymd_hms() to create a "datetime"" field,
## and make "interval" a factor
activities <- activities %>%
    mutate(datetime = ymd_hms(paste(dates, times, sep=" "))) %>%
    mutate(interval = as.factor(interval))

str(activities)
```

```
## 'data.frame':	17568 obs. of  4 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: Factor w/ 288 levels "0","5","10","15",..: 1 2 3 4 5 6 7 8 9 10 ...
##  $ datetime: POSIXct, format: "2012-10-01 00:00:00" "2012-10-01 00:05:00" ...
```

## What is mean total number of steps taken per day?

```r
# Calculate daily total steps using dplyr
dailySteps <- activities %>%
    group_by(date) %>%
    summarize(ttl.steps=sum(steps, na.rm=T))

# Show in a histogram
ggplot(data=dailySteps, aes(dailySteps$ttl.steps)) +
    geom_histogram(fill=rgb(31, 119, 180, maxColorValue=255),
                   col="white") +
    labs(title="Histogram of Total Daily Steps") +
    labs(x="Total Steps", y="Count of Days") +
    theme_bw()
```

![](PA1_template_files/figure-html/dailysteps-1.png) 

```r
# Calculate mean and median of total daily steps and present in a small table
meanSteps <- mean(dailySteps$ttl.steps)
medianSteps <- median(dailySteps$ttl.steps)
stepsSummary <- data.frame(cbind(meanSteps, medianSteps))
names(stepsSummary) <- c("Mean of Ttl Daily Steps",
                         "Median of Ttl Daily Steps")

print(xtable(stepsSummary), type="html", include.rownames=F)
```

<!-- html table generated in R 3.1.2 by xtable 1.7-4 package -->
<!-- Mon May 11 21:14:35 2015 -->
<table border=1>
<tr> <th> Mean of Ttl Daily Steps </th> <th> Median of Ttl Daily Steps </th>  </tr>
  <tr> <td align="right"> 9354.23 </td> <td align="right"> 10395.00 </td> </tr>
   </table>

## What is the average daily activity pattern?



## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
