
# Reproducible Research: Peer Assessment 1



## Loading and preprocessing the data
##### 1. Load the data 

```r
if(!file.exists('activity.csv')){
    unzip('activity.zip')
}
data <- read.csv('activity.csv')
```

-----

## What is mean total number of steps taken per day?

```r
stepsPerDay <- tapply(data$steps, data$date, sum, na.rm=TRUE)
```

##### 1. Make a histogram of the total number of steps taken each day

```r
qplot(stepsPerDay, xlab='Total steps per day', ylab='Frequency using binwith 500', binwidth=500)
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png) 

##### 2. Calculate the mean and median 

```r
stepsPerDayMean <- mean(stepsPerDay)
stepsPerDayMedian <- median(stepsPerDay)
```
* Mean: 9354.2295082
* Median:  10395

-----

## What is the average daily activity pattern?

```r
averageStepsPerTimeBlock <- aggregate(x=list(meanSteps=data$steps), by=list(interval=data$interval), FUN=mean, na.rm=TRUE)
```

##### 1. Make a time series plot

```r
ggplot(data=averageStepsPerTimeBlock, aes(x=interval, y=meanSteps)) +
    geom_line() +
    xlab("5-minute interval") +
    ylab("Average number of steps taken") 
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png) 

##### 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
mostSteps <- which.max(averageStepsPerTimeBlock$meanSteps)
timeMostSteps <-  gsub("([0-9]{1,2})([0-9]{2})", "\\1:\\2", averageStepsPerTimeBlock[mostSteps,'interval'])
```

* Most Steps at: 8:35

----

## Imputing missing values
##### 1. Calculate and report the total number of missing values in the dataset 

```r
numMissingValues <- length(which(is.na(data$steps)))
```

* Number of missing values: 2304


##### 3. Create a new dataset.


```r
dataImputed <- data
dataImputed$steps <- impute(data$steps, fun=mean)
```


##### 4. Make a histogram of the total number of steps taken each day 

```r
stepsPerDayImputed <- tapply(dataImputed$steps, dataImputed$date, sum)
qplot(stepsPerDayImputed, xlab='Total steps per day (Imputed)', ylab='Frequency using binwith 500', binwidth=500)
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png) 

##### ... and Calculate and report the mean and median total number of steps taken per day. 

```r
stepsPerDayMeanImputed <- mean(stepsPerDayImputed)
stepsPerDayMedianImputed <- median(stepsPerDayImputed)
```
* Mean (Imputed): 1.0766189\times 10^{4}
* Median (Imputed):  1.0766189\times 10^{4}


----

## Are there differences in activity patterns between weekdays and weekends?




```r
dataImputed$dateType <-  ifelse(as.POSIXlt(dataImputed$date)$wday %in% c(0,6), 'weekend', 'weekday')
```

##### 2. Make a panel plot containing a time series plot


```r
averagedDataImputed <- aggregate(steps ~ interval + dateType, data=dataImputed, mean)
ggplot(averagedDataImputed, aes(interval, steps)) + 
    geom_line() + 
    facet_grid(dateType ~ .) +
    xlab("5-minute interval") + 
    ylab("avarage number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-14-1.png) 

