# Reproducible Research: Peer Assessment 1




## Loading and preprocessing the data


```r
#create temporary directory and placeholder file
td <- tempdir() 
tf <- tempfile(tmpdir=td, fileext=".zip")  

#download file to temporary file
url <- "http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(url, tf)

#get the name of the zip file and unzip
fname <- unzip(tf, list=TRUE)$Name[1]
unzip(tf, files=fname, exdir=td, overwrite=TRUE)

#get the full path to the extracted file
fpath = file.path(td, fname)

#read file into data frame
activity <- read.table(fpath, header=TRUE, sep=",", row.names=NULL)

#covert date column to date format from factor format
activity$date <- as.Date(activity$date, format = "%Y-%m-%d")
```



## What is mean total number of steps taken per day?

#### Record the total steps per day in the variable steps.per.day

```r
steps.per.day <- tapply(activity$steps, activity$date, sum, na.rm=T)
```

#### Make a histogram of the total steps per day

```r
hist(steps.per.day)
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 


```r
daily.steps.mean <- round(mean(steps.per.day))
daily.steps.median <- median(steps.per.day)
```

The mean total number of steps taken per day is 9354.  
The median total number of steps taken per day is 10395.

## What is the average daily activity pattern?
#### Make a line plot of the daily average steps for each time interval

```r
interval.mean <- aggregate(steps ~ interval, activity, mean, na.rm=T)
plot(interval.mean$interval, interval.mean$steps, type="l", 
     main = "Mean Number Of Steps For Each Interval", 
     xlab = "Interval", 
     ylab = "Mean Number Of Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 



```r
interval.max <- interval.mean$interval[which.max(interval.mean$steps)]
```
Interval number 835 had the highest daily average number of steps.



## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
