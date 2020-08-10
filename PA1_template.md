---
title: 'Reproducible Research: Peer Assessment 1'
output:
  html_document:
    keep_md: yes
  word_document: default
---


## Loading and preprocessing the data

```r
actdata<-read.csv("C:/Users/KALYANI/Desktop/R Projects/RepData_PeerAssessment1/activity.csv")
summary(actdata)
```

```
##      steps                date          interval     
##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
##  Median :  0.00   2012-10-03:  288   Median :1177.5  
##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
##  NA's   :2304     (Other)   :15840
```

```r
str(actdata)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```
## What is mean total number of steps taken per day?

```r
sumsteps<-tapply(actdata$steps,actdata$date,sum,na.rm=TRUE)
png(file="A1plot1.png")
hist(sumsteps,col="red")
dev.off()
```

```
## png 
##   2
```

```r
mean(sumsteps)
```

```
## [1] 9354.23
```

```r
median(sumsteps)
```

```
## [1] 10395
```

## What is the average daily activity pattern?

```r
actdata$avgsteps<-tapply(actdata$steps, actdata$interval,mean,na.rm=TRUE)
library(lattice)
png(file="A1plot2.png")
xyplot(actdata$avgsteps~actdata$interval,data=actdata,col="blue",type="l",xlab="interval",ylab="No.of steps",plot.points=FALSE,main="Average steps Time series")
dev.off()
```

```
## png 
##   2
```
##the Time-Interval 830-835, on average across all the days in the dataset, contains the maximum number of steps. 

## Imputing missing values

```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
impdata<-actdata%>%group_by(interval)%>%mutate(impsteps=ifelse(is.na(steps),mean(steps,na.rm = TRUE),steps))
impsumsteps<-tapply(impdata$impsteps,impdata$date,sum,na.rm=TRUE)
png(file="A1plot3.png")
hist(impsumsteps,col="blue")
dev.off()
```

```
## png 
##   2
```

```r
mean(impsumsteps)
```

```
## [1] 10766.19
```

```r
median(impsumsteps)
```

```
## [1] 10766.19
```

```r
png(file="A1plot4.png")
par(mfrow=c(1,2))
hist(sumsteps,col="red")
hist(impsumsteps,col="blue")
dev.off()
```

```
## png 
##   2
```


## Are there differences in activity patterns between weekdays and weekends?

```r
impdata<-mutate(impdata,wdays=weekdays(as.Date(date)))
impdata$wdays<-factor(impdata$wdays,levels=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))
levels(impdata$wdays)<-c(rep("Weekday",5),rep("Weekend",2))

library(ggplot2)
png(file="A1plot5.png")
xyplot(impdata$steps~impdata$interval|factor(wdays),data=impdata,col="blue",type="l",layout=c(1,2),xlab="interval",ylab="No.of steps",plot.points=FALSE)
dev.off()
```

```
## png 
##   2
```
