---
title: "Reproducible Research: Peer Assessment 1"
===============================================================================
output: 
  html_document:
    keep_md: true
---

## Loading and preprocessing the data

### Load the data
#```{r echo=TRUE}
setwd("E:/Rt/Peer1/")
AMD = read.csv("Activity.csv", stringsAsFactors=FALSE)
#```

### Preprodess/transform the data
#```{r echo=TRUE}
uDate = unique(AMD[,"date"])
uInterval = unique(AMD[,"interval"])  # 288
nDate = length(uDate) # 61

Lens = vector(length=nDate)
nInterval = 60*24/5 # 60 minutes per hour * 24 hours / 5 minutes interval
for (i in 1:nDate) Lens[i] = length(AMD[AMD[,"date"]==uDate[i],"steps"])
if (sum(Lens != nInterval) != 0) print("Uneven measurements per day")
#```

## What is mean total number of steps taken per day?
### Histogram of Steps without Imputation
#```{r echo=TRUE}
TotSteps = aggregate(steps ~ date, data=AMD, sum, na.rm=TRUE)
hist(TotSteps$steps, main="Histogram of Steps without Imputation")
#```
### Mean and Median without Imputation
#```{r echo=TRUE}
mean(TotSteps$steps)  # 10766.19
median(TotSteps$steps) # 10765
#```

## What is the average daily activity pattern?
#```{r echo=TRUE}
AvgSteps = aggregate(steps ~ interval, data=AMD, mean, na.rm=TRUE)
plot(uInterval, AvgSteps$steps, type="l", ylab="Average Steps")
#```

### Which 5 min interval contains the maximum number?
#```{r echo=TRUE}
which.max(AvgSteps$steps)  # 104th inteval
AMD[which.max(AvgSteps$steps),"interval"] # [835, 840) interval
#```

## Imputing missing values
### total number of missing values
#```{r echo=TRUE}
sum(is.na(AMD[,"steps"])) # 2304
#```

### Impute NA with average steps in that interval (AvgSteps)
#```{r echo=TRUE}
mAMD = matrix(AMD[,"steps"], nrow=nInterval, ncol=nDate)
for (i in 1:nInterval) {
  for (j in 1:nDate) {
    if (is.na(mAMD[i,j])) mAMD[i,j] = AvgSteps[i,"steps"]
  }
}

AMD2 = AMD
AMD2[,"steps.im"] = as.vector(mAMD)
#```

### Histogram of Steps with Imputation
#```{r echo=TRUE}
TotSteps2 = aggregate(steps.im ~ date, data=AMD2, sum, na.rm=TRUE)
hist(TotSteps2$steps, main="Histogram of Steps with Imputation")
#```

### mean and median of total steps in days
#```{r echo=TRUE}
mean(TotSteps2$steps) # 10766.19
median(TotSteps2$steps) # 10766.19
#```


## Are there differences in activity patterns between weekdays and weekends?
#```{r echo=TRUE}
weekday.no = (julian(as.Date(AMD2[,"date"])) - 3) %% 7  #0:6 is Sun:Sat
AMD2[,"wday"] = "weekday"
AMD2[weekday.no==0 | weekday.no==6, "wday"] = "weekend"
AMD2[,"wday"] = factor(AMD2[,"wday"])

Steps2 = aggregate(steps ~ interval + wday, AMD2, mean)
library(lattice)
xyplot(steps ~ interval | factor(wday), data=Steps2, aspect=1/2, type="l")
#```

