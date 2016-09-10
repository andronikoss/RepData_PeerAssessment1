---
title: "Peer Graded Assignment - RepRes"
author: 
date: "5. September 2016"
output: html_document
---


### 1. Loading Data
```{r}
setwd("C:/Users/Ando/Dropbox/Coursera/Reproducible Research/Peer Graded Assignment")
# Loading data
df <- read.csv(file="activity.csv", header=T, sep=",",
							  stringsAsFactors=F)
str(df)
```
```{r}
'data.frame':      17568 obs. of  3 variables:
 $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
 $ date    : chr  "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" ...
 $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```
###2. Histogram of the total number of steps (tns) taken each day  
```{r}
tns <- with(data=df, tapply(steps, date, sum, na.rm=T ))
br <- seq(0, 25000, 1000)
hist(tns, col="steelblue", breaks = br, xlab="Total number of steps",
		 main="Histogram of total number of steps (per day)")
```
![plot of figure01](figure01.png) 

###3. Mean and median number of steps taken each day  
```{r eval=T}
coef <- c(Mean.of.Steps = mean(tns), Median.of.Steps = median(tns))
coef
```


###4. Time series plot of the average number of steps taken  
 
```{r}
tns.avg <- with(data=df, tapply(steps, date, mean, na.rm=T))
plot(as.Date(unique(df$date)), tns.avg, t="l", lwd="3",
		 ylab="Average number of steps", xlab="Date")
points(as.Date(unique(df$date)), tns.avg, pch=19)
```
  
###5. The 5-minute interval that, on average, contains the maximum number of steps  

```{r}
int5min <- with(data=df, tapply(steps, interval, mean, na.rm=T))
max(int5min)
```
### Code to describe and show a strategy for imputing missing data
```{r}
sum(!complete.cases(df))
```
```{r, results='hide'}
library(dplyr)
```
```{r}
ix <- is.na(df$steps)
df.m <- tbl_df(df)
df.m <- df.m %>% 
	group_by(interval) %>%
	mutate(avg = mean(steps, na.rm=T)) 
df.m$steps[ix] <- df.m$avg[ix]

tns.m <- with(data=df.m, tapply(steps, date, sum))
hist(tns.m, col="steelblue", breaks = br, xlab="Total number of steps",
		 main="Histogram of total number of steps (per day)")
coef.m <- c(Mean.of.Steps = mean(tns.m), Median.of.Steps = median(tns.m))
coef.m
```

### Are there differences in activity patterns between weekdays and weekends?
```{r}
wd <-weekdays(as.Date(df$date))
names <- names(table(wd))

dummy <- as.factor(wd == names[6:7])
levels(dummy) <- c("weekday", "weekend")
table(dummy)
df.m <- cbind(df.m, dummy) 
library(lattice)
df.n <- df.m %>%
	group_by(dummy, interval) %>%
	mutate(m = mean(steps))

xyplot(data=df.n, m~interval|dummy, layout=c(1, 2), t="s", 
			 ylab="Number of steps", xlab="Interval")
```
