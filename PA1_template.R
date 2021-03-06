#Course 5 March 2016
# Project 1
#PA1_template.R
#library(data.table)
setwd("/Users/Murali/Documents/Coursera/Course5/Week1/RepData_PeerAssessment1/")

# downloading data zip file - already done

memory.size()
# unzipped the files manually in the .zip file 

#1
#1a 
activity <- read.csv("activity.csv", sep=",", header=T)
names(activity)
dim(activity)
str(activity)
class(activity$date)

#1b
# converting date factor to date
activity$date <- as.Date(activity$date, "%Y-%m-%d")
str(activity)

#2
library(dplyr)
activity_date <- group_by(activity, date)
dim(activity_date)

#2a
# total number of steps per day - IGNORING MISSING STEPS INFO
activity_steps_day <- summarize(activity_date, sum=sum(steps, na.rm=TRUE))
activity_steps_day

#2b
library(ggplot2)
qplot(sum, data=activity_steps_day)

#2c
summary(activity_steps_day$sum)
# mean steps per day  = 9,354
# median steps per day = 10,395

#3 
activity_interval <- group_by(activity, interval)
dim(activity_interval)

#3a
# average number of steps per interval
activity_steps_interval <- summarize(activity_interval, average=mean(steps, na.rm=TRUE))
activity_steps_interval
dim(activity_steps_interval)
summary(activity_steps_interval$average)

plot(activity_steps_interval$interval, activity_steps_interval$average, data = activity_steps_interval, xlab="interval", ylab="average steps taken", type="n")
title("average number of steps taken daily over a given interval")
lines(activity_steps_interval$interval, activity_steps_interval$average)
# abline(h=max(activity_steps_interval$average), col="red")

#3b 
# interval with maximum number of steps on average

filter(activity_steps_interval, average==max(average)) # average = 206.1698, interval = 835

#4 dealing with missing values

#4a
colSums(is.na(activity)) 
#2,304 missing values for steps, date and interval columns had no missing values

#4b impute steps=NA with average steps for that interval
#4c, there are 288 intervals
activity_new <- merge(activity, activity_steps_interval, by.activity=interval,
by.activity_steps_interval=interval)
dim(activity_new)
#head(activity_new, 100)

activity_new <- activity_new %>%
      mutate(steps = ifelse(is.na(steps),average,steps)) # replace steps only if NA
activity_new <- activity_new[ , 1:3]
dim(activity_new)
#head(activity_new, 61)

#4d
activity_date_new <- group_by(activity_new, date)
dim(activity_date_new)
# total number of steps per day - IMPUTING MISSING STEPS INFO
activity_steps_day_new <- summarize(activity_date_new, sum=sum(steps))
activity_steps_day_new

hist(activity_steps_day_new$sum, xlab="Total steps")
title("Total number of steps per day - imputing missing steps data")

summary(activity_steps_day_new$sum)
# mean steps per day  = 10,770 on imputing increased from 9,354
# median steps per day = 10,770 on imputing increased from 10,395
# both mean and median increased. mean increased by more

#5 
#5a
activity_new <- mutate(activity_new, day_type=factor(1*(weekdays(date) %in% c("Saturday","Sunday")), 
labels=c("Weekday","Weekend")))
dim(activity_new)
head(activity_new)
summary(activity_new$day_type)

#5b
activity_new_day_type <- group_by(activity_new, day_type, interval)
activity_new_steps <- summarize(activity_new_day_type, average=mean(steps))
dim(activity_new_steps)
head(activity_new_steps)

library(lattice)
xyplot(average~interval | day_type , data = activity_new_steps, geom="lines")










