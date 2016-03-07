#Course 5 March 2016
# Project 1
#PA1_template.R
library(data.table)
setwd("/Users/Murali/Documents/Coursera/Course 5/Week 1")
#path=getwd()
# downloading data zip file - already done
# listing all files in the zip folder
unzip("repdata-data-activity.zip",list=T)

memory.size()
# unzipped the files manually in the .zip file 
# initially all dates, time and other variables read as factors
# instructions say missing values coded as ?

activity <- read.csv("activity.csv", sep=",", header=T)
names(activity)
dim(activity)
str(activity)
class(activity$date)

# converting factor to date
activity$date <- as.Date(activity$date, "%Y-%m-%d")
str(activity)

#1
library(dplyr)
activity_date <- group_by(activity, date)
dim(activity_date)

#1a
# total number of steps per day
activity_steps_day <- summarize(activity_date, sum=sum(steps, na.rm=TRUE))
activity_steps_day

#1b
library(ggplot2)
qplot(sum, data=activity_steps_day)

#1c
mean_steps <- mean(activity_steps_day$sum)
mean_steps
median_steps <- median(activity_steps_day$sum)
median_steps

#2 
activity_interval <- group_by(activity, interval)
dim(activity_interval)

#2a
# average number of steps per interval
activity_steps_interval <- summarize(activity_interval, average=mean(steps, na.rm=TRUE))
activity_steps_interval
dim(activity_steps_interval)
summary(activity_steps_interval$average)

# g <- qplot(interval, average, data=activity_steps_interval, geom="line")
# g
# switching back to base plot

plot(activity_steps_interval$interval, activity_steps_interval$average, data = activity_steps_interval, xlab="interval", ylab="average steps taken", type="n")
title("average number of steps taken daily over a given interval")
lines(activity_steps_interval$interval, activity_steps_interval$average)
abline(h=max(activity_steps_interval$average), col="red")
#2b 
# interval with maximum number of steps on average

#3 dealing with missing values

#3a
sum(is.na(activity$steps))   
#2,304 missing values for steps
#sum(is.na(activity$date))
#sum(is.na(activity$interval))
# better idea
colSums(is.na(activity))

#3c
activity_new <- merge(activity, activity_steps_interval, by.activity=interval,
by.activity_steps_interval=interval)
dim(activity_new)
head(activity_new, 100)

activity_new <- activity_new %>%
      mutate(steps = ifelse(is.na(steps),average,steps)) 
activity_new <- activity_new[ , 1:3]
dim(activity_new)
head(activity_new, 61)

#3d 
plot(activity_steps_interval$interval, activity_steps_interval$average, data = activity_steps_interval, xlab="interval", ylab="average steps taken", type="n")
title("average number of steps taken daily over a given interval")
lines(activity_steps_interval$interval, activity_steps_interval$average)











SCC <- readRDS("Source_Classification_Code.rds")

dim(SCC)   # row = 11,717 column = 25
head(SCC)
str(SCC)
SCC$SCC <- as.character(SCC$SCC)   # SCC variable in SCC data frame is Factor, in NEI is character
str(SCC)

dim(NEI)   # row = 6,497,651 column = 6
head(NEI)
names(NEI)
str(NEI)

# plot 1
par(mfrow=c(1,1))
#emissions_year <- as.numeric(unlist(with(NEI, tapply(Emissions, year, sum))))  # gets rid of year in name
#emissions_year  # without the as .numeric were getting the sum with name attached
#str(emissions_year)
#names(emissions_year)
#Year <- unique(NEI$year)
#Year
#emissions_plot1 <- data.frame(cbind(Year, emissions_year))  # first set it up as a data frame


# dplyr is a better method
library(dplyr)
NEI_year <- group_by(NEI, year)
dim(NEI_year)
emissions_year <- summarize(NEI_year, emis_yr = sum(Emissions))
emissions_year

with(emissions_year, plot(year, emis_yr, xlab= "Year", ylab="Total Emissions", 
main = "Total PM2.5 emissions in US by year", pch=19))
with(emissions_year, lines(year, emis_yr))
dev.copy(png, file="Plot1.png")
dev.off()



