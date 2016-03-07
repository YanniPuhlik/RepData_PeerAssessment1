#reading data
data <- read.csv("activity.csv")
# transforming "steps", "interval", "date" variables in easier format
steps <- data$steps
interval <- data$interval
date <- data$date

#creating histogram of the total number of steps taken each day
hist(tapply(steps, date, sum), xlab = "Steps", ylab = "Days", col = "green", breaks = 10, main = "The total number of steps taken each day")

#Calculate and report the mean and median total number of steps taken per day
#creating a framework to calculate the mean or median per day
stepsperday <- as.numeric(tapply(steps, date, sum))
# calculating mean
meanStepsperrday <- mean(stepsperday, na.rm = TRUE)

# mean is [1] 10766.19

# calculating median 
medianStepsperday <- median(stepsperday, na.rm = TRUE)
# median is [1] 10765
# two values are close to one another 

# The average daily activity pattern
# Building the time series plot (i.e. type = "l") of the 5-minute interval (x-axis)
#and the average number of steps taken, averaged across all days (y-axis)
# 1. Changing the interval variable to factor variable
interval <- as.factor(as.character(interval))
#calculating mean
meanInterval <- as.numeric(tapply(steps, interval, mean, na.rm = TRUE))
# building data frame to view data 
dataInterval <- data.frame(dataInterval = as.numeric(levels(interval)), meanInterval)
#ordering inrervals
dataInterval <- dataInterval[order(dataInterval$dataInterval),]

#Building the plot

plot(dataInterval$dataInterval, dataInterval$meanInterval, type = "l", col = "green", main = "Average steps per minute", xlab ="Time of Day", ylab="Mean Steps")

#Each 500 interval on the x-axis equals 5 minutes during the day
#Which 5-minute interval, on average across all the days in the dataset, 
#contains the maximum number of steps?

averInterval <- dataInterval[order(dataInterval$meanInterval,decreasing = TRUE),]
# we need only maximum value, so not to populate with other information 
head(averInterval,n=1)
#  dataInterval meanInterval
# 272          835     206.1698
#the maximum number of steps  in a given interval is 206.2

# Imputing missing values
# 1. Calculating the total number of missing values in the dataset"
missingValues <- sum(is.na(steps))
# We have [1] 2304 missing values in dataset. To conduct analysys we need to replace
# missing values with mean value 37.38

#Replacing the NA values
steps[is.na(steps)] <- 37.38
#Check to see NA values
summary(steps)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.00    0.00    0.00   37.38   37.38  806.00 
# Now we do not have any NA values, so we do not need
# to create new dataset
newdata <- data.frame(steps=steps, date=date, interval = interval)
#a summary of the new data frame
summary(newdata)
#steps                date          interval    
#Min.   :  0.00   2012-10-01:  288   0      :   61  
#1st Qu.:  0.00   2012-10-02:  288   10     :   61  
#Median :  0.00   2012-10-03:  288   100    :   61  
#Mean   : 37.38   2012-10-04:  288   1000   :   61  
#3rd Qu.: 37.38   2012-10-05:  288   1005   :   61  
#Max.   :806.00   2012-10-06:  288   1010   :   61  
#(Other)   :15840   (Other):17202  


#Creating histogram of the total number of steps taken each day
hist(tapply(newdata$steps, newdata$date, sum), xlab="Steps", ylab= "Days", col = "green", breaks=10, main="Steps taken on a Daily basis")

#Calculating the mean and median total number of steps taken per day
newsteps <- as.numeric(tapply(newdata$steps, newdata$date, sum))
meanNewsteps <-  mean(newsteps, na.rm=TRUE) 
# [1] 10766.09
medianNewsteps <- median(newsteps, na.rm=TRUE)
# [1] 10765.44
# The number of steps did not change the mean and median values per day
# On the other hand of you see the histogram, distribution is changed and becamo more pointy

# Are there differences in activity patterns between weekdays and weekends?

# Creating a new factor variable in the dataset with two levels -- "weekday" and "weekend" 
#indicating whether a given date is a weekday or weekend day.

newDATA <- newdata
newDATA$date <- as.Date(newDATA$date)
newDATA$day <- weekdays(newDATA$date) == "Sunday"  | weekdays(newDATA$date) == "Saturday"

newad_new_weekday <- newDATA[newDATA$day == FALSE, ]
newad_new_weekend <- newDATA[newDATA$day == TRUE, ]

#Making a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) 
#and the average number of steps taken, averaged across all weekday days or weekend days (y-axis)

mean_weekday <- tapply(newad_new_weekday$steps, newad_new_weekday$interval, mean)
mean_weekend <- tapply(newad_new_weekend$steps, newad_new_weekend$interval, mean)
#then create two separate plots


plot(mean_weekday, type = "l", xlab="Time interval", ylab="Steps", main="Weekday activity", col="darkblue")
plot(mean_weekend, type="l", xlab="Time interval", ylab="Steps", main="Weekend activity", col="red")

library(knitr)
knit2html("PA1_template.Rmd");browseURL("https://github.com/YanniPuhlik/RepData_PeerAssessment1/blob/master/PA1_template.Rmd")
knit2html("PA1_Template.md", "PA1_Template.html")
setwd("C:/Users/Ianni Puhlik/RepData_PeerAssessment1")
