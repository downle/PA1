#Load the data

activity <- read.csv("~/PA1_QIAN/activity.csv")

table(is.na(activity$steps))

head(is.na(activity$steps))

#mean by date
aggregate( formula = date~steps, 
           data = activity,
           FUN = mean)

mean(activity$steps,na.rm = TRUE)
tapply(activity$steps,activity$date,mean)

aggregate(activity$steps, by=list(activity$date), FUN=mean,na.rm=TRUE)


median_daily<-aggregate(activity$steps, by=list(activity$date), FUN=median) 
head(median_daily)

step_mean<-aggregate( formula = steps~interval, 
           data = activity,
           FUN = mean)

library("lattice")
xyplot(steps~interval,
       data = step_mean,
       type = "l")
