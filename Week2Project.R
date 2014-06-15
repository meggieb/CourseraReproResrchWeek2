data<-read.csv("activity.csv", header=T)
summary(data$interval)

#Make a histogram of the total number of steps taken each day
datam<-as.matrix(tapply(data$steps,data$date,mean))
barplot(t(datam))

#Calculate and report the mean and median total number of steps taken per day
datam<-as.data.frame(tapply(data$steps,data$date,mean,na.rm=TRUE))
head(datam)
datam<-cbind(row.names(datam),datam)
names(datam)<-c("Date","Mean")
row.names(datam)<-NULL

datamed<-as.data.frame(tapply(data$steps,data$date,median,na.rm=TRUE))
datamed<-cbind(row.names(datamed),datamed)
dim(datamed)
head(datamed)
names(datamed)<-c("Date","Median")
row.names(datamed)<-NULL
datamerge<-merge(datam,datamed,by="Date")

#Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
dataMInterval<-as.data.frame(tapply(data$steps,data$interval,mean,na.rm=TRUE))
dim(dataMInterval)
dataMInterval<-cbind(row.names(dataMInterval),dataMInterval)
names(dataMInterval)<-c("Interval","Steps")
dataMInterval
row.names(dataMInterval)<-NULL
dataMInterval$Interval<-as.numeric(as.character(dataMInterval$Interval))
dataMInterval$Interval
dataMInterval$Steps
plot(dataMInterval$Interval,dataMInterval$Steps, type="l")


#Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
dataMIntervalSort <- dataMInterval[order(-dataMInterval$Steps),]
maxValue<-which.max(dataMInterval$Steps)
maxInterval<-dataMInterval[maxValue,1]
#835

#Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
summary(data)

#Devise a strategy for filling in all of the missing values in the dataset. 
#The strategy does not need to be sophisticated. For example, you could use the 
#mean/median for that day, or the mean for that 5-minute interval, etc.
summary(dataNoNas)  #2304

#Fill in the 5-minute interval 
#Find all the rows with NA in the dataset, then find all their interval values
NaValues<-complete.cases(data)


#Create a new dataset that is equal to the original dataset but with the missing data filled in.
NaValuesRows<-grep("FALSE", NaValues)
dataNoNas<-data
for(i in 1:2304)
{
  p<-NaValuesRows[i]
  dataNoNas[p,1]<- dataMInterval$Steps[match(dataNoNas[p,3],dataMInterval$Interval)]
  ++i
}


#Make a histogram of the total number of steps taken each day and 
#Do these values differ from the estimates from the first part of the assignment? 
#What is the impact of imputing missing data on the estimates of the total daily number of steps?
datamNoNas<-as.matrix(tapply(dataNoNas$steps,dataNoNas$date,mean))
barplot(t(datamNoNas))

#Calculate and report the mean and median total number of steps taken per day. 
datamNoNas<-as.data.frame(tapply(dataNoNas$steps,dataNoNas$date,mean,na.rm=TRUE))
datamNoNas<-cbind(row.names(datamNoNas),datamNoNas)
names(datamNoNas)<-c("Date","Mean")
row.names(datamNoNas)<-NULL

datamedNoNas<-as.data.frame(tapply(dataNoNas$steps,dataNoNas$date,median,na.rm=TRUE))
datamedNoNas<-cbind(row.names(datamedNoNas),datamedNoNas)
names(datamedNoNas)<-c("Date","Median")
row.names(datamedNoNas)<-NULL
datamergeNoNas<-merge(datamNoNas,datamedNoNas,by="Date")

#Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
dataNoNasdays<-dataNoNas
head(dataNoNas)
dataNoNasdays$Weekday<-weekdays(as.Date(dataNoNasdays$date,"%Y-%m-%d"))

#Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute 
#interval (x-axis) and the average number of steps taken, averaged across all weekday days 
#or weekend days (y-axis). The plot should look something like the following, which was 
#creating using simulated data:

dataNoNasWeekend<-dataNoNasdays[(dataNoNasdays$Weekday=="Saturday"|dataNoNasdays$Weekday=="Sunday"),]
dataMInterval<-as.data.frame(tapply(data$steps,data$interval,mean,na.rm=TRUE))
dim(dataMInterval)
dataMInterval<-cbind(row.names(dataMInterval),dataMInterval)
names(dataMInterval)<-c("Interval","Steps")
dataMInterval
row.names(dataMInterval)<-NULL
dataMInterval$Interval<-as.numeric(as.character(dataMInterval$Interval))
dataMInterval$Interval
dataMInterval$Steps
plot(dataMInterval$Interval,dataMInterval$Steps, type="l")


#mean number of steps taken per day
hist(data$steps)

par(mfrow=c(1,1))
barplot(data$date, data$steps, type="l")

head(data)
library(reshape2)
mdata<-melt(data,id=date)
