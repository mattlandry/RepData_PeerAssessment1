library(lattice)
library(knitr)
opts_chunk$set(echo=TRUE,results="show",cache=TRUE)
setwd('C:/Users/mlandry/Documents/GitHub/ReproducibleResearch/RepData_PeerAssessment1')

#Load the CSV into a dataframe for this exercise
Master <- read.table("activity.csv",
                     header=TRUE,
                     sep=",",
                     stringsAsFactors = FALSE,
                     colClasses = c("numeric","Date","numeric")
)
df <- Master


TotalStepsByDay <- aggregate(df$steps,list(date=df$date),sum,na.rm=TRUE)
xAxis = seq(from=0,to=25000,by=2000) 
hist(TotalStepsByDay$x,
     breaks = xAxis,
     main="Total Steps per Day Frequency",
     col="red",
     xlab="Steps",
     ylab="Days",
     xaxt="n")
axis(side=1,at=xAxis,labels=xAxis)


stepMean <- mean(TotalStepsByDay$x,na.rm=T)
print(paste("PART 1: The Mean number of steps per day is",round(stepMean,1)))
stepMedian <- median(TotalStepsByDay$x,na.rm=T)
print(paste("PART 1: The Median number of steps per day is",round(stepMedian,1)))


#Converting time to something usable for this exercise, format adding 0's where needed
intHours <- df$interval %/% 100
intHours <- ifelse(intHours < 10,paste("0",intHours,sep=""),intHours)
intMinutes <- df$interval %% 100
intMinutes <- ifelse(intMinutes < 10,paste("0",intMinutes,sep=""),intMinutes)

#Combine the standardized time data back into a time data format
intTime <- paste(intHours,":",intMinutes,sep="")
intTime <- strptime(intTime,format="%H:%M")
df <- cbind(df,intTime)

#Start plotting the time
SPI <- aggregate(df$steps,list(intTime=df$intTime),mean,na.rm=TRUE)
plot(SPI$intTime,SPI$x,
     type = "l",
     main = "Average Steps per Interval",
     xlab = "Interval",
     ylab = "Average Steps")

#Which interval has the highest average?
MaxStepAvg <- max(SPI$x)
IntervalWithMaxStepAvg <- SPI$intTime[SPI$x == MaxStepAvg]
print(paste("PART 2: The highest average is", MaxStepAvg))
print(paste("PART 2: at this time", IntervalWithMaxStepAvg))

#Step 4
#Part 1
countNAs <- sum(is.na(df$steps))
print(paste("PART 2: Number of missing values =", countNAs))

#Part 2
#Rename column "x"
names(SPI)[names(SPI)=="x"] <- "avgIntervalSteps"

#Bring the average back to the dataframe
dfWithAvg <- merge(x=df,y=SPI,by="intTime",all.x=TRUE)
dfWithAvg <- dfWithAvg[order(dfWithAvg$date,dfWithAvg$intTime),]
dfWithAvg$imputedSteps <- ifelse(is.na(dfWithAvg$steps), 
                                 dfWithAvg$avgIntervalSteps,
                                 dfWithAvg$steps)

TotalStepsByDayImputed <- aggregate(dfWithAvg$imputedSteps,list(date=dfWithAvg$date),sum,na.rm=TRUE)
xAxis = seq(from=0,to=25000,by=2000) 
hist(TotalStepsByDayImputed$x,
     breaks = xAxis,
     main="Frequency of Total Steps (imputed) per Day",
     col="blue",
     xlab="Steps",
     ylab="Days",
     xaxt="n")
axis(side=1,at=xAxis,labels=xAxis)

stepMeanImputed <- mean(TotalStepsByDayImputed$x,na.rm=T)
stepMedianImputed <- median(TotalStepsByDayImputed$x,na.rm=T)
print(paste("PART 3: The Mean number of steps per day is",round(stepMeanImputed,1)))

print(paste("PART 3: The Median number of steps per day is",round(stepMedianImputed,1)))


dfWithAvg$weekday <- weekdays(dfWithAvg$date)
dfWithAvg$weekendFlag <- ifelse(dfWithAvg$weekday=="Saturday" | dfWithAvg$weekday=="Sunday","Weekend","Weekday")

#Find the average steps per day for weekend and weekdays
SPI2 <- aggregate(dfWithAvg$imputedSteps,list(intTime=dfWithAvg$intTime,weekendFlag=dfWithAvg$weekendFlag),mean,na.rm=TRUE)

#Align data for plot
xn <- seq(min(dfWithAvg$intTime),max(dfWithAvg$intTime),by="4 hour")

#Plot
xyplot(x~intTime|weekendFlag,
       data=SPI2,
       type="l",
       layout=c(1,2),
       xlab = "Time Interval (24-hour clock)",
       ylab = "Average Steps",
       main = "Average Steps per Day - Weekend vs Weekday",
       scales=list(
         x=list(
           at=xn,
           labels=format(xn,"%H:%M")
         )
       )
)
