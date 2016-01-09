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