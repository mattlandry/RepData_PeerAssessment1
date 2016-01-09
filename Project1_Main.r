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
