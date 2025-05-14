##
## US Bureau of Labor Statistics Data cleaning and Extraction
##
## CFRM 425 Project 1, University of Washington
## Dennise Aguilar Cano, February 16, 2025
##


TUS_Analysis <- function(){
#load in necessary packages
library(tidyverse)
library(readxl)
library(stringr)
library(ggplot2)
library(dplyr)
library(quantmod)
#load the data from directory
filepath = "/Users/denniseaguilar/Documents/CFRM 425/P R O J E C T 1"
filename1 = "TimeUseDataSurvey.xlsx"
filename2 = "CodesAndNames.xlsx"
#skip the top unnecessary rows
timeUsage <- read_excel(file.path(filepath,filename1),skip=2)
codeNames <- read_excel(file.path(filepath,filename2))
#perform an inner join to merge so the Series ID's are readable
test <- inner_join(x=codeNames,y=timeUsage,join_by("Code"=="Series ID"))
#remove unnecessary columns
test1 <- select(test,-Code)
#extract and create a new column of the genders from activity Name
genders <- unlist(regmatches(test1$Name, gregexpr("(?<=\\().*?(?=\\))", test1$Name,perl=T)))
test1$Gender <- genders
test1 <- test1 %>% relocate(Gender)
#remove unnecessary columns
test1 <- select(test1,-Name)
#clean up the year titles
names(test1)[3:ncol(test1)] <- str_extract(names(test1)[3:ncol(test1)], "\\d{4}")
#remove 2020 observations since COVID disrupted survey
test1 <- select(test1,-"2020")
#save the years of the data
years <- names(test1)[3:ncol(test1)]
#remove Hours Per Day after finding t is not meaningful
test1 <- test1 %>% filter(test1$`Short Name` != "HoursPerDay")
#transpose the data so observations are rows and variables are columns
test1 <- test1 %>% pivot_longer(cols=names(test1)[3:ncol(test1)],names_to="Year",values_to = "value")
test1 <- test1 %>% pivot_wider(names_from = "Short Name", values_from = "value")
#make gender an attribute of each observation
test1 <- test1 %>% select(Year,Gender,colnames(test1[3:length(test1)]))
test1 <- test1 %>% arrange(Year)
#convert chatacter variables to factors such as gender and year
test1$Gender <- as.factor(test1$Gender)
test1$Year <- as.factor(test1$Year)
############# Problem 2 ##########
#(a)calculate then separate the average time spent by gender
bothTimeSpent <- select(test1 %>% filter(Gender == "Both"),-c(Year,Gender))
avgBothTimeSpent <- colMeans(bothTimeSpent)
menTimeSpent <- select(test1 %>% filter(Gender=="Men"), -c(Year,Gender))
avgMenTimeSpent <- colMeans(menTimeSpent)
womenTimeSpent <- select(test1 %>% filter(Gender=="Women"),-c(Year,Gender))
avgWomenTimeSpent <- colMeans(womenTimeSpent)
#combine genders
TimeSpent <- rbind(avgBothTimeSpent,avgMenTimeSpent,avgWomenTimeSpent)
#rename the rows
rownames(TimeSpent) <- c("Both","Men","Women")
#return(TimeSpent)
#(b) 
#extract all activities
timeRange <- select(test1,-c(Year,Gender))
#extract min and max for each activity
minTimeRange <- apply(timeRange,2,min)
maxTimeRange <- apply(timeRange,2,max)
TimeRange <- rbind(minTimeRange,maxTimeRange)
#rename the rows
rownames(TimeRange) <- c("minimum","maximum")
#return(TimeRange)
#(c)
#filter by year 2019
year2019 <- test1 %>% filter(Year == "2019")
year2019 <- select(year2019,-Year)
#separate men and women into their own array 
menYear2019 <- select(year2019 %>% filter(Gender=="Men"),-Gender)
womenYear2019 <- select(year2019 %>% filter(Gender=="Women"),-Gender)
#determine if the women spent more time by activity
MoreTimeIn2019 <- womenYear2019 > menYear2019
#return(MoreTimeIn2019)
#(d)
#separate care for household and nonhousehold members by gender
bothHousehold <- (test1 %>% filter(Gender=="Both"))$CareForHousehold #1:20
bothNonHousehold <- (test1 %>% filter(Gender=="Both"))$CareForNonHousehold
menHousehold <- (test1 %>% filter(Gender=="Men"))$CareForHousehold 
menNonHousehold <- (test1 %>% filter(Gender=="Men"))$CareForNonHousehold
womenHousehold <- (test1 %>% filter(Gender=="Women"))$CareForHousehold 
womenNonHousehold <- (test1 %>% filter(Gender=="Women"))$CareForNonHousehold
#add care
both <- bothHousehold + bothNonHousehold
men <- menHousehold + menNonHousehold
women <- womenHousehold + womenNonHousehold
#combine by columns as gender
CaringForOthers <- cbind(both,men,women)
#rename rows to year
rownames(CaringForOthers) <- years
#return(CaringForOthers)
#(e)
#calculate frequency of leisure(eating and drinking, leisure) by gender or both
freqBoth <- apply(select(test1 %>% filter(Gender=="Both"),c(Eating,Leisure)),1,sum)/
  apply(select(test1 %>% filter(Gender=="Both"),-c(Year,Gender,Eating,Leisure)),1,sum)

freqMen <- apply(select(test1 %>% filter(Gender=="Men"),c(Eating,Leisure)),1,sum)/
  apply(select(test1 %>% filter(Gender=="Men"),-c(Year,Gender,Eating,Leisure)),1,sum)

freqWomen <- apply(select(test1 %>% filter(Gender=="Women"),c(Eating,Leisure)),1,sum)/
  apply(select(test1 %>% filter(Gender=="Women"),-c(Year,Gender,Eating,Leisure)),1,sum)
#combine frequencies
LeisurePercentage <- cbind(freqBoth,freqMen,freqWomen)
#rename rows to year
rownames(LeisurePercentage) <- years
#return(LeisurePercentage)
#(f)
#plot of leisure percentage from(e) vs year by gender
plot(LeisurePercentage[,"freqMen"],type="l",col="red",
     ylim=c(min(LeisurePercentage[,"freqMen"],LeisurePercentage[,"freqWomen"])-.05,max(LeisurePercentage[,"freqMen"],
                                                                                       LeisurePercentage[,"freqWomen"])+.05),
     xlab = "Year", ylab = "Leisure Percentage",main="Percentage of Time Spent on Leisure")
lines(LeisurePercentage[,"freqWomen"])
legend("topleft",legend=c("Men","Women"),col=c("red","black"),lty="solid")
LeisurePlot <- recordPlot()
plot.new()
LeisurePlot
return(list(TimeSpent,TimeRange,MoreTimeIn2019,CaringForOthers,LeisurePercentage,LeisurePlot ))
}
#plot.new()
#LeisurePlot

