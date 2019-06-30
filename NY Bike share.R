#Getting required libraries
library(dplyr)
library(caret)
library(rpart.plot)
library(ggplot2)
library(lubridate)
library(tidyverse)
library(rsconnect)
library(arules)

#Importing dataset

rm(list=ls())
setwd("C:\\Users\\212562166\\Desktop\\Big Data\\data")
data = read.csv("NYC BikeShare.csv")
head(data)

#Cleaning the data  
data$Trip.Duration=NULL #Removing redundant duration row
data$Gender=as.factor(data$Gender) # Gender as a factor
data$Start.Station.Name=as.character(data$Start.Station.Name)
data$End.Station.Name=as.character(data$End.Station.Name)

#Separating Date and Time

data$Start.Hours <- format(as.POSIXct(data$Start.Time, "%Y-%m-%d %H:%M:%S", tz = ""), format = "%H:%M:%S")
data$Start.Date <- format(as.Date(data$Start.Time,"%Y-%m-%d"), format = "%d/%m/%Y")
data$Stop.Hours <- format(as.POSIXct(data$Stop.Time, "%Y-%m-%d %H:%M:%S", tz = ""), format = "%H:%M:%S")
data$Stop.Date <- format(as.Date(data$Stop.Time,"%Y-%m-%d"), format = "%d/%m/%Y")
data$Start.Time=NULL
data$Stop.Time=NULL

#Each Station Id corresponds to same co-ordinates and same station name. SO creating separate
#station dataframe to store each station's name and co-ordinates
station1 = data[ ,c(2:5)]
station1$Station.ID=station1$Start.Station.ID 
station1$Station.Name=station1$Start.Station.Name
station1$Station.Latitude=station1$Start.Station.Latitude
station1$Station.Longitude=station1$Start.Station.Longitude
station1[,c(1:4)]=NULL

station2 = data[,c(6:9)]
station2$Station.ID=station2$End.Station.ID 
station2$Station.Name=station2$End.Station.Name
station2$Station.Latitude=station2$End.Station.Latitude
station2$Station.Longitude=station2$End.Station.Longitude
station2[,c(1:4)]=NULL

station=rbind(station1, station2)
station=station %>% distinct() #Station table is a dataframe with 114 unqiue records
write.csv(station, file = "Station Data.csv") #Write station dataframe back to hard disk 
data[, c(3:5,7:9)]=NULL #Keeping only the station IDs and removing the names, co-ordinates

#Data Exploration

summary(data$Trip_Duration_in_min)
quantile(data$Trip_Duration_in_min, c(.90, .95, .99, .999)) #99.9% of trips within 863 mins
nrow(data[data$Trip_Duration_in_min > 863, ]) #734 records with more than 14 hours
data<-data[!(data$Trip_Duration_in_min >863), ] # Records above 863 mins are deleted 
shortjrny=data[data$Trip_Duration_in_min<70, ]
longjrny = data[(data$Trip_Duration_in_min>70), ]
custdata = data[(data$User.Type == "Customer"), ]
subscdata = data[!(data$User.Type == "Customer"), ]

#Data Visualizations

hist(shortjrny$Trip_Duration_in_min, main="Histogram for Trip Duration", xlab="Trip Duration (in Mins)")

hist(longjrny$Birth.Year, main="Birth years for trips > 20 mins", xlab="Trip Duration (in Mins)")

ggplot(data = shortjrny, aes(x=(Trip_Duration_in_min), fill=factor(User.Type)))+
  geom_bar(width=0.5)+
  xlab("Trip Duration (in mins)")+
  ggtitle("Distribution by Trip Duration and User type for short journeys")+
  ylab("Total Count")+
  labs(fill="CUstomer")

ggplot(data = longjrny, aes(x=(Trip_Duration_in_min), fill=factor(User.Type)))+
  geom_bar(width=0.5)+
  xlab("Trip Duration (in mins")+
  ggtitle("Distribution by Trip Duration and User type for long journeys")+
  ylab("Total Count")+
  labs(fill="Customer")

ggplot(data = data, aes(x=(Gender), fill=factor(User.Type)))+
  geom_bar(width=0.5)+
  xlab("Gender (0-Unspecified, 1-Male, 2-Female)")+
  ggtitle("Distribution by Gender and User type")+
  ylab("Total Count")+
  labs(fill="Customer")

ggplot(data = data, aes(x=(Birth.Year), fill=factor(User.Type)))+
  geom_bar(width=0.5)+
  ggtitle("Distribution by Birth Year, User Type")+
  xlab("Birth Year")+
  ylab("Total Count")+
  labs(fill="Customer")

barplot(prop.table(table(data$Start.Hours)), 
        main="Demand for cycles during times of the day",
        xlab="Time in 24 hr format",
        ylab="Frequency of bike pickup",
        col.main="red", col.lab="blue")

barplot(prop.table(table(custdata$Start.Hours)), 
        main="Customer Demand for cycles during times of the day",
        xlab="Time in 24 hr format",
        ylab="Frequency of bike pickup",
        col.main="red", col.lab="blue")

barplot(prop.table(table(subscdata$Start.Hours)), 
        main="Subscriber Demand for cycles during times of the day",
        xlab="Time in 24 hr format",
        ylab="Frequency of bike pickup",
        col.main="red", col.lab="blue")

data<-data[!(data$User.Type == "Customer"), ] #Removing Customer rows


#Converting all IDs into characters
data$Start.Station.ID=as.character(data$Start.Station.ID)
data$End.Station.ID=as.character(data$End.Station.ID)
data$Birth.Year=as.factor(data$Birth.Year) # Converting Integers to factors
data$Start.Hours = substr(data$Start.Hours, start = 1, stop = 2)
data$Start.Hours = as.factor(data$Start.Hours) # Converting Integers to factors
data$Bike.ID=NULL
data$X=NULL

#Creating a separate sheet with only the target variables for which we need patterns
Rulesdata=data[ , c(1,2,4,5,7)]

#Data Conversion into transactional format
write.csv(Rulesdata, file = "Bike data_cleaned.csv") #Write cleaned dataframe back to hard disk
transdata = read.transactions("Bike data_cleaned.csv", format = "basket", sep = ",", cols = NULL)

#Association Rule Mining using Apriori

# For all 5 parameters 
rules5 <- apriori(transdata,parameter = list(sup = 0.0009, confidence = 0.6, minlen = 5, target="rules"))
inspect(sort(rules5, decreasing = TRUE, by = "count"))
str(rules5)

#Converting rules5 into dataframe for use in server function

rule5df = data.frame(
  lhs = labels(lhs(rules5)),
  rhs = labels(rhs(rules5)), 
  rules5@quality)
head(rule5df)
str(rule5df$lhs)

# Gather distinct frequent patterns
rule5df$lhs=as.character(rule5df$lhs)
rule5df$rhs=as.character(rule5df$rhs)
rule5df$lhs=substr(rule5df$lhs,1,nchar(rule5df$lhs)-1)
rule5df$lhs=substring(rule5df$lhs,2)
rule5df$rhs=substr(rule5df$rhs,1,nchar(rule5df$rhs)-1)
rule5df$rhs=substring(rule5df$rhs,2)
rule5df <- within(rule5df, FP <- paste(lhs, rhs, sep=","))
rule5df = rule5df[!duplicated(rule5df$count), ]

#Creating a separate sheet with only the target variables for which we need patterns
Rules3data=data[ , c(1,2,7)]

#Data Conversion into transactional format
write.csv(Rules3data, file = "Bike data_cleaned.csv") #Write cleaned dataframe back to hard disk
transdata = read.transactions("Bike data_cleaned.csv", format = "basket", sep = ",", cols = NULL)

#Association Rule Mining using Apriori

# For essential 3 parameters 
rules3 <- apriori(transdata,parameter = list(sup = 0.0009, confidence = 0.65, minlen=3, target="rules"))
inspect(sort(rules3, decreasing = TRUE, by = "count"))

#Converting rules3 into dataframe for use in server function

rule3df = data.frame(
  lhs = labels(lhs(rules3)),
  rhs = labels(rhs(rules3)), 
  rules3@quality)


# Gather distinct frequent patterns
rule3df$lhs=as.character(rule3df$lhs)
rule3df$rhs=as.character(rule3df$rhs)
rule3df$lhs=substr(rule3df$lhs,1,nchar(rule3df$lhs)-1)
rule3df$lhs=substring(rule3df$lhs,2)
rule3df$rhs=substr(rule3df$rhs,1,nchar(rule3df$rhs)-1)
rule3df$rhs=substring(rule3df$rhs,2)
rule3df <- within(rule3df, FP <- paste(lhs, rhs, sep=","))
rule3df = rule3df[!duplicated(rule3df$count), ]

#Creating a separate sheet with only the target variables for which we need patterns
Rules2data=data[ , c(1,2)]

#Data Conversion into transactional format
write.csv(Rules2data, file = "Bike data_cleaned.csv") #Write cleaned dataframe back to hard disk
transdata = read.transactions("Bike data_cleaned.csv", format = "basket", sep = ",", cols = NULL)

#Association Rule Mining using Apriori

# For essential 3 parameters 
rules2 <- apriori(transdata,parameter = list(sup = 0.0009, confidence = 0.65, target="rules"))
inspect(sort(rules2, decreasing = TRUE, by = "count"))

#Converting rules3 into dataframe for use in server function

rule2df = data.frame(
  lhs = labels(lhs(rules2)),
  rhs = labels(rhs(rules2)), 
  rules2@quality)


# Gather distinct frequent patterns
rule2df$lhs=as.character(rule2df$lhs)
rule2df$rhs=as.character(rule2df$rhs)
rule2df$lhs=substr(rule2df$lhs,1,nchar(rule2df$lhs)-1)
rule2df$lhs=substring(rule2df$lhs,2)
rule2df$rhs=substr(rule2df$rhs,1,nchar(rule2df$rhs)-1)
rule2df$rhs=substring(rule2df$rhs,2)
rule2df <- within(rule2df, FP <- paste(lhs, rhs, sep=","))
rule2df = rule2df[!duplicated(rule2df$count), ]

finalruledf = rbind(rule2df, rule3df, rule5df) # Final set of 41 freuqent patterns
finalruledf$lhs=NULL
finalruledf$rhs=NULL

finalpatterns=finalruledf[ ,c(4,5)]
str(finalpatterns$FP)
for (i in 1:3){
  finalpatterns$FP[i]= paste("NA,NA,",finalpatterns$FP[i],",NA") 
}
for (i in 4:36){
  finalpatterns$FP[i]= paste("NA,",finalpatterns$FP[i],",NA")
}
finalpatterns=separate(finalpatterns, FP, c("Time","Birth Year", "Stationid1", "Stationid2","Gender"), sep = ",")
for (i in 4:36){
  finalpatterns$Time[i]= finalpatterns$`Birth Year`[i]
  finalpatterns$`Birth Year`[i]="NA"
}
finalpatterns2 = finalpatterns
finalpatterns2$Stationid1=finalpatterns$Stationid2
finalpatterns2$Stationid2=finalpatterns$Stationid1
finalpatterns = rbind(finalpatterns, finalpatterns2)
finalpatterns <- finalpatterns[order(-finalpatterns$count),]
write.csv(finalpatterns, file = "Set of rules.csv") #Write cleaned dataframe back to hard disk

#Deploying the Shiny app
rsconnect::setAccountInfo(name='danbailoor', token='733F15BFB458BEEE9D698F979EC748A4', secret='cRkCbwg0VrIykxo4NGE6/w+nHq/6vAfQZUwwjxGF')
rsconnect::deployApp("Destination recommender")
