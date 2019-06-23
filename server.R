library(arules)
library(arulesViz)
library(shiny)
library(plyr)
library(dplyr)
library(ggplot2)

function(input, output, session) {
  
  
  dfData<-read.csv("bikeData_final.csv")
  #data(dfData),
  dfdata$BirthDecade<-as.factor(dfdata$BirthDecade)
  dfData$Gender<-as.factor(dfData$Gender)
  dfData$StartStationID<-as.factor(dfData$StartStationID)
  dfData$EndStationID<-as.factor(dfData$EndStationID)
  #dfData<-subset(dfData,dfData$Gender=gender)
  dfData<- as(dfData, "transactions")
  
  
  output$plot<-renderPlot(
    
    #gender<-input$Gender,
    conf<-input$Confidence,
    supp<-input$Support,
    
    rules_len<-apriori(data,parameter=list(support=supp,confidence=conf, minlen=4))
    
    
  )
  
}