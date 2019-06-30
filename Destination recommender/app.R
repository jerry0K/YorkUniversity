library(shiny)
library(shinythemes)
library(shinyWidgets)
#setwd("C:\\Users\\212562166\\Desktop\\Big Data\\data\\Destination Recommender")
stationlist = read.csv("Station Data.csv",sep = ";")
ruleslist = read.csv("Set of rules.csv")

server <- function(input, output, session) {
  
  observe({
    start_station=stationlist[stationlist$Station.Name == input$mysource, "Station.ID"]
    myby=as.numeric(input$myby)
    start_time = as.integer(format(Sys.time(), "%H"))
    checkstation = ruleslist[ruleslist$Stationid1 %in% start_station, ]
    checktime= checkstation[checkstation$Time %in% start_time, ]
    checkby=checktime[checktime$Birth.Year %in% myby, ]
    if (nrow(checkstation) %in% 0){ 
      end_station = " " 
      }
    if (nrow(checkstation) != 0 && nrow(checktime) %in% 0){
      end_station=as.character(stationlist[stationlist$Station.ID %in% checkstation$Stationid2[1], "Station.Name"])
      }
    if (nrow(checktime) != 0 && nrow(checkby)%in% 0){
      end_station=as.character(stationlist[stationlist$Station.ID %in% checktime$Stationid2[1], "Station.Name"])
    }
    if (nrow(checkby)!=0){
      end_station=as.character(stationlist[stationlist$Station.ID %in% checkby$Stationid2[1], "Station.Name"])
    }
    updateTextInput(session, "start_station", value= start_station)
    updateTextInput(session, "myby", value= myby)
    updateTextInput(session, "destination", value= end_station)
    updateTextInput(session, "mydestination", value= end_station)
  })
}

ui = fluidPage(
  setBackgroundImage("MY City Map.JPG"),
  headerPanel("Frequent Pattern based Destination recommender"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("mysource", "Choose your Starting Station", stationlist$Station.Name),
      textInput("start_station", "Station id is: "),
      radioButtons("gender", "Select Gender", choices = c(Male="1", Female = "2", Undisclosed = "0"), selected = "0"),
      textInput("myby", "Enter your birth year",value=1984),
      selectInput("mydestination", "Choose Destination", stationlist$Station.Name),
      h5("Powered By: "),
      tags$img(src= "Shiny.jpg", height = 60, width=120)
        ),
    mainPanel(
      textInput("destination", "Recommended Destination is: "),
      tags$img(src= "Sample rules.JPG", height = 600, width=600)
        )
  )
)

shinyApp(ui = ui, server = server) # this launches your app
