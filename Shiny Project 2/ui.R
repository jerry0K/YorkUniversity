library(shiny)
library(plotly)
library(ROAuth)
library(streamR)
library(twitteR)
library(wordcloud)
library(wordcloud2)
library(SentimentAnalysis)
library(httr)
library(stringr)
library(plyr)
library(dplyr)
library(RCurl)
library(ggplot2)
library(syuzhet)
library(sentimentr)
library(tm)
library(stopwords)
library(shinythemes)
library(tidyr)

ui <- fluidPage(theme=shinytheme("cosmo"),
                
                titlePanel("Stranger Things Tweet Analysis"),
                
                sidebarLayout(
                  sidebarPanel(
                    tags$head(tags$style(type="text/css", 
                       "#loadmessage {
                       position: fixed; top: 0px;left: 0px;width: 100%;padding: 5px 0px 5px 0px;
                       text-align: center;font-weight: bold;font-size: 100%;color: #000000;
                       background-color: #CCFF66;z-index: 105;
                       }"
                      )),
                    conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                     tags$div("Please wait while the data loads...",id="loadmessage")),
                    h4("This app will analyze maximum of 10,000 recent tweets related to Stranger Things"),
                    sliderInput(inputId = "ntweets", label = "No. of tweets to pull", 
                                min = 1000, max = 5000, value = 2000, step = 500),
                    actionButton("button", "Pull fresh tweets"),
                    hr(),
                    actionButton("button2", "Populate charts"),
                    sliderInput(inputId = "freqwords", "Min. frequency of Words in bar graph:", 
                                min=10, max= 50, value=25, step = 5),
                    sliderInput(inputId = "wcloud", "Size of wordcloud:",
                                min = 10,  max = 50, value = 10, step = 5),
                    h5("Powered By: "),
                    tags$img(src= "Shiny.JPG", height = 60, width=120)
                  ),
                  
                  
                  mainPanel(
                    tabsetPanel(
                      tabPanel("Keyword Plot", verbatimTextOutput("text"), plotOutput("freqplot") , value = 1,  
                               conditionalPanel(condition="input.tabselected %in% 1")),
                      tabPanel("Text clean-up", verbatimTextOutput("text2"), tableOutput("table") , value = 2,  
                               conditionalPanel(condition="input.tabselected %in% 2")),
                      tabPanel("WordCloud", wordcloud2Output("wcloudout"), value = 3,  
                               conditionalPanel(condition="input.tabselected %in% 3")),
                      tabPanel("Sentiment Analysis", verbatimTextOutput("text3"), plotOutput("sentiplot"), value = 4,  
                               conditionalPanel(condition="input.tabselected %in% 4")),
                      id = "tabselected")
                  ))
)



