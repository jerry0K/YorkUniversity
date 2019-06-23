shinyServer(
  pageWithSidebar(
    headerPanel("New York Bike Share Data"),
    
    sidebarPanel(
      selectInput("Target","Please Select the Target", choices = c("Gender","Birth Decade")),
      conditionalPanel(condition="input.Target=='Gender'",
                       selectInput("Gender","Please Select the Gender", choices = c("Male","Female"))
                       ),
      conditionalPanel(condition="input.Target=='Birth Decade'",
                       selectInput("Gender","Please Select the Gender", choices = c("60s","70s","80s","90s"))
                       )
      
    ),
    mainPanel()
  )
)