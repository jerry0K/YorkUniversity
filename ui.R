
shinyServer(
  pageWithSidebar(
    headerPanel("New York Bike Share Data"),
    
    sidebarPanel(
      selectInput("Gender","Please Select the Gender", choices = c("Male","Female")),
      sliderInput("Confidence","Please Select the required confidence level",
                  min=0.01, max=1.00, value=0.01, step=0.000001),
      sliderInput("Support","Please Select the required Support level",
                  min=0.000001, max=0.005000, value=0.0005, step=0.000001)
      
    ),
    mainPanel(
      
      plotOutput("plot")
      
    )
  )
)