library(shiny)

# Define UI for slider demo application
fluidPage(
  
  #  Application title
  titlePanel("Sliders"),
  
  # Sidebar with sliders that demonstrate various available
  # options
  sidebarLayout(
    sidebarPanel(
      selectInput('type', 'Sampling Type', c("On-Street Interviews","Random Sample of Telephone Numbers",
                                             "Clustered Sample of Household Addresses")),
      # Simple integer interval
      sliderInput("sample", "Sample Size:",
                  min=0, max=10000, value=100,step=100),
      
      # Decimal interval with step value
      sliderInput("cluster", "Cluster Size:",
                  min = 5, max = 50, value = 30, step= 1),
     
       actionButton("refresh", label = "Refresh")
        ,
    plotOutput("plot0")),

  
     # Show a table summarizing the values entered
    mainPanel(tableOutput("summary"),
       fluidRow(
        splitLayout(cellWidths = c("40%", "60%"), plotOutput("plot1"),plotOutput("map1"))
        , tableOutput("values"))
       
       
   
)

)
)