library(shiny)

# Define UI for slider demo application
fluidPage(
  
  #  Application title
  titlePanel("Analysis of Data"),
  
  # Sidebar with sliders that demonstrate various available
  # options
  sidebarLayout(
    sidebarPanel(
      selectInput('Show', 'Show', c("RBG Only","Both")),
      selectInput('Outcome', 'Outcome Variable', c("Income","Mobile","Electric","Food Consumption","Baby","Alcohol","PrivateSchool","Migration","Crime_Increase","Crime_Decreased","Jobs")),
      selectInput('Factor', 'Variable to Split Lines', c("Stratum","District","Gender of Head","Age of Head","Household Size")),
      selectInput('Split', 'Variable to Split Plots', c("None","Stratum","District","Gender of Head","Age of Head","Household Size")),
      selectInput('ComparisonType', 'Type of Comparison', c("Population Estimate","Fair Comparison"))
                  
      ,width=3)
  ,
  
  mainPanel(
    plotOutput("plot1")
  )
  )
)

  