library(shiny)

# Define UI for slider demo application
fillPage(
  
  #  Application title
  titlePanel("Design Effect Explanation"),
  
      tabsetPanel(
        tabPanel("Summary of Variables", plotOutput("startplot")), 
        tabPanel("Exploration of individual Variables", selectInput("Variable", "Choose a Variable:",
                                                                    c("Gender of Respondent"="Gender",
                                                                      "Time To Nearest Hospital"="TimeToHospital",
                                                                     "Household Income"="Income",
                                                                      "Happy With Healthcare"="Perception")),
                 sliderInput("n", "Cluster Size",
                             min=2, max=50,value=10),
                 plotOutput("plot1")),
        tabPanel("Impact of Cluster Size on Design Effect", plotOutput("endplot"))
)
      
    
    
      

)


