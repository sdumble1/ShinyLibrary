library(shiny)
library(ggplot2)
library(scales)
library(gridExtra)
load("shinydata.RData")


# Define server logic for slider examples
function(input, output) {
  
  output$startplot <- renderPlot({
plot(startplot)
})


  output$endplot <- renderPlot({
    plot(endplot)
  })
  



output$plot1 <- renderPlot({

  DData<-subset(summary,short==input$Variable)
  if(DData$type=="numeric"){
    p1<-ggplot(data=DEFF,aes(x=DEFF[,input$Variable],y=(..count..)/sum(..count..)))+geom_histogram(bins=20,fill="red",col="black")+
      scale_y_continuous(labels=percent)
    
  }
  if(DData$type=="factor"){
    p1<-ggplot(data=DEFF,aes(x=DEFF[,input$Variable],group=DEFF[,input$Variable],fill=DEFF[,input$Variable]))+geom_bar(show.legend=TRUE,col="black",position="dodge")
    
  }
  p1<-p1+facet_wrap(~Village)+
    ylab("% of Responses")+xlab(DData$full)+
    ggtitle(DData$full,subtitle = 
              paste("ICC =",round(DData$icc,2),"\nWith a cluster size of",input$n,"Households This Would Give A Design Effect of",
                    round(((as.numeric(as.character(input$n))-1)*DData$icc)+1,1)))
  p1
  
})

}
  
  
