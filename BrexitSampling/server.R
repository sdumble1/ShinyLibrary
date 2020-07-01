library(shiny)
require("rgdal") 
require("maptools")
require("ggplot2")
require("plyr")
require("sp")
require("rgeos")


load("obj.RData")


# Define server logic for slider examples
function(input, output) {
  
  

  
  # Reactive expression to compose a data frame containing all of
  # the values
  n1<-16141241
  n2<-17410742
  reality<-n2/(n2+n1)


  

   

    
  sliderValues <- reactive({
    
    if(input$type=="Random Sample of Telephone Numbers"){
      SRS<-eventReactive(input$refresh,
                         {sample(c("R","L"),size=as.numeric(input$sample),replace=T,prob=c(n1,n2*exp(-0.15)))})
      t1<-reactive({prop.table(table(SRS()))[1]})
      t2<-reactive({prop.table(table(SRS()))[2]})
      moe<-reactive({1.96*sqrt(t1()*t2()/as.numeric(input$sample))})
      
    # Compose data frame
    a<-data.frame(
      Name = c("Method",
               "Sample Size", 
               "Predicted % Leave",
               "Predicted % Remain",
               "Margin of Error (%)",
               "Difference Between Sample and Reality (%)",
               "Cost (GBP)"),
      Value = c(input$type, 
                    input$sample,
                round(100*t1(),1),
                round(100*t2(),1),
                round(100*moe(),1),
                round(100*(reality-t1()),1),
                1000+input$sample*20), 
      stringsAsFactors=FALSE)
    return(a)
    }
    if(input$type=="On-Street Interviews"){
      CS<-eventReactive(input$refresh,
                         {rbinom(1,as.numeric(input$sample),(London$Leave)/((London$Leave)+London$Remain))})
      t1<-reactive({prop.table(c(CS(),as.numeric(input$sample)-CS()))[1]})
      t2<-reactive({prop.table(c(CS(),as.numeric(input$sample)-CS()))[2]})
      moe<-reactive({1.96*sqrt(t1()*t2()/as.numeric(input$sample))})
      
      # Compose data frame
      a<-data.frame(
        Name = c("Method",
                 "Sample Size", 
                 "Predicted % Leave",
                 "Predicted % Remain",
                 "Margin of Error (%)",
                 "Difference Between Sample and Reality (%)",
                 "Cost (GBP)"),
        Value = c(input$type, 
                  input$sample,
                  round(100*t1(),1),
                  round(100*t2(),1),
                  round(100*moe(),1),
                  round(100*(reality-t1()),1),
                  1000+input$sample*25), 
        stringsAsFactors=FALSE)
      return(a)
    }
    if(input$type=="Clustered Sample of Household Addresses"){
    
    NCLuster<-reactive({round(input$sample/input$cluster)})
    RealN<-reactive({NCLuster*input$cluster})
    
    
  
    
    
    Clusters<-eventReactive(input$refresh,
                            {
    a<-data.frame(subset(Data1,Area_Code%in%
 sample(Data1$Area_Code,size=round(as.numeric(input$sample)/as.numeric(input$cluster)),prob=Data1$Electorate)),
                                         Freq=input$cluster)
                              
    a$R<-rbinom(nrow(a),input$cluster,a$Remain/((a$Leave+a$Remain)))
    a$L<-input$cluster-a$R
    return(a)
                            })
    
    n1<-reactive({sum(Clusters()$R)})
    n2<-reactive({sum(Clusters()$L)})
    t1<-reactive({prop.table(c(n2(),n1()))})
    DEFF<-reactive({1+(input$cluster-1)*rho})
    moe<-reactive({1.96*DEFF()*sqrt(t1()[1]*t1()[2]/(as.numeric(input$cluster)*round(as.numeric(input$sample)/as.numeric(input$cluster))
                                                   ))})
    a<-data.frame(
      Name = c("Method",
               "Sample Size",
               "Predicted % Leave",
               "Predicted % Remain",
               "Margin of Error (%)",
               "Number of Clusters",
               "Size of Clusters",
               "Design Effect",
               "Difference Between Sample and Reality (%)",
               "Cost (GBP)"),
      Value = c(input$type, 
                as.numeric(input$cluster)*round(as.numeric(input$sample)/as.numeric(input$cluster)),
                round(100*t1()[1],1),
                round(100*t1()[2],1),
                round(100*moe(),1),
                round(as.numeric(input$sample)/as.numeric(input$cluster)),
                input$cluster,
                DEFF(),
                round(100*(reality-t1()[1]),1),
                1000+(round(as.numeric(input$sample)/as.numeric(input$cluster))*100)+(input$cluster>1)*
                  (as.numeric(input$cluster)*round(as.numeric(input$sample)/as.numeric(input$cluster)))*(250)), 
      stringsAsFactors=FALSE)
    return(a)
    }
    
    
  }
    )
  
  
  # Show the values using an HTML table
  output$values <- renderTable({
    sliderValues()
  })
  output$summary <- renderTable({
    
    data.frame(Conclusion=ifelse(as.numeric(as.character(sliderValues()[3,2]))>50&(as.numeric(as.character(sliderValues()[3,2]))-
                                                                                     as.numeric(as.character(sliderValues()[5,2]))>50),
      "Evidence Suggests Britain will Vote to Live the EU!",
      ifelse(as.numeric(as.character(sliderValues()[3,2]))>=50&(as.numeric(as.character(sliderValues()[3,2]))-
                                                                 as.numeric(as.character(sliderValues()[5,2]))<50)|
               as.numeric(as.character(sliderValues()[3,2]))<=50&(as.numeric(as.character(sliderValues()[3,2]))
                                                                 +as.numeric(as.character(sliderValues()[5,2]))>50),
      "Too Close to Call!",  ifelse(as.numeric(as.character(sliderValues()[3,2]))&(as.numeric(as.character(sliderValues()[3,2]))+
                   as.numeric(as.character(sliderValues()[5,2]))<50),
      "Evidence Suggests Britain will Vote to Remain in the EU!",""))))
                                 
  })
  
  
  output$plot0 <- renderPlot({print(p1)})
  output$plot1 <- renderPlot({
    plotDT<-reactive({data.frame(L=as.numeric(as.character(sliderValues()[3,2])),
                                 E=as.numeric(as.character(sliderValues()[5,2])))})

      
    ggplot(plotDT(),aes(x=1,y=L))+geom_hline(yintercept=reality*100,col=2,size=1,linetype=2)+
     geom_hline(yintercept=50,col=1,size=1)+
      xlim(0.6,1.4)+
      geom_errorbar(aes(ymax=min(100,L+E),
                        ymin=max(0,L-E)),width=0.2,col="blue")+ylim(0,100)+ylab("% Voting Leave")+
      geom_point(size=4,col=ifelse(plotDT()$L>50,"red","green"))+ggtitle("Sample Results \n 95% Confidence Interval")+
            theme(axis.text.x=element_blank(),axis.title.x=element_blank(),axis.ticks.x=element_blank(),
            axis.text.y=element_text(size=14),axis.title.y=element_text(size=16))
    
  },height=350,width=350)
  
  output$map1 <- renderPlot({
    
    if(input$type=="Random Sample of Telephone Numbers"){
    locs<-eventReactive(input$refresh,{
      data.frame(table(sample(Data1$Area_Code,size=as.numeric(input$sample),
                              replace=T,prob=Data1$Electorate)))})
    
    gb.df2<-reactive({a=merge(gb.df,locs(),all=T,by.x="CODE",by.y="Var1")
    a$Freq<-ifelse(is.na(a$Freq),0,a$Freq)
    a<-a[order(a$order),]
    return(a)})
    }
    if(input$type=="On-Street Interviews"){
      locs<-eventReactive(input$refresh,{
        data.frame(table(sample(London$Area_Code,size=as.numeric(input$sample),
                                replace=T,prob=London$Electorate)))})
      
      gb.df2<-reactive({a=merge(gb.df,locs(),all=T,by.x="CODE",by.y="Var1")
      a$Freq<-ifelse(is.na(a$Freq),0,a$Freq)
      a<-a[order(a$order),]
      return(a)})
    }
    if(input$type=="Clustered Sample of Household Addresses"){
      
    
     locs<-eventReactive(input$refresh,{
        data.frame(subset(Data1,Area_Code%in%
sample(Data1$Area_Code,size=round(as.numeric(input$sample)/as.numeric(input$cluster)),prob=Data1$Electorate)),
Freq=input$cluster)
       
           })
      
      gb.df2<-reactive({
        
        a=merge(gb.df,locs(),all=T,by.x="CODE",by.y="Area_Code")
      a$Freq<-ifelse(is.na(a$Freq),0,a$Freq)
      a<-a[order(a$order),]
      return(a)})
    }
    
  
     
  ggplot(gb.df2()) + 
    aes(long,lat,group=group) + 
    geom_polygon(aes(fill=Freq),col=NA) +ylim(0,1080000)+
    coord_equal() +scale_fill_continuous(name="Number of Samples",low="gray75",high="red")+
    geom_polygon(data=regions.df,aes(long,lat,group=group),inherit.aes = F,fill=NA,col=1,size=0.25) +
    theme_classic() +ggtitle("Sampling Locations")+
    theme(axis.title=element_blank(),axis.text=element_blank(),axis.ticks=element_blank(),
          plot.background=element_blank(),
          plot.title=element_text(size=18,face="bold"))
    

  
  },height=400,width=400)
  
    }
  
  
