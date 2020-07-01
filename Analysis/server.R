library(doBy)
library(survey)
library(ggplot2)
library(gridExtra)
library(reshape2)
library(SDMTools)



#setwd("C:/Users/sdumb/Dropbox (SSC)/ssd-shiny-server/mm_workshop/Analysis")



source("functions.R")
d2<-read.csv("DataforShiny.csv")
d2$AgeofHead<-cut(d2$AgeHH.base,breaks=c(0,35,45,55,65,999),labels=c("<35","35-45","45-55","55-65",">65"))
d2$HouseholdSize<-cut(d2$HHSize.base,breaks=c(0,3.1,5.1,7.1,999),labels=c("<=3 Members","4-5 Members","6-7 Members",">=8 Members"))

levels(d2$Stratum)<-c("Live Near Road","Road Builders")


function(input, output) {
  Type<-reactive({ifelse(input$ComparisonType=="Population Estimate","Estimation","Comparison")})

  
  
  Factor1<-reactive({ifelse(input$ComparisonType=="Population Estimate","Estimation","Comparison")})
  
  output$plot1<-renderPlot({
    if(input$Show=="RBG Only"){dtplot<-droplevels(subset(d2,Stratum=="Road Builders"))}
if(input$Show!="RBG Only"){
  dtplot<-d2}
print(makeplot(Outcome=gsub(" ","",input$Outcome),Factor=gsub(" ","",input$Factor),
               Split=gsub(" ","",input$Split),ComparisonType=Type(),Data=dtplot))
  },height=800,width=800)
}







