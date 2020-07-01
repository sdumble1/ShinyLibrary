library(matrixStats)

moe<-function(x){
  1.96*sd(x)/sqrt(length(x))
}

wts<-function(x,wt){
  c(wt.mean(x,wt),1.96*wt.sd(x,wt)/sqrt(length(x)),sum(wt),weightedMedian(x,wt))
}


makeplot<-function(Outcome,Factor,Split,ComparisonType,Data,type="mean",label=""){
  Outcome2<-ifelse(Outcome=="Mobile","% Households with Mobile Phone",ifelse(Outcome=="Electric","% Households With Electricity",
                                                                             ifelse(Outcome=="FoodConsumption","Mean Food Consumption Score (0-120)",
                                                                                    ifelse(Outcome=="Income","Annual Household Income (NPR)", 
                                                                                           ifelse(Outcome=="Baby","% Household with Child 1 year or under",
                                                                                                  ifelse(Outcome=="Alcohol","% Household Indicating Alcohol as Negative Influence",
                                                                                                         ifelse(Outcome=="PrivateSchool","% Household Sending Children to Private School",
                                                                                                                ifelse(Outcome=="Migration","% Households With 1 or More Migrant Member",
                                                                                                                ifelse(Outcome=="Crime_Increase","% Households Perceiving an Increase in Crime Over Last Year",
                                                                                                                       ifelse(Outcome=="Crime_Decreased","% Households Perceiving a Decrease in Crime Over Last Year",
                                                                                                                              ifelse(Outcome=="Jobs","% Households Where At Least 1 Member has a Waged Job",label)))))))))))
  
  ComparisonType2<-ifelse(ComparisonType=="Comparison","Fair Comparisons","Population Estimates")
  Data<-subset(Data,is.na(Data[,Outcome])==F)               
  if(ComparisonType=="Comparison"){
    
    if(Split!="None"){
      plotdata<-summaryBy(as.formula(paste(Outcome,"~",Factor,"+",Split,"+Time")),data=Data,FUN=c(mean,moe,length,median))
     if(Factor!=Split){
       colnames(plotdata)[c(1,2,(ncol(plotdata)-3):ncol(plotdata))]<-c("Factor","Split","mean","moe","length","median")
     }
      if(Factor==Split){
        colnames(plotdata)[c(1,(ncol(plotdata)-3):ncol(plotdata))]<-c("Factor","mean","moe","length","median")
plotdata$Split<-plotdata$Factor
      }
      tab<-dcast(data=plotdata[plotdata$Time==1,],
                 formula=Split~Factor,value.var="length")
      
    }
    if(Split=="None"){
      plotdata<-summaryBy(as.formula(paste(Outcome,"~",Factor,"+Time")),data=Data,FUN=c(mean,moe,length,median))
      colnames(plotdata)[c(1,(ncol(plotdata)-3):ncol(plotdata))]<-c("Factor","mean","moe","length","median")
      tab<-plotdata[plotdata$Time==1,c("Factor","length")]
      colnames(tab)<-c(Factor,"Sample Size")
    }
  }
  if(ComparisonType=="Estimation"){
    
    if(Split!="None"){
      plotdata<-expand.grid(Factor=levels(Data[,Factor]),Split=levels(Data[,Split]),Time=unique(Data[,"Time"]))
      plotdata$mean<-NA
      plotdata$moe<-NA
      plotdata$median<-NA
      for(i in 1:nrow(plotdata)){
        tmp<-subset(Data,Data[,Factor]==plotdata$Factor[i]&Data[,Split]==plotdata$Split[i]&Time==plotdata$Time[i])
        wt1<-wts(tmp[,Outcome],tmp$hh_wt)
        plotdata$mean[i]<-wt1[1]
        plotdata$moe[i]<-wt1[2]
        plotdata$length[i]<-round(wt1[3])
        plotdata$median[i]<-wt1[4]
      }
      tab<-dcast(data=plotdata[plotdata$Time==1,],
                 formula=Split~Factor,value.var="length")
      
    }
    if(Split=="None"){
      plotdata<-expand.grid(Factor=levels(Data[,Factor]),Time=unique(Data[,"Time"]))
      plotdata$mean<-NA
      plotdata$moe<-NA
      plotdata$length<-NA
      plotdata$median<-NA
      for(i in 1:nrow(plotdata)){
        tmp<-subset(Data,Data[,Factor]==plotdata$Factor[i]&Time==plotdata$Time[i])
        wt1<-wts(tmp[,Outcome],tmp$hh_wt)
        plotdata$mean[i]<-wt1[1]
        plotdata$moe[i]<-wt1[2]
        plotdata$length[i]<-round(wt1[3])
        plotdata$median[i]<-wt1[4]
      }
      tab<-plotdata[plotdata$Time==1,c("Factor","length")]
      colnames(tab)<-c(Factor,"Population Size")
    }
    
  }
  
  if(type=="mean"){
  if(Split!="None"){
    p1<- ggplot(data=plotdata,aes(y=mean,x=Time,group=Factor))+geom_line(aes(col=Factor),size=1)+geom_point(aes(col=Factor),size=2)+
      facet_wrap(~Split)+geom_errorbar(aes(ymax=mean+moe,ymin=mean-moe,col=Factor),width=0.1)+
      scale_x_continuous(breaks=c(min(plotdata$Time),max(plotdata$Time)),labels=c("Baseline","Midline"))+ylab(Outcome2)+
      ggtitle(paste(Outcome2,"by",Factor,"and",Split,"\n",ComparisonType2))
  }
  if(Split=="None"){
    p1<- ggplot(data=plotdata,aes(y=mean,x=Time,group=Factor))+geom_line(aes(col=Factor),size=1)+geom_point(aes(col=Factor),size=2)+
      geom_errorbar(aes(ymax=mean+moe,ymin=mean-moe,col=Factor),width=0.1)+
      scale_x_continuous(breaks=c(min(plotdata$Time),max(plotdata$Time)),labels=c("Baseline","Midline"))+ylab(Outcome2)+ggtitle(paste(Outcome2,"by",Factor,"\n",ComparisonType2))
  }
  }
  if(type=="median"){
  
  if(Split!="None"){
    p1<- ggplot(data=plotdata,aes(y=median,x=Time,group=Factor))+geom_line(aes(col=Factor),size=1)+
      geom_point(aes(col=Factor),size=2)+
      facet_wrap(~Split)+
      scale_x_continuous(breaks=c(min(plotdata$Time),max(plotdata$Time)),labels=c("Baseline","Midline"))+
      ylab(paste("median",Outcome2))+ggtitle(paste(Outcome2,"\nby",Factor,"and",Split,"\n",ComparisonType2))
  }
  if(Split=="None"){
    p1<- ggplot(data=plotdata,aes(y=median,x=Time,group=Factor))+
      geom_line(aes(col=Factor),size=1)+geom_point(aes(col=Factor),size=2)+
      scale_x_continuous(breaks=c(min(plotdata$Time),max(plotdata$Time)),labels=c("Baseline","Midline"))+
      ylab(paste("median",Outcome2))+ggtitle(paste(Outcome2,"\nby",Factor,"\n",ComparisonType2))
  }
  
  }
  p1<-p1+ylim(0,NA)+
    theme(axis.title=element_text(size=16,face="bold"),legend.text=element_text(size=14),legend.title=element_text(size=16),
          axis.text.x=element_text(size=14),axis.text.y=element_text(size=14),strip.text=element_text(size=16,face="bold"),plot.title=element_text(size=18,face="bold"))
  if(Factor=="Stratum"){
    p1<-p1+scale_colour_manual(name="",values = c("Road Builders"="#1B5E20","Live Near Road"="#b71c1c"))
  }
 else{
    p1<-p1+scale_colour_discrete(name="")
  }
  
  tbl <- tableGrob(tab, rows=NULL)
  tbl2 <- tableGrob(data.frame(ifelse(Split!="None",ifelse(ComparisonType=="Estimation","Population Size","Sample Size"),"")), 
                    rows=NULL,cols = NULL)
  # Plot chart and table into one object
  return(grid.arrange(p1, tbl2,tbl,
                      nrow=3,
                      as.table=TRUE,
                      heights=c(4,0.25,1)))
  
}

#library(rsconnect)
#deployApp('C:/Users/sdumb/Dropbox (SSC)/Indonesia/Test/Shiny2')
