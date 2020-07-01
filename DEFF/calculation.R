DEFF<-expand.grid(HH=1:100,Village=1:4)


DEFF$Gender<-sample(c("Male","Female"),size = 400,replace=TRUE)
DEFF$TimeToHospital<-abs(round(rnorm(mean=60*(DEFF$Village-1),sd=15,n=400)/10)*10)
DEFF$Income<-round(with(DEFF,ifelse(Village==1,rnorm(100,mean=5000,sd=1500),ifelse(Village==2,rnorm(100,mean=6500,sd=2000),
            ifelse(Village==3,rnorm(100,mean=6000,sd=1500),ifelse(Village==4,rnorm(100,mean=4000,sd=1500),NA)))))/100)*100
DEFF$Perception<-c(sample(c("Yes","No"),size = 100,replace=TRUE,prob=c(0.75,0.25)),sample(c("Yes","No"),size = 100,replace=TRUE,prob=c(0.6,0.4)),
                   sample(c("Yes","No"),size = 100,replace=TRUE,prob=c(0.4,0.6)),sample(c("Yes","No"),size = 100,replace=TRUE,prob=c(0.25,0.75)))


ggplot(data=DEFF,aes(y=Income,x=1))+facet_wrap(~Village,nrow=1)+geom_boxplot()+geom_point(col="red",alpha=0.5)

library(lme4)
library(sjstats)
fit1 <- lmer(TimeToHospital ~ 1 + (1 | Village), DEFF)
fit2 <- glmer(factor(Gender) ~ 1 + (1 | Village), DEFF,family="binomial")
fit3 <- lmer(Income ~ 1 + (1 | Village), DEFF)
fit4 <- glmer(factor(Perception) ~ 1 + (1 | Village), DEFF,family="binomial")
icc(fit0)
icc(fit1)
icc(fit2)
icc(fit3)

n=15
library(scales)
library(ggplot2)
summary<-data.frame(short=c("TimeToHospital","Gender","Income","Perception"),
           snappy=c("Time To Nearest Hospital","Gender of Respondent","Household Income","Happy With Healthcare"),
           full=c("Time To Nearest Hospital (minutes)","Gender of Respondent","Annual Household Income ($)",
                  "Are you happy With the service provided at your local hospital?"),type=c("numeric","factor","numeric","factor"),
           icc=c(icc(fit1)[[1]],icc(fit2)[[1]],icc(fit3)[[1]],icc(fit4)[[1]]))

variable="Perception"
DData<-subset(summary,short==variable)

DEFF$Village<-paste("Village",DEFF$Village)



if(DData$type=="numeric"){
p1<-ggplot(data=DEFF,aes_string(x=variable,y="(..count..)/sum(..count..)"))+geom_histogram(bins=20,fill="red",col="black")+
  scale_y_continuous(labels=percent)

}
if(DData$type=="factor"){
p1<-ggplot(data=DEFF,aes_string(x=1,group=variable,fill=variable))+geom_bar(show.legend=TRUE,col="black")

}
p1<-p1+facet_wrap(~Village)+
  ylab("% of Responses")+xlab(DData$full)+
  ggtitle(DData$full,subtitle = 
            paste("ICC =",round(DData$icc,2),"\nWith a cluster size of",n,"Households This Would Give A Design Effect of",
                  round(((n-1)*DData$icc)+1,1)))
p1


plot1<-ggplot(data=DEFF,aes_string(x="TimeToHospital",y="(..count..)/sum(..count..)"))+geom_histogram(bins=40,fill="red",col="black")+ylab("")+
  ggtitle(summary$full[1])+xlab("")+
  scale_y_continuous(labels=percent)
plot2<-ggplot(data=DEFF,aes_string(y="(..count..)/sum(..count..)",x="Gender",group="Gender",fill="Gender"))+
  geom_bar(show.legend=TRUE,col="black",position="dodge")+ylab("")+
  ggtitle(summary$full[2])+xlab("")+
  scale_y_continuous(labels=percent)
plot3<-ggplot(data=DEFF,aes_string(x="Income",y="(..count..)/sum(..count..)"))+geom_histogram(bins=40,fill="red",col="black")+ylab("")+
  ggtitle(summary$full[3])+xlab("")+
  scale_y_continuous(labels=percent)
plot4<-ggplot(data=DEFF,aes_string(y="(..count..)/sum(..count..)",x="Perception",group="Perception",fill="Perception"))+
  geom_bar(show.legend=TRUE,col="black",position="dodge")+
  ggtitle(summary$full[4])+xlab("")+ylab("")+
  scale_y_continuous(labels=percent)

library(gridExtra)
startplot<-grid.arrange(plot1,plot2,plot3,plot4)

enddata<-expand.grid(icc=summary$icc,n=2:50)
enddata<-merge(enddata,summary)
enddata$deff<-((enddata$n-1)*enddata$icc)+1
endplot<-ggplot(data=enddata,aes(y=deff,x=n,col=snappy,group=snappy))+geom_line(size=1)+xlab("Cluster Size")+
  ylab("Design Effect")+scale_color_discrete(name="")+scale_y_continuous(breaks=seq(0,50,by=2))

save(startplot,endplot,plot1,plot2,plot3,summary,DEFF,file="C:/Users/sdumb/Dropbox (SSD)/ssd-shiny-server/DEFF/shinydata.RData")
