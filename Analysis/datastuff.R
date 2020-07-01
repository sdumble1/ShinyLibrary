library(doBy)
library(survey)
library(ggplot2)
library(gridExtra)
library(reshape2)
library(SDMTools)
data1<-read.csv("C:/Users/sdumb/Dropbox (SSD)/DFID Nepal-RAP/Midline survey 2016/updatedMergedv3.csv")

colnames(data1)
data1<-subset(data1,duplicated(data1$ID.mid)==F&Stratum%in%c("Build - Inner Buffer","Build - RBG"))

FCS<-melt(data1,id.vars=c("Stratum","District.mid","GenderHH.mid","ID.mid","hh_wt","AgeHH.base","HHSize.base"),measure.vars = c("FoodConsumption.base","FoodConsumption.mid"),
          variable.name = "Time",value.name="FoodConsumption")
FCS$Time<-as.numeric(as.factor(FCS$Time))#

Electric<-melt(data1,id.vars=c("Stratum","District.mid","GenderHH.mid","ID.mid","hh_wt","AgeHH.base","HHSize.base"),measure.vars = c("Electric.base","Electric.mid"),
               variable.name = "Time",value.name="Electric")
Electric$Time<-as.numeric(as.factor(Electric$Time))


Mobile<-melt(data1,id.vars=c("Stratum","District.mid","GenderHH.mid","ID.mid","hh_wt","AgeHH.base","HHSize.base"),measure.vars = c("Anyphone.base","Anyphone.mid"),
             variable.name = "Time",value.name="Mobile")
Mobile$Time<-as.numeric(as.factor(Mobile$Time))


Income<-melt(data1,id.vars=c("Stratum","District.mid","GenderHH.mid","ID.mid","hh_wt","AgeHH.base","HHSize.base"),measure.vars = c("Income1_base","Income1_mid"),
             variable.name = "Time",value.name="Income")
Income$Time<-as.numeric(as.factor(Income$Time))


Baby<-melt(data1,id.vars=c("Stratum","District.mid","GenderHH.mid","ID.mid","hh_wt","AgeHH.base","HHSize.base"),measure.vars = c("baby_B","baby_M"),
             variable.name = "Time",value.name="Baby")
Baby$Time<-as.numeric(as.factor(Baby$Time))


Alc<-melt(data1,id.vars=c("Stratum","District.mid","GenderHH.mid","ID.mid","hh_wt","AgeHH.base","HHSize.base"),measure.vars = c("alcohol.base","alcohol.mid"),
           variable.name = "Time",value.name="Alcohol")
Alc$Time<-as.numeric(as.factor(Alc$Time))


Priv<-melt(data1,id.vars=c("Stratum","District.mid","GenderHH.mid","ID.mid","hh_wt","AgeHH.base","HHSize.base"),measure.vars = c("Private.base","Private.mid"),
          variable.name = "Time",value.name="Private")
Priv$Time<-as.numeric(as.factor(Priv$Time))



Crime<-melt(data1,id.vars=c("Stratum","District.mid","GenderHH.mid","ID.mid","hh_wt","AgeHH.base","HHSize.base"),measure.vars = c("crime.base","crime.mid"),
           variable.name = "Time",value.name="Crime")
Crime$Time<-as.numeric(as.factor(Crime$Time))

data1$AnyMigrants.base

Migrant<-melt(data1,id.vars=c("Stratum","District.mid","GenderHH.mid","ID.mid","hh_wt","AgeHH.base","HHSize.base"),measure.vars = c("AnyMigrants.base","AnyMigrants.mid"),
            variable.name = "Time",value.name="Migration")
Migrant$Time<-as.numeric(as.factor(Migrant$Time))


Jobs<-melt(data1,id.vars=c("Stratum","District.mid","GenderHH.mid","ID.mid","hh_wt","AgeHH.base","HHSize.base"),measure.vars = c("Job.Base","Job.Mid"),
              variable.name = "Time",value.name="Jobs")
Jobs$Time<-as.numeric(as.factor(Jobs$Time))


table(data1$Educ5.max.base)


d1<-merge(FCS,Electric)
d1.1<-merge(d1,Income)
d1.2<-merge(d1.1,Baby)
d1.3<-merge(d1.2,Alc)
d1.41<-merge(d1.3,Crime)
d1.4<-merge(d1.41,Priv)
d1.5<-merge(d1.4,Migrant)
d1.6<-merge(d1.5,Jobs)
d2<-merge(d1.6,Mobile)
d2$Electric<-100*(1-d2$Electric)
d2$Mobile<-100*d2$Mobile
d2$Alcohol<-100*d2$Alcohol
d2$Crime_Increase<-100*(as.numeric(d2$Crime=="Increased"))
d2$Crime_Decreased<-100*(as.numeric(d2$Crime=="Decreased"))
d2$Migration<-100*d2$Migration
d2$Jobs<-100*d2$Jobs
table(is.na(d2$Baby))
d2$Baby<-100*(as.numeric(d2$Baby>0))
d2$PrivateSchool<-100*(as.numeric(d2$Private>0))


d2$GenderofHead<-d2$GenderHH.mid
d2$District<-d2$District.mid


setwd("C:/Users/sdumb/Dropbox (SSD)/ssd-shiny-server/mm_workshop/Analysis")
write.csv(d2,"DataforShiny.csv",row.names=F)
