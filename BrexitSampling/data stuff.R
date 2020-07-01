setwd("C:/Users/sdumb/Dropbox (SSC)/ssd-shiny-server/BrexitSampling")

gb = readOGR(dsn=".", layer="district_borough_unitary_region_ref3")
gb@data$id = rownames(gb@data)
gb.points = fortify(gb, region="id")
gb.df = join(gb.points, gb@data, by="id")

regions = readOGR(dsn=".", layer="regions3")
regions@data$id = rownames(regions@data)
regions.points = fortify(regions, region="id")
regions.df = join(regions.points, regions@data, by="id")


Data1<-read.csv("EU-referendum-result-data.csv")

London<-subset(Data1,Region=="London")

gb.df3<-merge(gb.df,Data1[,c("Pct_Leave","Area_Code","Region")],by.x="CODE",by.y="Area_Code")

rho=0.05



p1<-ggplot(gb.df3) + 
  aes(long,lat,group=group) + 
  geom_polygon(aes(fill=Pct_Leave)) +
  geom_polygon(data=regions.df,aes(long,lat,group=group),inherit.aes = F,fill=NA,col=1,size=0.5) +
  coord_equal() +scale_fill_gradient2(name="% Voting Leave",low="orange",high="blue",mid="gray75",midpoint=50)+
  theme_classic() +ggtitle("Actual Results")+
  theme(axis.title=element_blank(),axis.text=element_blank(),axis.ticks=element_blank(),
        plot.background=element_blank(),
        plot.title=element_text(size=18,face="bold"))

p1

save.image(file="obj.RData")
