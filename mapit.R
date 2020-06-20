library(tidyverse)

# files 
        
newest_data<-"~/../Downloads/tmpjdulssd7.csv"

# spatial landmarks
# corner of mass ave and albany 
MaAlb_long<-as.numeric("-71.0755771")
MaAlb_lat<-as.numeric("42.333496")
geo_zoom<-c(MaAlb_lat,MaAlb_long)
{
# create geographic map of crime in Boston

bos_crime<-read.csv(newest_data)
shootings<-subset(bos_crime,bos_crime$SHOOTING=="Y")
shots<-table(shootings$DISTRICT,shootings$YEAR)
years<-as.data.frame(table(shootings$DISTRICT))
long_incl<-subset(bos_crime,bos_crime$Lat>"-71")
plot(long_incl$Lat~long_incl$Long)
ggplot(long_incl,aes(Long,Lat,color=DISTRICT))+geom_point(alpha=0.1)

# subset homicide
long_incl$ismurder<-ifelse(long_incl$OFFENSE_CODE_GROUP=="Homicide","1","0")
ggplot(long_incl, aes(Long,Lat,color=ismurder))+geom_point(alpha=0.1)

# experimental all crime with overlay subset homicide
long_incl<-subset(bos_crime,bos_crime$Lat>"-71")
long_incl$ismurder<-ifelse(long_incl$OFFENSE_CODE_GROUP=="Homicide","1","0")
hom<-subset(long_incl,long_incl$ismurder=="1")
ggplot(long_incl, aes(Long,Lat,color=ismurder))+
  geom_point(alpha=0.01)+
  geom_point(data=hom,alpha=1.0,pch=3)+
  theme(legend.position="right")+
  labs(fill="type of crime")
}
# create geographic map of auto breakins
auto<-subset(bos_crime,bos_crime$OFFENSE_CODE_GROUP=="Larceny From Motor Vehicle")
years<-as.data.frame(table(auto$DISTRICT))
long_incl<-subset(auto,auto$Lat>"-71")
plot(long_incl$Lat~long_incl$Long)
library(tidyverse)
ggplot(long_incl,aes(Long,Lat,color=YEAR))+geom_point(alpha=0.1)+
  scale_color_gradient(low="#deebf7",high="#08306b")+
  geom_vline(xintercept = geo_zoom[2],colour="grey")+
  geom_hline(yintercept = geo_zoom[1],colour="grey")


# create geographic map of drug arrests
drug<-subset(bos_crime,bos_crime$OFFENSE_CODE_GROUP=="Drug Violation")
years<-as.data.frame(table(drug$DISTRICT))
long_incl<-subset(drug,drug$Lat>"-71")
plot(long_incl$Lat~long_incl$Long)
library(tidyverse)
ggplot(long_incl,aes(Long,Lat,color=YEAR))+geom_point(alpha=0.1)+
  scale_color_gradient(low="#deebf7",high="#08306b")+
  geom_hline(yintercept = geo_zoom[1],colour="grey")+
  geom_vline(xintercept = geo_zoom[2],colour="grey")

just2019<-subset(bos_crime,bos_crime$YEAR=="2019")

# turn into function

# spatial landmarks

# corner of mass ave and albany, 42.333496, -71.0755771

MaAlb_long<-as.numeric("-71.0755771")
MaAlb_lat<-as.numeric("42.333496")
geo_zoom<-c(MaAlb_lat,MaAlb_long)

offCodeGroup<-"Drug Violation"
offCode<-c(617:700)
newest_data<-"~/../Downloads/tmpa5mvyouu.csv"
bos_crime<-read.csv(newest_data)

mapIt<-function(offCode){
  target<-subset(bos_crime,bos_crime$OFFENSE_CODE %in% offCode)
  years<-as.data.frame(table(target$DISTRICT))
  long_incl<-subset(target,target$Lat>"-71")
  library(tidyverse)
  ggplot(long_incl,aes(Long,Lat,color=YEAR))+geom_point(alpha=0.1)+
    scale_color_gradient(low="#deebf7",high="#08306b")+
    geom_hline(yintercept = geo_zoom[1],colour="grey")+
    geom_vline(xintercept = geo_zoom[2],colour="grey")
    }
