library(tidyverse)

# files 
        
newest_data<-"~/../Downloads/tmpjdulssd7.csv"

# spatial landmarks
# corner of mass ave and albany 
# corner of mass ave and albany, 42.333496, -71.0755771

MaAlb_long<-as.numeric("-71.0755771")
MaAlb_lat<-as.numeric("42.333496")
geo_zoom<-c(MaAlb_lat,MaAlb_long)

offCodeGroup<-"Drug Violation"
offCode<-c(617:700)
newest_data<-"~/../Downloads/tmpa5mvyouu.csv"
bos_crime<-read.csv(newest_data)

# subset homicide
long_incl$ismurder<-ifelse(long_incl$OFFENSE_CODE_GROUP=="Homicide","1","0")
ggplot(long_incl, aes(Long,Lat,color=ismurder))+geom_point(alpha=0.1)

mapIt<-function(offCode){
  code_list<-readxl::read_xlsx("~/../Desktop/rmsoffensecodes.xlsx")
  target<-subset(bos_crime,bos_crime$OFFENSE_CODE %in% offCode)
  years<-as.data.frame(table(target$DISTRICT))
  long_incl<-subset(target,target$Lat>"-71")
  ggplot(long_incl,aes(Long,Lat,color=YEAR))+geom_point(alpha=0.1)+
  ggtitle(label=code_list$NAME[which(code_list$CODE==offCode)])+
  labs(caption = "jkant@bu.edu")
  }

mapIt_wGrid<-function(offCode){
  target<-subset(bos_crime,bos_crime$OFFENSE_CODE %in% offCode)
  years<-as.data.frame(table(target$DISTRICT))
  long_incl<-subset(target,target$Lat>"-71")
  library(tidyverse)
  ggplot(long_incl,aes(Long,Lat,color=YEAR))+geom_point(alpha=0.1)+
    scale_color_gradient(low="#deebf7",high="#08306b")+
    geom_hline(yintercept = geo_zoom[1],colour="grey")+
    geom_vline(xintercept = geo_zoom[2],colour="grey")
}
