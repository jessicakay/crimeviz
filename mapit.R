library(tidyverse)

# files 
        
newest_data<-"~/../Downloads/tmpa5mvyouu.csv"
code_list<-readxl::read_xlsx("~/../Desktop/rmsoffensecodes.xlsx")
attribution<-"github.com/jessicakay"
bos_crime<-read.csv(newest_data)

# spatial landmarks
# corner of mass ave and albany, 42.333496, -71.0755771

MaAlb_long<-as.numeric("-71.0755771")
MaAlb_lat<-as.numeric("42.333496")
geo_zoom<-c(MaAlb_lat,MaAlb_long)

# subset homicide

long_incl$ismurder<-ifelse(long_incl$OFFENSE_CODE_GROUP=="Homicide","1","0")
ggplot(long_incl, aes(Long,Lat,color=ismurder))+geom_point(alpha=0.1)

# function to retrieve, stratify and visualize by given code

mapIt<-function(offCode){
  code_list<<-as.data.frame(code_list)
  code_name<-code_list$NAME[which(code_list$CODE==offCode)]
  code_indx<-paste("code: ",code_list$CODE[which(code_list$CODE==offCode)],
                   ", name: \"",code_name,"\"",
                   sep="")
  target<-subset(bos_crime,bos_crime$OFFENSE_CODE %in% offCode)
  years<-as.data.frame(table(target$DISTRICT))
  long_incl<-subset(target,target$Lat>"-71")
    ggplot(long_incl,aes(Long,Lat,color=YEAR))+geom_point(alpha=0.1)+
    labs(title = "Crime in Boston", 
       subtitle=tolower(code_indx),
       caption=attribution)
      }

# variation with "cross-hair" coords for orienting viewer to 
# specific position in space

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

# function to retrieve, stratify and visualize by given code
# with optional argument to save as PNG file

mapIt<-function(offCode,s){
  code_list<<-as.data.frame(code_list)
  code_name<-code_list$NAME[which(code_list$CODE==offCode)]
  code_indx<-paste("code: ",code_list$CODE[which(code_list$CODE==offCode)],
                   ", name: \"",code_name,"\"",
                   sep="")
  target<-subset(bos_crime,bos_crime$OFFENSE_CODE %in% offCode)
  years<-as.data.frame(table(target$DISTRICT))
  long_incl<-subset(target,target$Lat>"-71")
  makeMap<<-function(){
    ggplot(long_incl,aes(Long,Lat,color=YEAR))+geom_point(alpha=0.1)+
      labs(title = "Crime in Boston", 
           subtitle=tolower(code_indx),
           caption=attribution)}
  makeMap()
  if(s==1){
      # needs bugfix
      png(filename='~/../Desktop/plot.png',width= 800, height=500)
      makeMap()
      dev.off()
    }
}
