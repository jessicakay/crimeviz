library(tidyverse)

# files 
        
newest_data<-"/Users/jessa/Downloads/Downloads/tmpa5mvyouu.csv"
code_list<-readxl::read_xlsx("/Users/jessa/Downloads/old computer/Desktop/rmsoffensecodes.xlsx")
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
                   ", name: \"",code_name,"\"", sep="")
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
      filename<-paste("~/../Documents/Github/crimeviz/plots/",
                      offCode,".png",sep="")
      png(filename=filename, width= 800, height=500)
           print({makeMap()})
      dev.off()
    }
  }

# generates spacial plot of crimes designated by specific code
# and uses lookup index to retrieve name. Includes optional 
# argument to save to file where 0 = don't save,1 = save

mapIt(801,1) 

# add crosshair function below (in progress):

  geo_zoom<-c(MaAlb_lat,MaAlb_long)
  geom_hline(yintercept = geo_zoom[1],colour="grey")+
  geom_vline(xintercept = geo_zoom[2],colour="grey")
