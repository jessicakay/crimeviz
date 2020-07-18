
library(dplyr)
library(stringr)

# Boston Fire data

  # parse all data in folder and output to master file

loadFiles<-function(startAtFile){
  setwd("~/../Downloads/Boston Fire data/")
  getwd()
  x<-list.files(getwd())
  i<-startAtFile
  buffer<-""
  while(i < length(x)+1)
    {
      print(x[i])
      y<-read.csv(x[i],header=T)
      buffer<-rbind(y,buffer)
      i<-i+1
      write.csv(buffer,"~/../Desktop/buffer.csv")
    }
}

  # note: even after merging all 30 available datasets, 
  # narcan adminstration doesn't appear in a single one.
  # using "police matter" and other vague codes

  dataset<-read.csv("~/../Desktop/GIS final/fire dept data for GIS project/bfdata_all.csv")
  colnames(dataset)<-gsub("\\.","",colnames(dataset))

  # add city and state columns for geocoding

  codeableData<-dataset
  codeableData$city<-"Boston"
  codeableData$state<-"MA"

  # strip extra white space
  
  codeableData$StreetNumber<-gsub("\\s+"," ",codeableData$StreetNumber) 
  codeableData$StreetName<-gsub("\\s+"," ",codeableData$StreetName)
  codeableData$StreetType<-gsub("\\s+"," ",codeableData$StreetType)
  codeableData$StreetName<-trimws(codeableData$StreetName,which=c("both"), whitespace = "[ \t\r\n]")
  codeableData$address<-paste(codeableData$StreetNumber,
                              codeableData$StreetName,
                              codeableData$StreetType,sep = " ")

  # create subsetted data using only 

    possibleCodes<-c(300,311,320,321,381,500,
                   510,550,551,552,553,554,
                   600,661,900,911)

  x<-subset(all,all$IncidentType %in% c(possibleCodes))

  sqldf("select distinct(IncidentType), 
        IncidentDescription from dataset where 
        AlarmDate like '%2019%'")


# mapIt function for Boston Crime Data, 2019, Jess Kant
# the following code loads 


MaAlb_long<-as.numeric("-71.0755771")
MaAlb_lat<-as.numeric("42.333496")
geo_zoom<-c(MaAlb_lat,MaAlb_long)

newest_data<-"~/../Downloads/tmpjdulssd7.csv"
bos_crime<-read.csv(newest_data)
aggravated<-423
if(("sqldf" %in% installed.packages()[,1])==FALSE){install.packages("sqldf")}else{library(sqldf)}

getCount<-function(){
  two18<<-sqldf(sprintf("select count(INCIDENT_NUMBER) from bos_crime 
                       where OFFENSE_CODE like '%s' AND YEAR like 2018
                       AND DISTRICT like 'E13'",offCode))
  two19<<-sqldf(sprintf("select count(INCIDENT_NUMBER) from bos_crime 
                       where OFFENSE_CODE like '%s' AND YEAR like 2019
                       AND DISTRICT like 'E13'",offCode))
                      }

mapIt<-function(offCode){
  target<-subset(bos_crime,bos_crime$OFFENSE_CODE %in% offCode)
  years<-as.data.frame(table(target$DISTRICT))
  long_incl<-subset(target,target$Lat>"-71")
  library(tidyverse)
  ggplot(long_incl,aes(Long,Lat,color=YEAR))+geom_point(alpha=0.1)+
    scale_color_gradient(low="#deebf7",high="#08306b")+
    geom_hline(yintercept = geo_zoom[1],colour="grey")+
    geom_vline(xintercept = geo_zoom[2],colour="grey")
    getCount()
    cat("2018: ", as.numeric(two18)," 2019: ",as.numeric(two19))
    }

just18<-subset(bos_crime,bos_crime$YEAR=="2018")

write.csv(just1819,"~/../Desktop/GIS final/bos_crime_1819only.csv")

sqldf("select count(INCIDENT_NUMBER) from bos_crime 
                where (street like '%Albany%' 
                 OR street like '%Southampton%' 
                 OR street like '%Melnea%' )
               AND year like 2019 
               AND month like '6'
               AND OFFENSE_CODE like '423'")

library(dplyr)
library(stringr)


  assaults<-filter(bos_crime,str_detect(OFFENSE_CODE_GROUP,"Assault")) 
  newtab<-select(assaults,"YEAR","DISTRICT")
  
  assaults<-filter(assaults,MONTH=="7")
  assaults<-filter(assaults,MONTH=="6")
  
    # generate table stratified by type and year
    
    entries<-table(assaults$OFFENSE_CODE_GROUP~assaults$YEAR)
    newtab<-select(assaults,"YEAR")
    tble<-filter(as.data.frame(table(assaults$OFFENSE_CODE_GROUP,assaults$YEAR)),Freq>1)
    
  
  newtab<-select(assaults,"YEAR","DISTRICT")
  x<-table(newtab)
  write.table(x,"clipboard",sep="\t")
  colnames(tble)[1]<-"description"
  colnames(tble)[2]<-"year"
  
  # subset generation
    
    colnames(bos_crime)
    portOver<-c("YEAR","Long","Lat","OFFENSE_CODE_GROUP","YEAR","DISTRICT")
    trimmed<-select(bos_crime, portOver)
    write.csv(trimmed,"~/../Desktop/GIS final/all_years_trimmed.csv")
    
    july2018<-read.csv("~/../Desktop/July assault, 2018.csv")
    july2019<-read.csv("~/../Desktop/July assault, 2019.csv")
    
    colnames(july2018)[1]<-"CT"
    colnames(july2019)[1]<-"CT"
    colnames(july2018)[76]<-"tract"
    colnames(july2019)[76]<-"tract"
    
    a<-as.data.frame(sqldf('select count(*), CT from july2018 group by CT'))
    b<-as.data.frame(sqldf('select count(*), CT from july2019 group by CT'))
    c<-cbind(a,b)
    
    colnames(c)[1]<-"2018"
    colnames(c)[2]<-"CT_18"
    colnames(c)[3]<-"2019"
    colnames(c)[4]<-"CT_19"
    
    colnames(c)<-"1"
    
    pullTracts<-c(25025080401, 25025081700, 25025080601, 25025080100, 25025071201, 
                  25025070900, 25025080300, 25025080100, 25025061200, 25025071101)
    sqldf('select * from c where * like pullTracts')
    write.table(cbind(a,b),"clipboard",sep="\t")

# BARI 911 calls, breakdown by crime type

esf<-read.csv("~/../Desktop/911_calls_2018_vc.csv", stringsAsFactors = FALSE)
x<-esf %>% 
  select(Gn_2018,Vl_2018,SD_2018,PC_2018,ISD_Nbh) %>%
  group_by(ISD_Nbh) %>%
  mutate(m=mean(Gn_2018))

