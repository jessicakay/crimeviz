bos_crime<-read.csv("~/../Downloads/crime_incident_reports.csv")
shootings<-subset(bos_crime,bos_crime$SHOOTING=="Y")
shots<-table(shootings$DISTRICT,shootings$YEAR)
years<-as.data.frame(table(shootings$DISTRICT))
plot(shootings$DISTRICT,xlab="District",ylab="incidents",main="Boston shootings, 2015-2018")
plot(shootings$STREET,ylab="street",main="shootings by street A-Z")
crimes<-table(sqldf('select OFFENSE_DESCRIPTION, DISTRICT from bos_crime'))
library(sqldf)
murders<-sqldf('select OFFENSE_DESCRIPTION, DISTRICT from bos_crime 
      where OFFENSE_DESCRIPTION like "%murder%" ')

murder_data<-subset(bos_crime, bos_crime$OFFENSE_CODE=="111")
murder_by_district<-table(subset(bos_crime, bos_crime$OFFENSE_CODE=="111")$DISTRICT)
mdataset<-as.data.frame(murder_by_district)
View(mdataset)
  colnames(mdataset)[1]<-"District"
  colnames(mdataset)[2]<-"Frequency"
plot(mdataset$District,mdataset$Frequency,
     main="Homicide by district, 2015-2018",xlab="district",
     ylab="number of homicides")
just2018<-subset(murder_data,murder_data$YEAR=="2018")
murder_by_district<-table(subset(just2018, just2018$OFFENSE_CODE=="111")$DISTRICT)
mdataset<-as.data.frame(murder_by_district)
colnames(mdataset)[1]<-"District"
colnames(mdataset)[2]<-"Frequency" 
plot(mdataset$District, mdataset$Frequency,main="Homicide by district, 2018",
     xlab="district",ylab="number of homicides")
boxplot(mdataset$Frequency,main="Homicide by district, 2018",
        xlab="number of homicides",ylab=paste("( mean: ",round(mean(mdataset$Frequency),0),
                                   " median: ",round(max(mdataset$Frequency),0),")")
                                   ,horizontal = TRUE)
        top<-subset(mdataset,mdataset$Frequency==max(mdataset$Frequency))$District
        text(max(mdataset$Frequency),0.90,top)
        
{
  cob<-read.csv("~/../Downloads/employee-earnings-report-2017 (1).csv")        
sws<-subset(cob,cob$TITLE=="Streetworkers")
sup<-subset(cob,cob$TITLE=="Sr Streetworker")

sw<-''
sw$sws_sals<-""
sw$sws_sals<-gsub("\\$","",sws$REGULAR)
sw$sws_newsals<-gsub("\\,","",sw$sws_sals)
sw$sws_finsals<-gsub("\\ ","",sw$sws_newsals)

sw$sup_sals<-""
sw$sup_sals<-gsub("\\$","",sup$REGULAR)
sw$sup_newsals<-gsub("\\,","",sw$sup_sals)
sw$sup_finsals<-gsub("\\ ","",sw$sup_newsals)


swss<-as.numeric(sw$sws_finsals)
sups<-as.numeric(sw$sup_finsals)

boxplot(swss,sups, ylim=c(35000,55000), 
        horizontal = TRUE,
        main="street worker salaries for City of Boston",xlab="annual salary",
        ylab="job title",names=c("street workers","Sr. Streetworkers"))

sws_mean<-paste("avg = $",mean(round(as.numeric(sw$sws_finsals)),2))
sup_mean<-paste("avg = $",mean(round(as.numeric(sw$sup_finsals)),2))
text(36500,1,sws_mean)
text(36500,2,sup_mean)
}

# files 
        
newest_data<-"~/../Downloads/tmpjdulssd7.csv"

# spatial landmarks
# corner of mass ave and albany 
MaAlb_long<-as.numeric("-71.0755771")
MaAlb_lat<-as.numeric("42.333496")
geo_zoom<-c(MaAlb_lat,MaAlb_long)
# create geographic map of crime in Boston
bos_crime<-read.csv(newest_data)
shootings<-subset(bos_crime,bos_crime$SHOOTING=="Y")
shots<-table(shootings$DISTRICT,shootings$YEAR)
years<-as.data.frame(table(shootings$DISTRICT))
long_incl<-subset(bos_crime,bos_crime$Lat>"-71")
plot(long_incl$Lat~long_incl$Long)
library(tidyverse)
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

# create mapping function, plug and play variables
# spatial landmarks
# corner of mass ave and albany 
MaAlb_long<-as.numeric("-71.0755771")
MaAlb_lat<-as.numeric("42.333496")
geo_zoom<-c(MaAlb_lat,MaAlb_long)

offCodeGroup<-"Drug Violation"
offCode<-c(617:700)
newest_data<-"~/../Downloads/tmpjdulssd7.csv"
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

just2019<-subset(bos_crime,bos_crime$YEAR=="2019")

# experimental
ggplot(long_incl,aes(Long,Lat,color=YEAR))+geom_point(alpha=0.1)


# field observation data
library(sqldf)

fio<-merge(fio_data,fio_name,by="fc_num")       # join data by fc_num
# num_white<-sqldf('select count(streetaddr) from fio where contact_reason like "%white male%"')
# num_black<-sqldf('select count(streetaddr) from fio where contact_reason like "%black male%"')


fio_data<-read.csv("~/../Downloads/fieldcontactforpublic2016 (1).csv",stringsAsFactors = FALSE)
fio_name<-read.csv("~/../Downloads/fieldcontactnameforpublic2016.csv",stringsAsFactors = FALSE)
library(dplyr)
library(stringr)
fio<- fio_data %>% inner_join(fio_name,by="fc_num")
fio$zip<-paste("0",fio$zip,sep="")
fio$city[fio$city=="BSTN"]<-"Boston"
fio$city[fio$city=="BOSTON"]<-"Boston"
dorchester<-c("DDORCHESTER","DORCH","DORCHESTER","DORCHSTER","DORCCHESTER","DOR","DORCHESTERR")
fio$city[fio$city %in% dorchester]<-"Dorchester"
fio$num<-str_extract(fio$streetaddr, "[[:digit:]]+")
fio$num[is.na(fio$num)]<-""
fio$txt<-str_extract(fio$streetaddr, "[[:alpha:][:space:]&]+")
fio$zip
write.csv(fio,"~/../Desktop/cleaned.csv")


View(fio)
colnames(fio_data)


by_race$b<-table(fio$race)[3]
by_race$w<-table(fio$race)[7]

col1<-table(sqldf('select race, frisked from fio order by race'))

