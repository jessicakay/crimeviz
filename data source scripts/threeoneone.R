# 311 data parsing
# jkant@bu.edu | github.com/jessicakay

library(tidyverse)
library(dplyr)
library(lubridate)
library(sqldf)

attribution<-"github.com/jessicakay"
buffer<-read.csv("~/../Desktop/buffer.csv")    # 900+ MB dataset...

# load data

loadFiles<-function(startAtFile){
  setwd("~/../Desktop/311/dataset_archive/")
  getwd()
  x<-list.files(getwd())
  i<-startAtFile
  buffer<-""
  while(i < length(x)+1)
  {
    print(x[i])
    y<-read.csv(x[i],header=T)
    buffer<<-rbind(y,buffer)
    i<-i+1
    write.csv(buffer,"~/../Desktop/buffer.csv")
  }
}

too$open_dt <- as.Date(too$open_dt)

# select out reports with user-submitted photos, dump into dataframe

threeoneone <- too %>% 
                    filter(is.na(submittedphoto)==FALSE) %>%
                    filter(str_detect(submittedphoto,".jpg")) %>%
                    filter(open_dt>"2020-06-01") %>%
                    select(submittedphoto, open_dt)
# as a function                    

category<-"Graffiti"
sub_date<-"2020-05-25"
end_date<-"2020-06-01"
targpath<-"~/../Desktop/"
end_date<-as.Date(end_date)

saveSet<-function(x){
  write.table(x, noquote(paste(targpath,category,".txt",sep="")),
              row.names = FALSE,
              quote = FALSE)}

getSet<-function(category,sub_date,end_date){
  setList <<- too %>% 
      filter(reason==category) %>%
      filter(is.na(submittedphoto)==FALSE) %>%
      filter(str_detect(submittedphoto,".jpg")) %>%
      filter(open_dt > as.Date(sub_date)) %>%
      filter(open_dt < as.Date(end_date)) %>%
      select(submittedphoto, open_dt, reason)
  View(setList)
  setList <<- setList %>% select(submittedphoto)
  saveSet(setList)
}

# example query getSet("Graffiti","2020-05-26","2020-05-31")
getSet(category,sub_date,end_date)  # uses global vars  above 

# loud party calls

png(filename = '~/../Documents/GitHub/crimeviz/plots/complaints.png', width= 800, height=500)

buffer %>% 
      filter(reason=="Noise Disturbance") %>%
      filter(as.Date(open_dt) > as.Date("2010-01-01")) %>%
      filter(as.Date(open_dt) < as.Date("2021-01-01")) %>%
      filter(is.na(police_district)==FALSE) %>%
      filter(str_detect(police_district,"[:alpha:]+[:digit:]+")) %>%
        ggplot(aes(longitude,latitude,color=police_district))+
        geom_point(alpha=0.3)+
        scale_color_discrete(name="District")+
        labs(title = "City complaints, Boston 311 system", 
             subtitle = "Noise disturbances, 2010-2020",
             caption=attribution)
dev.off()

# pull categories for viewing into separate window

View(sqldf('select distinct(reason) from too'))
View(sqldf('select distinct(type) from too'))
View(colnames(too)) # variable names for constructing queries

# fusker; mass download images

for(i in length(threeoneone)){
  curl::curl_download(url = threeoneone[i])
}


