# 311 data parsing
# jkant@bu.edu | github.com/jessicakay

library(tidyverse)
library(dplyr)
library(lubridate)
library(sqldf)

# load data

too<-read.csv("~/../Desktop/tmp_qu8207b.csv")
colnames(too)

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


too %>% 
      filter(reason=="Noise Disturbance") %>%
      filter(open_dt > as.Date("2019-01-01")) %>%
      ggplot(aes(longitude,latitude))+
        geom_point(alpha=0.1)+
        labs(title = "City complaints", 
             subtitle = "Noise disturbances",
            caption=attribution)


# pull categories for viewing into separate window

View(sqldf('select distinct(reason) from too'))
View(sqldf('select distinct(type) from too'))
View(colnames(too)) # variable names for constructing queries


# fusker; mass download images

for(i in length(threeoneone)){
  curl::curl_download(url = threeoneone[i])
}
