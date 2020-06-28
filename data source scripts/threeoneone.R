# 311 data parsing

library(sqldf)
library(tidyverse)
library(dplyr)
library(lubridate)

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

getSet(category,sub_date,end_date)


# example query getSet("Graffiti","2020-05-26","2020-05-31")

View(threeoneone)
too_table<-noquote(threeoneone$submittedphoto)
write.table(too_table,"~/../Desktop/threeoneone.txt",
            row.names = FALSE,
            quote = FALSE)

# fusker; mass download images

for(i in length(threeoneone)){
  curl::curl_download(url = threeoneone[i])
}

