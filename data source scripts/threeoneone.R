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

View(threeoneone)

too_table<-noquote(threeoneone$submittedphoto)

write.table(too_table,"~/../Desktop/threeoneone.txt",
            row.names = FALSE,
            quote = FALSE)

# fusker; mass download images

for(i in length(threeoneone)){
  curl::curl_download(url = threeoneone[i])
}

