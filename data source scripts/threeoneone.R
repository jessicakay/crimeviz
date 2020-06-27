# 311 data parsing

library(sqldf)
library(tidyverse)
library(dplyr)

too<-read.csv("~/../Desktop/tmp_qu8207b.csv")
colnames(too)



threeoneone <- too %>% 
                    filter(is.na(submittedphoto)==FALSE) %>%
                    filter(str_detect(submittedphoto,".jpg")) %>%
                    select(submittedphoto)

for(i in length(threeoneone)){
  curl::curl_download(url = threeoneone[i])
}
