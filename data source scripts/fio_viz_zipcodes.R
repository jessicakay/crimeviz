# fio breakdown by location
# jkant@bu.edu | github.com/jessicakay/crimeviz

library(stringr)
library(tidyverse)


fio<-read.csv("~/../Desktop/rms_fio_2019.csv",stringsAsFactors = FALSE)

fio$zip<-stringr::str_extract(fio$zip,"[[:digit:]]+")


install.packages("wesanderson")
library(wesanderson)

pal <- c("gray51","firebrick4","gray63","gray69","gray76","gray82","gray88")

fio %>% select(zip, race) %>%
  filter(is.na(zip) == FALSE & race != "") %>%
  ggplot() +
  scale_fill_manual(values = pal) +
  labs(title = "Breakdown of FIO stops by zip code")


fio %>% select(zip, race) %>%
  filter(is.na(zip) == FALSE && race != "") %>%
  group_by(zip,race) %>%
  summarize(n=n()) %>%
  ggplot() +
  geom_point(aes(y=zip,x=n,color=race))+
  scale_fill_manual(values = pal) +
  labs(title = "Breakdown of FIO stops by zip code")
