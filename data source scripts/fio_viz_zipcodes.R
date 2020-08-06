# fio breakdown by location
# jkant@bu.edu | github.com/jessicakay/crimeviz

library(stringr)
library(tidyverse)
install.packages("yarrr")

attribution <- "github.com/jessicakay/crimeviz"
fio<-read.csv("~/../Desktop/rms_fio_2019.csv",stringsAsFactors = FALSE)

fio$zip<-stringr::str_extract(fio$zip,"[[:digit:]]+")


install.packages("wesanderson")
library(wesanderson)

pal <- c("gray51","firebrick","gray63","gray69","gray76","gray82","gray88")

###

t<-fio %>% 
  filter(race=="Black") %>% 
  select(zip,race) %>% 
  group_by(zip) %>%
  summarize(n=n())

fio %>% select(zip, race) %>%
  filter(is.na(zip) == FALSE & race != "") %>%
  ggplot() +
  geom_bar(aes(y=zip,fill=race),width = 0.3)+
  scale_fill_manual(values=pal)+
  labs(title = "Breakdown of FIO stops by zip code",
       subtitle = "Boston, January - September 2019",
       caption = attribution)+
  xlab("")+
  theme_classic()+
  theme(legend.position = c(0.8,0.8),
        legend.title = element_blank(),
        legend.background = element_rect(linetype = 1,colour = "gray"))

###

fio %>% select(zip, race) %>%
  filter(is.na(zip) == FALSE & race != "") %>%
  ggplot() +
  geom_bar(aes(y=zip,fill=race))+
  scale_fill_manual(values = pal) +
  labs(title = "Breakdown of FIO stops by zip code")+
  theme_minimal()




fio %>% select(zip, race) %>%
  filter(is.na(zip) == FALSE && race != "") %>%
  group_by(zip,race) %>%
  ggplot()+
  geom_bar(aes(y=zip,fill=race))+
  scale_color_manual(values=as.vector(piratepal("usualsuspects")))+
  labs(title = "Breakdown of FIO stops by zip code")
<<<<<<< HEAD
  
  
  fio %>% select(zip, race) %>%
    filter(is.na(zip) == FALSE && race != "") %>%
    group_by(zip, race) %>%
    summarize(n = n()) %>%
    ggplot() +
    geom_bar(aes(y = zip, x = n, color = race)) +
    scale_color_manual(values = as.vector(piratepal("usualsuspects"))) +
    labs(title = "Breakdown of FIO stops by zip code")
  

  fio %>% select(zip, race) %>%
    filter(is.na(zip) == FALSE & zip != "") %>%
    filter(is.na(race) == FALSE & race != "") %>%
    group_by(zip,race) %>%
    summarize(n=n()) %>%
    ggplot()+
    geom_point(aes(y=zip,x=n,color=race))+
  scale_color_manual(values=as.vector(piratepal("usualsuspects")))+
    labs(title = "Breakdown of FIO stops by zip code")
  
  
  fio %>% select(zip, race) %>%
    filter(is.na(zip) == FALSE & race != "") %>%
    group_by(zip,race) %>%
    summarize(count=n())%>%
    group_by(race)%>%
    mutate(m=mean(count))

  fio %>% filter (race=="Black") %>% select
  
  
=======

>>>>>>> 01810bd703f2bbf0f88861464fe0a9e255ed62c2
