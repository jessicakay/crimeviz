# fio breakdown by location
# jkant@bu.edu | github.com/jessicakay/crimeviz

library(stringr)
library(tidyverse)

attribution <- "github.com/jessicakay/crimeviz"
jset <- "https://raw.githubusercontent.com/jackiejahn/boston-FIO-2019/master/rms_fio_2019.csv"

fio<-read.csv("~/../Desktop/rms_fio_2019.csv",stringsAsFactors = FALSE)

if(Sys.getenv("DESKTOP_SESSION")=="ubuntu"){
  fio<-read.csv(jset,stringsAsFactors = FALSE)
}

fio$zip<-stringr::str_extract(fio$zip,"[[:digit:]]+")


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

  

# zip code breakdown from http://www.bostonplans.org/getattachment/8d4b1dc5-ce11-45b4-9bf5-370444b49b68
# courtesy of Nathan Story

  allston<-c("02134", "02135",
             "02163")
  backbay<-c("02108", "02116", 
             "02117", "02199", 
             "02217", "02133")
  central<-c("02109", "02110", 
             "02111", "02112", 
             "02113", "02114", 
             "02196", "02201", 
             "02203", "02205", 
             "02211", "02212", 
             "02222", "02241", 
             "02283", "02297")
  drchstr<-c("02122", "02124", 
             "02125")
  eastbos<-c("02128","02228")
  fenwayk<-c("02115","02215", 
             "02123")
  hydeprk<-c("02136","02137")
  roxbury<-c("02119", "02120", 
             "02121")
  sothbos<-c("02127","02210")
  
  
  fio <- fio %>% mutate(
    neighborhood = case_when(
      zip %in% allston ~ "Allston",
      zip %in% backbay ~ "Backbay",
      zip == "02130" ~ "Jamaica Plain",
      zip == "02126" ~ "Mattapan",
      zip == "02131" ~ "Roslindale",
      zip == "02129" ~ "Charlestown",
      zip == "02132" ~ "West Roxbury",
      zip == "02118" ~ "South End",
      zip %in% central ~ "Central Boston",
      zip %in% drchstr ~ "Dorchester",
      zip %in% eastbos ~ "East Boston",
      zip %in% fenwayk ~ "Fenway/Kenmore",
      zip %in% hydeprk ~ "Hyde Park",
      zip %in% roxbury ~ "Roxbury",
      zip %in% sothbos ~ "South Boston"
    )
  )
  
  ### convert zip to neighborhood
  
  png(filename="~/Documents/GitHub/crimeviz/plots/hist_plot_fio_stops.png", width= 600, height=1000)
  
  fio %>% select(neighborhood, race) %>%
    filter(is.na(neighborhood) == FALSE & race != "") %>%
    ggplot() +
    geom_bar(aes(y=neighborhood,fill=race), position = position_stack())+
    scale_fill_manual(values = pal) +
    labs(title = "Breakdown of FIO stops by zip code",
         subtitle="stops grouped by zip code, neighborhood",
         caption = attribution)+
    theme_minimal()+
    xlab("")+
    ylab("")+
    theme(legend.position = "bottom")
  
  
  