# jkant@bu.edu

library(lubridate)
library(tidyverse)
library(stringr)
library(sqldf)
library(gridExtra)


attribution <- "github.com/jessicakay/crimeviz"

# FIO, 2019 data merged by Jackie Jahn

oldfio_data<-read.csv("/Users/jessa/Downloads/old computer/Desktop/rms_fio_2019.csv",stringsAsFactors = FALSE)

fio_data<-read.csv("/Users/jessa/OneDrive/Desktop/mark43_fieldcontacts_for_public_2020_202104151551.csv",stringsAsFactors = FALSE)
fio_name<-read.csv("/Users/jessa/OneDrive/Desktop/mark43_fieldcontacts_name_for_public_2020_202104151551.csv",stringsAsFactors = FALSE)
fio<- fio_data %>% inner_join(fio_name,by="fc_num")

# clean data

fio$zip<-paste("0",fio$zip,sep="")
fio$city[fio$city=="BSTN"]<-"Boston"
fio$city[fio$city=="BOSTON"]<-"Boston"
dorchester<-c("DDORCHESTER","DORCH","DORCHESTER","DORCHSTER","DORCCHESTER","DOR","DORCHESTERR")
boston<-c("BOSTON","BSTN","BTSN","BSNT","BSTNA","BSTON","boston","Boston")
fio$city[fio$city %in% dorchester]<-"DORCHESTER"
fio$city[fio$city %in% boston]<-"BOSTON"

# zip codes from Nathan Story @ wokewindows.org (not census), arrays/grouping by JK

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

oldfio_data <- oldfio_data %>% mutate(
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



  # palette to highlight anti-black bias in BPD
  
  pal <- c("gray51","firebrick","gray63","gray69","gray76","gray82","gray88")

# factor months before piping into tidy
  
fio$mnth<-lubridate::month(fio$contact_date.x)
fio$mnth<-factor(fio$mnth,levels=c(1:12),labels=c("January","February","March",
                                                  "April","May","June","July",
                                                  "August","September","October",
                                                  "November","December")) 

oldfio_data$mnth<-lubridate::month(oldfio_data$contact_date.x)
oldfio_data$mnth<-factor(oldfio_data$mnth,levels=c(1:12),labels=c("January","February","March",
                                                                    "April","May","June","July",
                                                                    "August","September","October",
                                                                    "November","December")) 
# build individual plots

oldfio_data %>% 
    select(neighborhood, race,contact_date.x) %>%
    filter(is.na(neighborhood) == FALSE & race != "") %>%
    mutate(mnth=lubridate::month(contact_date.x)) %>%
    filter(mnth %in% c(1:8)) %>%
    ggplot() +
    geom_bar(aes(y=neighborhood,fill=race), position = position_stack())+
    scale_fill_manual(values = pal) +
    labs(title = "Breakdown of FIO stops by zip code, 2019",
         subtitle="January - September",
         caption = "")+
    theme_minimal()+
    xlab("")+
    ylab("")+
    xlim(... = c(0,3500))+
    theme(axis.text.x = element_text(angle = 90))+
    coord_flip()+
    theme(legend.position = "bottom") -> plot_a

  fio %>% select(neighborhood, race, contact_date.x) %>%
    filter(is.na(neighborhood) == FALSE & race != "") %>%
    mutate(mnth=lubridate::month(contact_date.x)) %>%
    filter(mnth %in% c(1:8)) %>%
    ggplot() +
    geom_bar(aes(y=neighborhood,fill=race), position = position_stack())+
    scale_fill_manual(values = pal) +
    labs(title = "Breakdown of FIO stops by zip code, 2020",
         subtitle="January - September",
         caption = attribution)+
    theme_minimal()+
    xlab("")+
    ylab("")+
    xlim(... = c(0,3500))+
    coord_flip()+
    theme(axis.text.x = element_text(angle = 90))+
    theme(legend.position = "bottom") -> plot_b
  
  pal<-c("lightgrey","firebrick")

  oldfio_data %>% select(neighborhood, race, circumstance,contact_date.x,mnth) %>%
    filter(is.na(neighborhood) == FALSE & race != "") %>%
    mutate(blackroxbury=ifelse(neighborhood=="Roxbury" & race=="Black" , "Red", "lightgrey" ) ) %>%
    filter(mnth %in% c("May","June","July","August","September")) %>%
    ggplot() +
    geom_bar(aes(y=neighborhood,fill=blackroxbury), position = position_stack(),show.legend = FALSE)+
    scale_fill_manual(values = pal) +
    theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),panel.background = element_blank())+
    xlab("")+
    ylab("")+
    xlim(... = c(0,700))+
    coord_flip()+
    theme(axis.text.y = element_text(angle=0), axis.text.x = element_text(angle=90,hjust=1) )+
    theme(legend.position = "bottom")+
    facet_grid(.~mnth) -> plot_c
  
  fio %>% select(neighborhood, race, circumstance,contact_date.x,mnth) %>%
    filter(is.na(neighborhood) == FALSE & race != "") %>%
    mutate(blackroxbury=ifelse(neighborhood=="Roxbury" & race=="Black" , "Red", "lightgrey" ) ) %>%
    filter(mnth %in% c("May","June","July","August","September")) %>%
    ggplot() +
    geom_bar(aes(y=neighborhood,fill=blackroxbury), position = position_stack(),show.legend = FALSE)+
    scale_fill_manual(values = pal) +
    theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),panel.background = element_blank())+
    xlab("")+
    ylab("")+
    coord_flip()+
    xlim(... = c(0,700))+
    theme(axis.text.y = element_text(angle=0), axis.text.x = element_text(angle=90,hjust=1) )+
    theme(legend.position = "bottom")+
    facet_grid(.~mnth) -> plot_d

  # make it big
  
  png(filename = "fio2019v2020_plot.png",height = 1800,width = 2000)
      a_b <- gridExtra::grid.arrange(plot_a,plot_b,ncol=2)
      c_d <- gridExtra::grid.arrange(plot_c,plot_d,ncol=2)
      grid.arrange(c_d,a_b,heights=c(1,2))
  dev.off()
  