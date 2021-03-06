library(lubridate)
library(tidyverse)
library(stringr)
library(sqldf)
library(ggplot2)

# jkant@bu.edu

attribution <- "github.com/jessicakay/crimeviz"

# FIO 

oldfio_data<-read.csv("/Users/jessa/Downloads/old computer/Desktop/rms_fio_2019.csv",stringsAsFactors = FALSE)
oldfio_name<-read.csv("/Users/jessa/OneDrive/Desktop/mark43_fieldcontacts_for_public_2020_202104151551.csv",stringsAsFactors = FALSE)
oldfio<- oldfio_data %>% inner_join(oldfio_name,by="fc_num")
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



fio<-read.csv("~/../Desktop/mark43_fio_2019.csv") # obs from dates 9/29/19 - 12/31/19
fio<-read.csv("~/../Desktop/rms_fio_2019.csv") # obs 1/1/2019 - 9/29/2019

fio$stop_duration<-as.numeric(fio$stop_duration)
summary(as.numeric(fio$stop_duration))
table(fio$stop)

png(filename = '~/../Desktop/duration.png', 
    width= 800, height=500)

fio %>%
  filter(!is.na(stop_duration)) %>%
  filter(stop_duration!="NULL") %>%
  filter(sex !="NULL" & sex != "Unknown") %>%
  filter(race !="NULL" & race != "Unknown") %>%
  mutate(stop_duration=as.numeric(stop_duration)) %>%
  ggplot(mapping = aes(stop_duration))+
  geom_histogram(binwidth = 5)+
  xlim(0,90)+
  labs(title = "BPD Field Encounter and Interrogation, 2019", 
       subtitle = "Breakdown of stop duration by race and gender", 
       caption = "github.com/jessicakay/crimeviz")+
  xlab("Duration of stop (mins)")+
  ylab("Number of stops")+
  facet_grid(sex~race)

dev.off()

# import Jackie's cleaned dataset

fio<-read.csv("~/../Desktop/rms_fio_2019.csv",stringsAsFactors = FALSE)

if(Sys.getenv("DESKTOP_SESSION")=="ubuntu"){
  fio<-read.csv("https://raw.githubusercontent.com/jackiejahn/boston-FIO-2019/master/rms_fio_2019.csv",
                stringsAsFactors = FALSE)
}


fio$stop_duration <- factor(
  fio$stop_duration,
  levels = c(
    "Less Than Five Minutes",
    "Five to Ten Minutes",
    "Ten to Fifteen Minutes",
    "Fifteen to Twenty Minutes",
    "Twenty to Twenty-Five Minutes",
    "Twenty-Five to Thirty Minutes",
    "Thirty to Forty-Five Minutes",
    "Forty-Five to Sixty Minutes",
    "One to Two Hours",
    "Longer Than Two Hours"
  ),
  ordered = TRUE
)

fio %>%
  filter(sex != "NULL" & sex != "Unknown" & is.na(sex) == FALSE) %>%
  filter(race != "NULL" &
           race != "Unknown" &
           is.na(race) == FALSE & race != "") %>%
  filter(
    stop_duration != "NULL" & stop_duration != "Unknown" &
      is.na(stop_duration) == FALSE & stop_duration != ""
  ) %>%
  mutate(
    sex = case_when(
      sex == "Male" ~ "Male",
      sex == "Transgender Female to Male" ~ "Male",
      sex == "Female" ~ "Female",
      sex == "Transgender Male to Female" ~ "Female"
    )
  ) %>%
  mutate(
    stop_duration = case_when(
      stop_duration == "Less Than Five Minutes" ~ "<5",
      stop_duration == "Five to Ten Minutes" ~ "5-10",
      stop_duration == "Ten to Fifteen Minutes" ~ "10-15",
      stop_duration == "Fifteen to Twenty Minutes" ~ "15-20",
      stop_duration == "Twenty to Twenty-Five Minutes" ~ "20-25",
      stop_duration == "Twenty-Five to Thirty Minutes" ~ "25-30",
      stop_duration == "Thirty to Forty-Five Minutes" ~ "30-45",
      stop_duration == "Forty-Five to Sixty Minutes" ~ "45-60",
      stop_duration == "One to Two Hours" ~ "60-120",
      stop_duration == "Longer Than Two Hours" ~ ">2 hrs"
    )
  ) %>%
  ggplot(mapping = aes(x = stop_duration)) +
  geom_bar() +
  labs(title = "BPD Field Interrogation and Observation (FIO) data, 2019",
       subtitle = "Breakdown of stop duration by race and gender",
       caption = "github.com/jessicakay/crimeviz") +
  xlab("Duration of stop (mins)") +
  ylab("Number of stops") +
  theme(axis.text.x = element_text(angle = 45)) +
  facet_grid(sex ~ race)

table(fio %>%select(sex))

# FIO analytics

round(prop.table(table(fio$race[fio$frisk.search!="NULL"],
                       fio$frisk.search[fio$frisk.search!="NULL"]),
                 margin = 1),2)

x<-fio %>% 
  select(race,ethnicity,frisk.search,sex) %>%
  filter(frisk.search!="NULL" & is.null(frisk.search)==FALSE) 

prop.table(table(fio$basis,fio$race),margin = 1)

# plot breakdown of stops by basis, stratify by sex/race
# 
# coding notes: sex is converted to gender, as BPD's 
# categorization is based on an erroneous understanding of gender identity

details<-round(prop.table(table(fio$race,fio$sex)),2)
black_f<-details[14]
black_m<-details[15]


fio<-read.csv("~/../Desktop/rms_fio_2019.csv",stringsAsFactors = FALSE)

table(
  fio %>%
    filter(sex == "Male" | sex == "Female") %>%
    filter(race == "Black" | race == "White") %>%
    select(frisked, sex, race, basis)
)


fio$frisked<-as.factor(fio$frisked)
fio$searchperson<-as.factor(fio$searchperson)
levels(fio$frisked)
levels(fio$searchperson)




p<-fio %>% filter(basis!="Unknown" & basis!="NULL" & is.null(basis)==FALSE) %>%
  filter(race!="Unknown" & race!="NULL" & is.null(race)==FALSE) %>%
  filter(race=="Black" | race=="White") %>%
  filter(sex!="NA" & is.na(sex)==FALSE & is.null(sex)==FALSE & 
           is_empty(sex)==FALSE & sex!="") %>%
  filter(sex=="Male" | sex=="Female") %>%
  mutate(
    searchperson=case_when(
      searchperson=="" ~ "Not searched",
      searchperson=="Y" ~ "Searched"),ordered=TRUE)%>%
  mutate(
    frisked=case_when(
    frisked=="" ~ "Not frisked",
    frisked=="Y" ~ "Frisked"))%>%
  mutate(frisked=factor(frisked,levels=c("Not frisked","Frisked")))%>%
    mutate(
    sex = case_when(
      sex == "Male" ~ "Male",
      sex == "Transgender Female to Male" ~ "Male",
      sex == "Female" ~ "Female",
      sex == "Transgender Male to Female" ~ "Female"))



levels(fio$searchperson)

# get caption data...

j<-table(fio %>% filter(frisked == "Y") %>%
                      filter(basis == "Probable Cause") %>%
                      select(frisked, race,sex))
k<-round(prop.table(j),2)
black_num_frisked<-paste(j[30]," ( ",k[36]*100,"% )",sep="")
white_num_frisked<-paste(j[30]," ( ",k[36]*100,"% )",sep="")
j<-table(fio %>% filter(searchperson == "Y") %>%
           filter(basis == "Probable Cause") %>%
           select(searchperson, race,sex))
k<-round(prop.table(j),2)
black_num_searchd<-paste(j[26]," ( ",k[30]*100,"% )",sep="")


# note: this analysis is looking specifically at the role 
# anti-Blackness has in policing in Boston; this is not 
# a comprehensive breakdown of race/ethnicity

search<-p %>% ggplot(fio, mapping = aes(basis,fill=searchperson,position="stacked")) +
  geom_bar() +
  coord_flip() +
  labs(title="FIO searches, January-September 2019",
       subtitle = "Role Of Anti-Black Bias in \"community policing\", breakdown by gender")+
  xlab("")+
  ylab("number of stops")+
  scale_fill_manual(name="searched",values = c("darkgray","firebrick"))+
  facet_grid(sex~race)

frisk<-p %>% ggplot(fio, mapping = aes(basis,fill=frisked,position="stacked")) +
  geom_bar() +
  coord_flip()+
  xlab("")+
  ylab("number of stops")+
  scale_fill_manual(values = c("darkgray","firebrick"))+
  labs(caption=attribution)+
  facet_grid(sex~race)

png(filename="~/../Documents/Github/crimeviz/plots/bias_redgrey.png", width= 800, height=800)
gridExtra::grid.arrange(search,frisk)
dev.off()

# vehicle search, summons issued
searchvehicle<-p %>% 
  mutate(searchvehicle=case_when(
    searchvehicle=="" ~ "No",
    searchvehicle=="Y" ~"Yes")
    ) %>%
  ggplot(fio, mapping = aes(basis,fill=searchvehicle,position="stacked")) +
  geom_bar() +
  coord_flip()+
  xlab("")+
  ylab("number of stops")+
  labs(title="FIO searches, January-September 2019",
       subtitle = "Role Of Anti-Black Bias in \"community policing\", breakdown by gender")+
  scale_fill_manual(name="car search",values = c("darkgray","firebrick"))+
  facet_grid(sex~race)

summons<-p %>% 
  mutate(summonsissued=case_when(
    summonsissued=="" ~ "No",
    summonsissued=="Y" ~"Yes")
    ) %>%
  ggplot(fio, mapping = aes(basis,fill=summonsissued,position="stacked")) +
  geom_bar() +
  coord_flip()+
  xlab("")+
  ylab("number of stops")+
  scale_fill_manual(name="summons",values = c("darkgray","firebrick"))+
  labs(caption=attribution)+
  facet_grid(sex~race)

png(filename="~/../Documents/Github/crimeviz/plots/summons_redgrey.png", width= 800, height=800)
gridExtra::grid.arrange(searchvehicle,summons)
dev.off()

#

fio %>% filter(basis!="Unknown" & basis!="NULL" & is.null(basis)==FALSE) %>%
  filter(race!="Unknown" & race!="NULL" & is.null(race)==FALSE) %>%
  filter(race=="Black" | race=="White") %>%
  filter(ethnicity!="Unknown" & is.null(ethnicity)==FALSE & ethnicity!="NULL") %>%
  ggplot(mapping = aes(basis,fill=race)) +
  geom_bar(position = "dodge")+
  facet_grid(sex~ethnicity)


table(fio$contact_officer_name,fio$frisked,fio$race)

# multiple y axes
install.packages("latticeExtra")


# function to pull distinct values from vectors, includes 
# support for multi-select lists such as "select all that apply" fields. 

getVals <- function(target, d) {
  target<-as.character(target)
  new_df <- unlist(strsplit(target, ","))
  new_df <- as.data.frame(trimws(new_df))
  if (d == 1) {
    new_df <- distinct(new_df)
  }
  colnames(new_df)[1] <- "keywords"
  new_df <<- str_squish(new_df$keywords)
  head(new_df)
}

getWords <- function(target, d) {
  target<-as.character(target)
  new_df <- unlist(strsplit(target, c(","," ")))
  new_df <- as.data.frame(trimws(new_df))
  if (d == 1) {
    new_df <- distinct(new_df)
  }
  colnames(new_df)[1] <- "keywords"
  new_df <<- str_squish(new_df$keywords)
  head(new_df)
}


# qualitative analysis

install.packages("wordcloud")
install.packages("tm")
library(tidyverse)
library(wordcloud)
library(tm)


getVals(fio$otherclothing,1)
textmatrix<-as.matrix(new_df)
text<-Corpus(VectorSource(new_df))  

text <- Corpus(VectorSource(new_df)) 
text <- text %>% 
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)

wordcloud(text)


text_source <- fio %>% 
  select(contact_reason) 

if(Sys.getenv("DESKTOP_SESSION")=="ubuntu"){
  fio<-read.csv("https://raw.githubusercontent.com/jackiejahn/boston-FIO-2019/master/rms_fio_2019.csv",
                stringsAsFactors = FALSE)
}

text_source <- fio %>% 
 select(race, sex, hair_style)

text_source<-as.vector(text_source)
text_source<-gsub("\"","",text_source)
text <- Corpus(VectorSource(text_source)) 
text <- text %>% 
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace) %>%
  tm_map(removeWords, stopwords("English"))

wordcloud(text)

table(fio$race)

as.list(text)




p<-fio %>% filter(basis!="Unknown" & basis!="NULL" & is.null(basis)==FALSE) %>%
  filter(race!="Unknown" & race!="NULL" & is.null(race)==FALSE) %>%
  filter(race=="Black" | race=="White") %>%
  filter(sex!="NA" & is.na(sex)==FALSE & is.null(sex)==FALSE & 
           is_empty(sex)==FALSE & sex!="") %>%
  filter(sex=="Male" | sex=="Female") %>%
  mutate(
    frisk.search=case_when(
      frisk.search=="" ~ "Not frisked",
      frisk.search=="Y" ~ "Frisked"))%>%
  mutate(frisk.search=factor(frisk.search,levels=c("Not frisked","Frisked")))%>%
  mutate(
    sex = case_when(
      sex == "Male" ~ "Male",
      sex == "Transgender Female to Male" ~ "Male",
      sex == "Female" ~ "Female",
      sex == "Transgender Male to Female" ~ "Female"))




p %>%
  ggplot(fio, mapping = aes(basis,fill=frisk.search,position="stacked")) +
  geom_bar(aes(x=frisk.search)) +
  coord_flip()+
  xlab("")+
  ylab("number of stops")+
  scale_fill_manual(name="summons",values = c("darkgray","firebrick"))+
  labs(caption=attribution)+
  facet_grid(sex~race)


f <- fio %>% filter(race=="Black" | race=="White")
prop.table(table(f$frisk.search,f$race),margin=2)

table(fio$race)
