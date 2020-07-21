# FIO 

fio_data<-read.csv("~/../Desktop/fieldcontacts_for_public_20192.csv",stringsAsFactors = FALSE)
fio_name<-read.csv("~/../Desktop/fieldcontacts_name_for_public_2019.csv",stringsAsFactors = FALSE)
fio<- fio_data %>% inner_join(fio_name,by="fc_num")

# clean data

fio$zip<-paste("0",fio$zip,sep="")
fio$city[fio$city=="BSTN"]<-"Boston"
fio$city[fio$city=="BOSTON"]<-"Boston"
dorchester<-c("DDORCHESTER","DORCH","DORCHESTER","DORCHSTER","DORCCHESTER","DOR","DORCHESTERR")
boston<-c("BOSTON","BSTN","BTSN","BSNT","BSTNA","BSTON","boston","Boston")
fio$city[fio$city %in% dorchester]<-"DORCHESTER"
fio$city[fio$city %in% boston]<-"BOSTON"
fio$num<-str_extract(fio$streetaddr, "[[:digit:]]+")
fio$num[is.na(fio$num)]<-""
fio$txt<-str_extract(fio$streetaddr, "[[:alpha:][:space:]&]+")


# function to pull distinct values from vectors, includes 
# support for multi-select lists such as "select all that apply" fields. 

getVals<-function(target){
  new_df<<-unlist(strsplit(target,","))
  new_df<<-as.data.frame(trimws(new_df))
  new_df<<-distinct(new_df)
  colnames(new_df)[1]<<-"keywords"
}

write.csv(fio,"~/../Desktop/cleaned.csv")

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

fio$stop_duration<-factor(fio$stop_duration,levels=c("Less Than Five Minutes", 
                          "Five to Ten Minutes", "Ten to Fifteen Minutes",
                          "Fifteen to Twenty Minutes", "Twenty to Twenty-Five Minutes",
                          "Twenty-Five to Thirty Minutes", "Thirty to Forty-Five Minutes",
                          "Forty-Five to Sixty Minutes", "One to Two Hours",
                          "Longer Than Two Hours"),ordered = TRUE)

fio %>%
  filter(sex != "NULL" & sex != "Unknown" & is.na(sex) == FALSE) %>%
  filter(race != "NULL" &
           race != "Unknown" & is.na(race) == FALSE & race != "") %>%
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
  ggplot(mapping = aes(x=reorder(stop_duration,stop_duration))) +
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

fio %>% filter(basis!="Unknown" & basis!="NULL" & is.null(basis)==FALSE) %>%
  filter(race!="Unknown" & race!="NULL" & is.null(race)==FALSE) %>%
  filter(race=="Black" | race=="White") %>%
  ggplot(mapping = aes(basis)) +
  geom_bar() +
  facet_grid(.~race)

fio %>% filter(basis!="Unknown" & basis!="NULL" & is.null(basis)==FALSE) %>%
  filter(race!="Unknown" & race!="NULL" & is.null(race)==FALSE) %>%
  filter(race=="Black" | race=="White") %>%
  filter(ethnicity!="Unknown" & is.null(ethnicity)==FALSE & ethnicity!="NULL") %>%
  ggplot(mapping = aes(basis,fill=race)) +
  geom_bar(position = "dodge")+
  facet_grid(sex~ethnicity)


# qualitative analysis

install.packages("wordcloud")
install.packages("tm")
library(tidyverse)
library(wordcloud)
library(tm)

text_source <- fio %>% 
  select(race, sex, hair_style) 
  
text <- Corpus(VectorSource(text_source)) 
text <- text %>% 
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)

wordcloud(text,scale = c(1,4))


text_source <- fio %>% 
  select(contact_reason) 
text <- Corpus(VectorSource(text_source)) 
text <- text %>% 
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace) %>%
  tm_map(removeWords, stopwords("English"))

wordcloud(text)

table(fio$race)

as.list(text)
