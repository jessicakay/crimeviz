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


