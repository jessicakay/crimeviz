# fio breakdown by location
# jkant@bu.edu | github.com/jessicakay/crimeviz

library(stringr)
fio$zip<-stringr::str_extract(fio$zip,"[[:digit:]]+")
fio %>% select(zip) %>% filter(is.na(zip)==FALSE) %>%
  ggplot()+
  geom_bar(aes(y=zip))
