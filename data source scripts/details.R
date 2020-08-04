details <- read.csv("~/../Desktop/details.csv")

library(dplyr)
library(tidyverse)

d<-details %>%
  select(employee_name,minutes_worked) %>% 
  group_by(employee_name)

distinct(details,employee_name)
