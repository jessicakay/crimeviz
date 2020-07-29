fio %>% ggplot( mapping = aes(basis,position="stacked")) +
  geom_bar() +
  labs(title="FIO searches, January-September 2019",
       subtitle = "Role Of Anti-Black Bias in \"community policing\", breakdown by gender")+
  xlab("")+
  ylab("number of stops")+
  scale_fill_manual(name="searched",values = c("darkgray","firebrick"))+
  facet_grid(.~race)


png(filename="~/../Documents/Github/crimeviz/plots/fio_unstratified.png", width= 800, height=800)

fio<-read.csv("~/../Desktop/rms_fio_2019.csv",stringsAsFactors = FALSE)

p %>% ggplot( mapping = aes(basis,position="stacked")) +
  geom_bar() +
  coord_flip() +
  labs(title="FIO searches, January-September 2019",
       subtitle = "Role Of Anti-Black Bias in \"community policing\", breakdown by gender")+
  xlab("")+
  ylab("number of stops")+
  scale_fill_manual(name="searched",values = c("darkgray","firebrick"))


dev.off()

round(prop.table(table(fio$basis)),2)

round(prop.table(table(fio$basis,fio$race)),2)

as.data.frame

View(table(p$basis,p$race,p$frisked,p$sex))
View(round(prop.table(table(p$basis,p$race,p$frisked,p$sex),margin=1),2))

View(round(prop.table(table(p$basis,p$race,p$searchperson,p$sex),margin=1),2))
View(round(prop.table(table(p$basis,p$race,p$searchvehicle,p$sex),margin=1),2))
View(round(prop.table(table(p$basis,p$race,p$summonsissued,p$sex),margin=1),2))

write.table(sqldf::sqldf('select distinct(race) from fio'),"clipboard",sep="\t")

