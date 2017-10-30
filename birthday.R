deaths<-read.csv('KoreanConflict.csv',header=TRUE,stringsAsFactors=FALSE)

sum(str_detect(deaths$INCIDENT_DATE,"\\d{8}"))

for(i in 1:36574){
  incident<-str_detect(deaths$INCIDENT_DATE[i],"\\d{8}")
  fatality<-str_detect(deaths$FATALITY[i],"\\d{8}")
  if(incident==FALSE & fatality==TRUE){
    deaths$INCIDENT_DATE[i]<-deaths$FATALITY[i]
  }
  print(i)
}

df<-deaths%>%
  filter(str_detect(deaths$BIRTH_YEAR,"\\d{4}"))%>%
  group_by(BIRTH_YEAR)%>%
  summarize(num_deaths=n())%>%
  select(BIRTH_YEAR,num_deaths)

ggplot()+
  geom_line(data=df,aes(x=BIRTH_YEAR,y=num_deaths))

