library(tidyverse)
library(lubridate)

data<-read_csv("data/2022-07-cath-volumes.csv")%>%
  mutate(Date = as_date(Date), 
         date_time = ymd_hms(str_c(Date, Time)),
         status = case_when(Volume<30 ~ "<30", 
                            Volume>=30 & Volume<=40 ~ "30<x<40",
                            Volume>40 ~ ">40"
         ))

ggplot(data = data, aes(x = Volume))+
  geom_histogram(binwidth =  10)+
  theme_bw()+
  geom_vline(xintercept = 25, linetype="dashed")+
  geom_vline(xintercept  =45, linetype="dashed")+
  ggtitle("All cath volumes")

data_last_3<-read_csv("cath-volumes-last-3.csv")%>%
  mutate(Date = as_date(Date),
         date_time = ymd_hms(str_c(Date, Time)),
         status = case_when(Volume<30 ~ "<30", 
                            Volume>=30 & Volume<=40 ~ "30<x<40",
                            Volume>40 ~ ">40"
                   ))

data_last_3%>%
  ggplot( aes(x = Volume))+
  geom_histogram(binwidth =  10)+
  theme_bw()+
  geom_vline(xintercept = 25, linetype="dashed")+
  geom_vline(xintercept = 45, linetype="dashed")+
  ggtitle("Cath volumes last 3 days")

data%>%
  count(as.character(Volume))

data%>%
  count(status)

data_last_3%>%
  count(status)

ggplot(data = data, 
       aes(x = date_time, 
           y = Volume))+
  geom_jitter()+
  theme_bw()+
  geom_hline(yintercept = 28, linetype="dashed")+
  geom_hline(yintercept  =42, linetype="dashed")+
  geom_smooth(method='lm')


model<-lm(Volume ~ date_time, data = data)
summary(model)

ggplot(data = data_last_3, 
       aes(x = date_time, 
           y = Volume))+
  geom_jitter()+
  theme_bw()+
  geom_hline(yintercept = 28, linetype="dashed")+
  geom_hline(yintercept  =42, linetype="dashed")+
  geom_smooth(method='lm')


model<-lm(date_time ~ Volume, data = data_last_3)
summary(model)

average_per_day <- data%>%group_by(Date) %>% 
  summarise(mean = mean(Volume, na.rm = TRUE),
            sd = sd(Volume, na.rm = TRUE))

ggplot(data = average_per_day, 
       aes(x = Date, 
           y = mean))+
  geom_line()+
  theme_bw()+ 
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2,
                          position=position_dodge(.9)) + 
  geom_hline(yintercept = 30, linetype="dashed")+
  geom_hline(yintercept  =40, linetype="dashed")

ggplot(data = average_per_day, 
       aes(x = Date, 
           y = mean))+
  geom_line()+
  theme_bw()+ 
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2,
                position=position_dodge(.9)) + 
  geom_hline(yintercept = 30, linetype="dashed")+
  geom_hline(yintercept  =40, linetype="dashed")


ggplot(data = data, 
       aes(x = Date, 
           y = Volume, 
           color = as.character(Time)))+
  geom_line()+
  theme_bw()


ggplot(data = data, 
       aes(x = Time, 
           y = Volume))+
  geom_point()+
  theme_bw()


average_by_time <- data%>%group_by(Time) %>% summarise(mean = mean(Volume, na.rm=TRUE))

ggplot(data = average_by_time, 
       aes(x = Time, 
           y = mean))+
  geom_line()+
  theme_bw()

ggplot(data = data, 
       aes(x = Date, 
           y = Volume))+
  geom_jitter()+
  theme_bw()+ 
  geom_hline(yintercept = 28, linetype="dashed")+
  geom_hline(yintercept = 42, linetype="dashed")

ggplot(data = data, 
       aes(x = as.character(Date), 
           y = Volume))+
  geom_boxplot()+
  theme_bw()+ 
  geom_hline(yintercept = 30, linetype="dashed")+
  geom_hline(yintercept = 40, linetype="dashed")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(data = data, 
       aes(x = as.character(Date), 
           y = Volume,
           color = as.character(Date)))+
  geom_jitter()+
  theme_bw()+ 
  geom_hline(yintercept = 30, linetype="dashed")+
  geom_hline(yintercept = 40, linetype="dashed")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(data = data, 
       aes(x = date_time, 
           y = Volume))+
  geom_point()+
  theme_bw()+ 
  geom_hline(yintercept = 29, linetype="dashed")+
  geom_hline(yintercept = 41, linetype="dashed")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  ggtitle("Cath volumes over time")
