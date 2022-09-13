

library(rsdmx)
library(lubridate)
library(dplyr)
library(stringr)
library(ggplot2)
library(ggthemes)
library(zoo)

graph_width = 20
graph_height = 15

link_graph =   Sys.getenv("HOME")

sdmx <- readSDMX(providerId = "ECB", resource = "data", flowRef = "MIR",
                 key = "M.U2+DE+FR+IT+ES.B.A2C.A.R.A.2250.EUR.N",verbose=F,start=2004)
df <- as.data.frame(sdmx)
df$date<-as.Date(as.yearmon(df$obsTime))



breaks = c("U2", "DE", "FR","IT","ES")
labels = c("Zone euro","Allemagne","France","Italie","Espagne")

last_values = df %>%
  filter(date == max(date)) %>%
  select(obsTime, obsValue, BS_ITEM, REF_AREA) %>%
  mutate(obsTime = as.Date(paste0(obsTime,"-01")))

date_caption = paste0(month(last_values[1,"obsTime"], label = T, abbr = F), " ",
                      year(last_values[1,"obsTime"]), " -")

last_values = as.data.frame(last_values)
caption = date_caption
for(i in 1:nrow(last_values)){
  last_values[i,"REF_AREA_label"] = labels[which(breaks[]==last_values[i,"REF_AREA"])]
  caption = paste0(caption, sprintf(" %s: %s%% ", last_values[i,"REF_AREA_label"], round(last_values[i,"obsValue"],1)))
}



gg_credit_menage = 
ggplot(data=df,
       aes(x=date,y=obsValue,color=REF_AREA,group=REF_AREA)) + 
  geom_line(size=1.2) +
  ggtitle("Taux des crédits nouveaux aux ménages
 (%), BCE") +
  scale_x_date(date_breaks = "2 years",date_labels = "%Y") + 
  labs(x= "Date", y ="", caption = caption,
       subtitle=paste("Dernier point:",
                      as.yearmon(df$obsTime[dim(df)[1]]))) +
  theme_stata() + 
  guides(color=guide_legend("")) + 
  theme(axis.text.y = element_text(angle=0),
        text = element_text(size=18),
        plot.caption = element_text(hjust= 0.5),
        panel.grid.major.y=element_line(colour="black",size = 0.5,
                                        linetype = 1),
        plot.subtitle=element_text(size=12, hjust=0.5, face="italic", color="black")) + 
  scale_colour_discrete(
          breaks=c("U2", "DE", "FR","IT","ES"),
          labels =c("Zone euro","Allemagne","France","Italie","Espagne")) 

gg_credit_menage %>% ggsave(file=file.path(link_graph, "taux_credit_nv_menage.pdf" ),
         width = graph_width, height = graph_height)

