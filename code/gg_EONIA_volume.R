

library(lubridate)
library(dplyr)
library(lubridate)
library(ggplot2)

# EONIA
sdmx <- readSDMX(providerId = "ECB", resource = "data", flowRef = "EON",
                 key = "D.EONIA_TO.VOLUME",verbose=F)
df<-as.data.frame(sdmx)

# ESTER
ESTER = as.data.frame(
  readSDMX(providerId = "ECB", resource = "data", flowRef = "EST",
           key = "B.EU000A2X2A25.TT",verbose=F)
) %>% mutate(label = "ESTER")

start_date = as.Date('2015-01-01')

df = df %>% 
  filter(obsTime >= start_date) %>% 
  mutate(label = "EONIA") %>% 
  select(obsValue, obsTime, label) %>% 
  bind_rows(ESTER)

min_date = start_date
max_date = today() %m+% months(1)

xaxis_breaks = seq.Date(from = start_date, to = max_date, by = "3 months")

link_graph = Sys.getenv("HOME")

gg_EONIA_volume = 
  ggplot(data=df,aes(x=as.Date(obsTime),y=obsValue/1000, colour = label)) + 
  geom_line() +
  ggtitle("Volume de transactions au jour-le-jour sur le marche monetaire européen") +
  labs(x= "", y ="Milliards",subtitle=paste(" BCE, Dernier point:",format(as.Date(df$obsTime[dim(df)[1]]), "%a %d %b"))) +
  scale_x_date(breaks = xaxis_breaks, date_labels = "%b %y") + 
  theme_stata() + 
  theme(axis.text.y = element_text(angle=0),
        axis.text.x = element_text(angle=45, hjust = 1),
        text = element_text(size=18),
        panel.grid.major.y = element_line(colour="black", 
                                          size = 0.5,
                                          linetype = 1),
        plot.subtitle=element_text(size=12, hjust=0.5, face="italic", color="black")) 
 
gg_EONIA_volume %>%  ggsave(file=file.path(link_graph, "transaction_marche_monetaire_euro.pdf" ),
         width = 15, height = 10)

# gg_EONIA_volume