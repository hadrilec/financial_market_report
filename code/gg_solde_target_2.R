
library(rsdmx)
library(ggplot2)

link_graph = Sys.getenv("HOME")

sdmx <- readSDMX(providerId = "ECB", resource = "data", flowRef = "TGB",
                 key = "M.DE+FR+IT+ES.N.A094T.U2.EUR.E",verbose=F)
df <- as.data.frame(sdmx)
df$date<-as.Date(as.yearmon(df$obsTime))


gg_solde_target_2 =
ggplot(data = df,
       aes(x = date, y = obsValue, color = REF_AREA, group = REF_AREA)) + 
  geom_line(size=1.2) + 
  ggtitle("Soldes Target 2, BCE") + 
  scale_x_date(date_breaks = "2 years",date_labels = "%Y") + 
  labs(x = "Date", y ="Solde",
       subtitle=paste("Dernier point:",as.yearmon(df$obsTime[dim(df)[1]]))) + 
  theme_stata()+
  guides(color=guide_legend("")) + 
  theme(plot.subtitle = element_text(size=10, hjust=0.5, face="italic", color="black")) 
  
gg_solde_target_2 %>% ggsave(file=file.path(link_graph, "solde_target_2.pdf" ),
         width = 15, height = 10)

