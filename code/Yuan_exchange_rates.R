
library(rdbnomics)
library(rsdmx)
library(ggthemes)
library(grid)
library(lubridate)
library(eurostat)
library(fredr)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(tidyr)


link_cahierFI_graph = "M:/Usuels.dsc/pRev/FI/cahier_FI/graph"

file_name = paste0("graph_yuan_exchange_rates", ".pdf" )
file_graph = file.path(link_cahierFI_graph, file_name)

start_time = as.Date("2014-01-01")  

link = 'https://api.db.nomics.world/v22/series/FED/H10/CNY.9?observations=1'
usd_cny <- rdbnomics::rdb_by_api_link(link) %>%
  dplyr::select(period, value) %>%
  drop_na() %>%
  mutate(type = "Dollar américain", label = "YUAN / DOLLAR")

link = 'https://api.db.nomics.world/v22/series/BDF/EXR/EXR.D.CNY.EUR.SP00.A?observations=1'
eur_cny <- rdbnomics::rdb_by_api_link(link) %>%
  dplyr::select(period, value) %>%
  drop_na() %>%
  mutate(type = "Euro", label = "YUAN / EURO")

data = rbind(usd_cny, eur_cny)

data = data %>% 
  filter(period >= start_time)
  
data_time = data$period

subtitle_month = gsub("\\\\.","",
                      lubridate::month(max(data_time), label = TRUE))

subtitle_year = lubridate::year(max(data_time)) 

xaxis_breaks = seq.Date(from = min(data_time), to = max(data_time), by = "6 months")

graph_subtitle = sprintf("Dernier point : %s %s, source : Banque de France, FED",
                         subtitle_month, subtitle_year)

graph_cny_exr = ggplot(data = data,
                            aes(x = data_time, y = value, colour = label)) +
  facet_wrap(~type, scales = "free") +
  geom_line(size = 1) +
  ggtitle("Taux de change du Yuan chinois") +
  labs(subtitle = graph_subtitle) + 
  scale_y_continuous(position = "right") + 
  scale_x_date(breaks = xaxis_breaks, date_labels = "%b %y" ) +
  theme_stata() +
  theme(
    plot.title   = element_text(lineheight = 0.8, face = "bold", hjust = 0.5, size = 18),
    axis.text.x  = element_text(angle = 45, hjust = 1),
    axis.text.y  = element_text(angle = 0, hjust = 1),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.title = element_blank(),
    legend.position = "bottom"
  ) 

graph_cny_exr + ggsave(filename = file_graph, width = 12, height = 7)
