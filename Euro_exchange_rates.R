
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
link_cahierFI_excel = "M:/Usuels.dsc/pRev/FI/cahier_FI/excel"

file_name = paste0("graph_euro_exchange_rates", ".pdf" )
file_graph = file.path(link_cahierFI_graph, file_name)

start_time = as.Date("2014-01-01")  

link = 'https://api.db.nomics.world/v22/series/BDF/EXR/EXR.D.JPY.EUR.SP00.A?observations=1'
eur_jpy <- rdbnomics::rdb(link) %>%
  mutate(type = "Yen japonais", label = "YEN / EURO") %>%
  dplyr::select(period, value, type, label) %>%
  drop_na()

link = 'https://api.db.nomics.world/v22/series/BDF/EXR/EXR.D.GBP.EUR.SP00.A?observations=1'
eur_gbp <- rdbnomics::rdb(link) %>%
  mutate(type = "Livre sterling britannique", label = "POUND / EURO") %>%
  dplyr::select(period, value, type, label) %>%
  drop_na()

link = 'https://api.db.nomics.world/v22/series/BDF/EXR/EXR.D.USD.EUR.SP00.A?observations=1'
eur_usd <- rdbnomics::rdb(link) %>%
  mutate(type = "Dollar américain", label = "DOLLAR / EURO") %>%
  dplyr::select(period, value, type, label) %>%
  drop_na()

link = 'https://api.db.nomics.world/v22/series/BDF/EXR/EXR.D.CNY.EUR.SP00.A?observations=1'
eur_cny <- rdbnomics::rdb(link) %>%
  mutate(type = "Yuan renminbi chinois", label = "YUAN / EURO") %>%
  dplyr::select(period, value, type, label) %>%
  drop_na()

data = rbind(eur_jpy, eur_gbp, eur_usd, eur_cny)
data = data %>% 
  filter(period >= start_time)
  
data_time = data$period

subtitle_month = gsub("\\\\.","",
                      lubridate::month(max(data_time), label = TRUE))

subtitle_year = lubridate::year(max(data_time)) 

xaxis_breaks = seq.Date(from = min(data_time), to = max(data_time), by = "6 months")

graph_subtitle = sprintf("Dernier point : %s %s, source : Banque de France",
                         subtitle_month, subtitle_year)

graph_eur_exr = ggplot(data = data,
                            aes(x = period, y = value, colour = label)) +
  facet_wrap(~type, scales = "free") +
  geom_line(size = 1) +
  ggtitle("Taux de change de l'euro") +
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

graph_eur_exr + ggsave(filename = file_graph, width = 12, height = 7)

export_graph(graph_eur_exr, perim = "FI", folder_name = "eur_exr", update = TRUE)

openxlsx::write.xlsx(data, file = file.path(link_cahierFI_excel, "taux_change.xslx"))


