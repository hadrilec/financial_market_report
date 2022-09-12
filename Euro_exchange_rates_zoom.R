
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

file_name = paste0("graph_euro_exchange_rates_zoom", ".pdf" )
file_graph = file.path(link_cahierFI_graph, file_name)

start_time = today() %m-% months(12)
# start_time = as.Date(paste0(year(today()),"-01-01"))  

link = 'https://api.db.nomics.world/v22/series/BDF/EXR/EXR.D.JPY.EUR.SP00.A?observations=1'
eur_jpy <- rdbnomics::rdb(link) %>%
  mutate(type = "Yen japonais", label = "YEN / EURO") %>%
  # select(period, value, type, label) %>%
  drop_na()
eur_jpy = as.data.frame(eur_jpy)
eur_jpy = eur_jpy[,c("period", "value", "type", "label")]

link = 'https://api.db.nomics.world/v22/series/BDF/EXR/EXR.D.GBP.EUR.SP00.A?observations=1'
eur_gbp <- rdbnomics::rdb(link) %>%
  mutate(type = "Livre sterling britannique", label = "POUND / EURO") %>%
  # select(period, value, type, label) %>%
  drop_na()
eur_gbp = as.data.frame(eur_gbp)
eur_gbp = eur_gbp[,c("period", "value", "type", "label")]

link = 'https://api.db.nomics.world/v22/series/BDF/EXR/EXR.D.USD.EUR.SP00.A?observations=1'
eur_usd <- rdbnomics::rdb(link) %>%
  mutate(type = "Dollar américain", label = "DOLLAR / EURO") %>%
  # select(period, value, type, label) %>%
  drop_na()

eur_usd = as.data.frame(eur_usd)
eur_usd = eur_usd[,c("period", "value", "type", "label")]

link = 'https://api.db.nomics.world/v22/series/BDF/EXR/EXR.D.CNY.EUR.SP00.A?observations=1'
eur_cny <- rdbnomics::rdb(link) %>%
  mutate(type = "Yuan renminbi chinois", label = "YUAN / EURO") %>%
  # select(period, value, type, label) %>%
  drop_na()

eur_cny = as.data.frame(eur_cny)
eur_cny = eur_cny[,c("period", "value", "type", "label")]

data = rbind(eur_jpy, eur_gbp, eur_usd, eur_cny)
data = data %>% 
  filter(period >= start_time)
  
usd_last = data %>%
  filter(period == max(period),
         label == "DOLLAR / EURO") %>%
  pull(value) %>%
  round(.,2)

yuan_last = data %>%
  filter(period == max(period),
         label == "YUAN / EURO") %>%
  pull(value) %>%
  round(.,1)

yen_last = data %>%
  filter(period == max(period),
         label == "YEN / EURO") %>%
  pull(value) %>%
  round(.,0)

pound_last = data %>%
  filter(period == max(period),
         label == "POUND / EURO") %>%
  pull(value) %>%
  round(.,2)

data_time = data$period

subtitle_month = gsub("\\\\.","",
                      lubridate::month(max(data_time), label = TRUE))

subtitle_day = gsub("\\\\.","",
                      lubridate::day(max(data_time)))

subtitle_year = lubridate::year(max(data_time)) 

xaxis_breaks = seq.Date(from = min(data_time), to = max(data_time), by = "1 months")

graph_subtitle2 = sprintf("Dernières valeurs 1€ =  %s $   %s £   %s CNY   %s JPY",
                          usd_last, pound_last, yuan_last, yen_last)

graph_subtitle = sprintf("Dernier point : %s %s %s, source : Banque de France,      %s ",
                         subtitle_day, subtitle_month, subtitle_year, graph_subtitle2)



graph_eur_exr_zoom = ggplot(data = data,
                            aes(x = period, y = value, colour = label)) +
  facet_wrap(~type, scales = "free") +
  geom_line(size = 1) +
  # geom_vline(xintercept = as.Date(c("2019-12-19", "2020-01-31", "2020-01-27", "2020-01-09", "2020-01-15")), linetype = "dashed") + 
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
    text = element_text(size = 14),
    strip.text.x = element_text(size = 14),
    legend.position = "bottom"
  ) 

graph_eur_exr_zoom + ggsave(filename = file_graph, width = 12, height = 7)

# graph_eur_exr_zoom2 = graph_eur_exr_zoom 

export_graph(graph_eur_exr_zoom, folder_name = "euro_taux_change", perim = "FI")
# export_graph(graph_eur_exr_zoom, folder_name = "euro_dollar2", perim = "FI")

data_q = data %>% 
  filter(str_detect(label, "DOLLAR")) %>% 
  mutate(quarter_ = lubridate::quarter(period, with_year = TRUE)) %>% 
  filter(str_detect(quarter_, "2020")) %>% 
  group_by(quarter_) %>% 
  drop_na() %>% 
  mutate(mean_value = mean(value)) %>% 
  mutate(inverse_value = 1 /mean_value)

data_euro_dollar = data %>% 
  filter(str_detect(label, "DOLLAR")) %>% 
  arrange(desc(period))
  

data_m = data %>% 
  mutate(month_ = lubridate::month(period)) %>% 
  mutate(year_ = lubridate::year(period)) %>% 
  filter(str_detect(year_, "2020")) %>% 
  group_by(month_, label) %>% 
  drop_na() %>% 
  summarise(mean_value = mean(value)) %>% 
  mutate(inverse_value = 1 /mean_value)


data_values = data %>% 
  group_by(label) %>% 
  filter(period >=  "2019-10-01") %>% 
  filter(value %in% c(max(value), min(value), value[period == max(period)]))