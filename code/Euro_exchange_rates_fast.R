
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

link_cahierFI_graph = Sys.getenv("HOME")

file_name = paste0("graph_euro_exchange_rates_fast", ".pdf" )
file_graph = file.path(link_cahierFI_graph, file_name)

start_time = today() %m-% months(12)
# start_time = as.Date(paste0(year(today()),"-01-01"))  

webstat_client_ID <- 'd85fb7da-a306-469f-9b60-2e35d251611b'

eur_jpy = rwebstat::w_data(dataset_name = "EXR", series_name = "D.JPY.EUR.SP00.A") %>% 
  mutate(label = "YEN / EURO") %>% 
  mutate(value = EXR.D.JPY.EUR.SP00.A) %>% 
  select(date, value, label)

eur_gbp = rwebstat::w_data(dataset_name = "EXR", series_name = "D.GBP.EUR.SP00.A") %>% 
  mutate(label = "POUND / EURO") %>% 
  mutate(value = EXR.D.GBP.EUR.SP00.A) %>% 
  select(date, value, label)

eur_usd = rwebstat::w_data(dataset_name = "EXR", series_name = "D.USD.EUR.SP00.A") %>% 
  mutate(label = "DOLLAR / EURO") %>% 
  mutate(value = EXR.D.USD.EUR.SP00.A) %>% 
  select(date, value, label) %>% 
  as.data.frame()

# eur_usd[nrow(eur_usd)+1,] = c("2020-03-10", 1.14, "DOLLAR / EURO")
eur_usd = eur_usd %>% 
  mutate(value = as.numeric(value))

eur_cny = rwebstat::w_data(dataset_name = "EXR", series_name = "D.CNY.EUR.SP00.A") %>% 
  mutate(label = "YUAN / EURO") %>% 
  mutate(value = EXR.D.CNY.EUR.SP00.A) %>% 
  select(date, value, label)

data = bind_rows(eur_usd, eur_gbp, eur_jpy, eur_cny) %>% 
  drop_na() %>% 
  dplyr::filter(date >= start_time)

last_values = data %>% 
  group_by(label) %>% 
  dplyr::filter(date == max(date)) %>% 
  mutate(value = round(value, 2)) 

subtt = sprintf("Dernier point : %s, Fait le : %s\n",
                max(last_values$date), gsub("CET","", Sys.time()))

for(i in 1:nrow(last_values)){
  subtt = paste0(subtt, sprintf(" %s : %s,  ", last_values[i,"label"],  last_values[i,"value"]))
}

graph_eur_exr_fast = 
  ggplot(data = data, aes(x = date, y = value, colour = label)) +
  facet_wrap(~label, scales = "free") +
  geom_line(size = 1) +
  labs(subtitle = subtt) +
  scale_y_continuous(position = "right") +
  ggtitle("Taux de change de l'euro") +
  ggthemes::theme_stata() +
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

graph_eur_exr_fast %>%  ggsave(filename = file_graph, width = 12, height = 7)
  