
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

link_cahierFI_graph = Sys.getenv("HOME")
file_name = paste0("graph_EU_inflation_forecasts", ".pdf" )
file_graph = file.path(link_cahierFI_graph, file_name)

readSDMX2 = rsdmx::readSDMX

yearc = lubridate::year(today())

path_ECB_FM = "https://sdw-wsrest.ecb.europa.eu/service/data/SPF/"
myUrl <- paste0(path_ECB_FM, "Q.U2..POINT+SUM..Q.")
# myUrl <- paste0(path_ECB_FM,"Q.U2..POINT+SUM.LT.Q.")
dt <- readSDMX2(myUrl)
dt <- as.data.frame(dt)

data = dt %>% 
  mutate(date = lubridate::yq(obsTime)) %>% 
  mutate(year_date = year(date)) %>% 
  filter(FCT_HORIZON %in% c(yearc, yearc+1, "LT")) %>% 
  filter(year_date %in% c(yearc, yearc-1)) %>% 
  filter(UNIT == "PCPA") %>% 
  filter(FCT_TOPIC %in% c("HICP", "CORE"))

last_release_quarter = 
  data %>%
  filter(date == max(date)) %>% 
  mutate(quarter = quarter(date)) %>% 
  pull(quarter) %>% 
  unique()

if(last_release_quarter %in% c(1,2)){
  LT_year = yearc+4
}else{
  LT_year = yearc+5
}

data = data %>% 
  mutate(FCT_TOPIC_label = case_when(FCT_TOPIC == "HICP" ~ "Inflation",
                                     FCT_TOPIC == "CORE" ~ "Inflation sous-jacente")) %>% 
  mutate(horizon = case_when(FCT_HORIZON == "LT" ~ sprintf("- Horizon : long terme - %s", LT_year),
                             TRUE ~ paste("- Horizon :", as.character(FCT_HORIZON)))) %>% 
  mutate(var_horizon = paste(FCT_TOPIC_label, horizon))

list_var = data %>% distinct(TITLE)

data_point = data %>% 
  filter(!FCT_SOURCE %in% c("AVG", "VAR", "NUM")) 

data_line = data %>% 
  filter(FCT_SOURCE %in% c("AVG")) 

data_variance = data %>% 
  filter(FCT_SOURCE %in% c("VAR")) 

gg = 
ggplot() +
  geom_point(data=data_point, aes(x = obsTime, y = obsValue), group=1) +
  geom_line(data=data_line, aes(x = obsTime, y = obsValue), colour = "red", group=1) +
  geom_point(data=data_line, aes(x = obsTime, y = obsValue), colour = "red", group=1) +
  facet_wrap(~var_horizon) +
  ggthemes::theme_stata() +
  ggtitle("Prévisions d'inflation en zone euro - enquête au près des prévisionnistes") +
  labs(subtitle = "Source: BCE", caption = "") +
  theme(
    plot.title   = element_text(lineheight = 0.8, face = "bold", hjust = 0.5, size = 18),
    axis.text.x  = element_text(angle = 45, hjust = 1),
    axis.text.y  = element_text(angle = 0, hjust = 1),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.title = element_blank(),
    legend.position = "bottom"
  ) 


gg %>%  ggsave(filename = file_graph, width = 12, height = 7)
