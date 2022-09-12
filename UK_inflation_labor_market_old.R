
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


link_cahierFI_graph = "M:/Usuels.dsc/pRev/FI/cahier_FI/graph"

file_name = paste0("graph_UK_inflation_labor_market", ".pdf" )
file_graph = file.path(link_cahierFI_graph, file_name)

fredr_set_key("1e1376b050a44076281adda2fe2e1a32")
start_time = as.Date("2012-01-01")

uk_unem_rate = fredr(series_id = "LRHUTTTTGBM156S",observation_start = start_time)

uk_unem_rate[,"label"] = "Chômage"
uk_unem_rate[,"type"] = "Chômage en %"
uk_unem_rate = uk_unem_rate[,-2]

uk_HICP = eurostat::get_eurostat("prc_hicp_manr") %>% 
  filter(geo == "UK",  # Euro Area
         coicop %in% c("CP00",  #all-items HICP
                       "TOT_X_NRG_FOOD_NP"),  # Overall index excluding energy and unprocessed food
         time >= as.Date("2012-01-01", "%Y-%m-%d")
  )

uk_HICP = uk_HICP %>% mutate(name = case_when(coicop == "CP00" ~ "Inflation",
                                        coicop == "TOT_X_NRG_FOOD_NP" ~ "Inflation sous-jacente")) %>%
  dplyr::select(name, values, time) %>%
  dplyr::rename(value = values, date = time, label = name)

uk_HICP[,"type"] = "Inflation"
uk_HICP = uk_HICP[,c("date", "value" ,  "label",     "type"  )] 

data = rbind(uk_HICP, uk_unem_rate)

max_cpi = uk_HICP %>%
  filter(label == "Inflation") %>%
  arrange(desc(date)) %>%
  slice(1:3) %>%
  mutate(month = month(date, label = T, abbr = T)) %>%
  mutate(month = gsub("\\\\.","", month))

max_core_cpi = uk_HICP %>%
  filter(label == "Inflation sous-jacente") %>%
  arrange(desc(date)) %>%
  slice(1:3) %>%
  mutate(month = month(date, label = T, abbr = T)) %>%
  mutate(month = gsub("\\\\.","", month))

max_unem = uk_unem_rate %>%
  arrange(desc(date)) %>%
  slice(1:3) %>%
  mutate(month = month(date, label = T, abbr = T)) %>%
  mutate(month = gsub("\\\\.","", month))

data_time = data$date

subtitle_month = gsub("\\\\.","",
                      lubridate::month(max(data_time), label = TRUE))

subtitle_year = lubridate::year(max(data_time)) 

xaxis_breaks = seq.Date(from = min(data_time), to = max(data_time), by = "6 months")

graph_subtitle = sprintf("Dernier point : %s %s, source : Eurostat",
                         subtitle_month, subtitle_year)

graph_uk_labor_market = ggplot(data = data,
                            aes(x = data_time, y = value, colour = label)) +
  facet_wrap(~type, scales = "free") +
  geom_line(size = 1) +
  ggtitle("Inflation et chômage au Royaume-Uni") +
  labs(subtitle = graph_subtitle) + 
  scale_y_continuous(position = "right", labels = function(x) paste0(x, "%")) +
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

graph_uk_labor_market + ggsave(filename = file_graph, width = 12, height = 7)
