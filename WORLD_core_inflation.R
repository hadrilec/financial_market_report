
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

file_name = paste0("graph_WORLD_core_inflation", ".pdf" )
file_graph = file.path(link_cahierFI_graph, file_name)

fredr_set_key("1e1376b050a44076281adda2fe2e1a32")
start_time = as.Date("2002-01-01")

jp_cpi = fredr(series_id = "CPGRLE01JPM657N", observation_start = as.Date("2000-01-01"))
jp_cpi = jp_cpi %>%
  mutate(
         label = "Japon",
         month = lubridate::month(date),
         type = "Inflation-jacente") %>%
  dplyr::select(date, value, label, type)  %>%
  filter(date >= start_time) %>%
  dplyr::select(date, value, label, type) %>%
  drop_na()

us_core_cpi = fredr(series_id = "CPILFESL", observation_start = start_time)
us_core_cpi = us_core_cpi %>%
  mutate(
    label = "Etats-Unis",
    month_ = lubridate::month(date),
    type = "Inflation-jacente") %>%
  mutate(us_infl = (value / dplyr::lag(value, 12) - 1) * 100) %>%
  dplyr::select(-series_id, -value, -month_) %>%
  rename(value = us_infl) %>%
  dplyr::select(date, value, label, type) %>%
  drop_na()

HICP = eurostat::get_eurostat("prc_hicp_manr") %>% 
  filter(geo %in% c("UK","EA"),  # Euro Area
         coicop %in% c("TOT_X_NRG_FOOD_NP"),  #all-items HICP 
         time >= start_time) %>%
  rename(date = time, value = values)

HICP = HICP %>% 
  mutate(label = case_when(geo == "UK" ~ "Royaume-Uni",
                           geo == "EA" ~ "Zone Euro"),
         type = "Inflation-jacente") %>%
  dplyr::select(date, value, label, type)


data = rbind(HICP,  us_core_cpi)  #jp_cpi


data_time = data$date

subtitle_month = gsub("\\\\.","",
                      lubridate::month(max(data_time), label = TRUE))

subtitle_year = lubridate::year(max(data_time)) 

xaxis_breaks = seq.Date(from = min(data_time), to = max(data_time), by = "6 months")

graph_subtitle = sprintf("Dernier point : %s %s, source : Eurostat, BLS, OCDE",
                         subtitle_month, subtitle_year)


yaxis_breaks = seq(from = floor(min(data$value, na.rm = TRUE) / 0.5) * 0.5,
                   to = ceiling(max(data$value, na.rm = TRUE)/ 0.5) * 0.5, by = 0.5)
# scale_y_continuous(sec.axis = dup_axis(), breaks = yaxis_breaks, labels = function(x) paste0(x, "%")) +


graph_world_core_inflation = ggplot(data = data,
                            aes(x = data_time, y = value, colour = label)) +
  facet_wrap(~type, scales = "free") +
  geom_line(size = 1) +
  ggtitle("Inflation dans le monde") +
  labs(subtitle = graph_subtitle) + 
  scale_y_continuous(sec.axis = dup_axis(), breaks = yaxis_breaks, labels = function(x) paste0(x, "%")) +
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

graph_world_core_inflation + ggsave(filename = file_graph, width = 12, height = 7)
