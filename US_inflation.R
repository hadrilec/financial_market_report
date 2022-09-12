
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

file_name = paste0("graph_US_inflation", ".pdf" )
file_graph = file.path(link_cahierFI_graph, file_name)

fredr_set_key("1e1376b050a44076281adda2fe2e1a32")
start_time = as.Date("2000-01-01")

us_core_cpi = fredr(series_id = "CPILFESL", observation_start = start_time)
us_core_cpi = us_core_cpi %>%
  mutate(
         label = "Inflation sous-jacente",
         month_ = lubridate::month(date),
         type = "Inflation") %>%
  mutate(us_infl = (value / dplyr::lag(value, 12) - 1) * 100) %>%
  dplyr::select(-series_id, -value, -month_) %>%
  dplyr::rename(value = us_infl) %>%
  dplyr::select(date, value, label, type) %>%
  drop_na()


# us_cpi = fredr(series_id = "CPALTT01USM659N", observation_start = start_time)
# us_cpi = us_cpi %>% 
#   mutate(
#     label = "Etats-Unis",
#     type = "Inflation") %>%
#   dplyr::select(date, value, label, type)

us_cpi = fredr(series_id = "CPIAUCSL", observation_start = start_time)
us_cpi = us_cpi %>%
  mutate(
    label = "Inflation",
    month_ = lubridate::month(date),
    type = "Inflation") %>%
  mutate(us_infl = (value / dplyr::lag(value, 12) - 1) * 100) %>%
  dplyr::select(-series_id, -value, -month_) %>%
  dplyr::rename(value = us_infl) %>%
  dplyr::select(date, value, label, type) %>%
  drop_na()

max_cpi = us_cpi %>%
  arrange(desc(date)) %>%
  slice(1:3) %>%
  mutate(month = month(date, label = T, abbr = T)) %>%
  mutate(month = gsub("\\\\.","", month)) %>%
  mutate(value = round(value,1))

max_core_cpi = us_core_cpi %>%
  arrange(desc(date)) %>%
  slice(1:3) %>%
  mutate(month = month(date, label = T, abbr = T)) %>%
  mutate(month = gsub("\\\\.","", month)) %>%
  mutate(value = round(value,1))


data = rbind(us_core_cpi, us_cpi)

data_time = data$date

subtitle_month = gsub("\\\\.","", lubridate::month(max(data_time), label = TRUE))

subtitle_year = lubridate::year(max(data_time)) 

xaxis_breaks = seq.Date(from = min(data_time), to = max(data_time), by = "6 months")

graph_subtitle_core_cpi = sprintf("%s : %s%%, %s : %s%%, %s : %s%% ",
                             max_core_cpi[1,"month"], max_core_cpi[1,"value"],
                             max_core_cpi[2,"month"], max_core_cpi[2,"value"],
                             max_core_cpi[3,"month"], max_core_cpi[3,"value"])

graph_subtitle_cpi = sprintf("%s : %s%%, %s : %s%%, %s : %s%% ",
                          max_cpi[1,"month"], max_cpi[1,"value"],
                          max_cpi[2,"month"], max_cpi[2,"value"],
                          max_cpi[3,"month"], max_cpi[3,"value"])

graph_subtitle = sprintf("Dernier point : %s %s, source : BLS \n Inflation %s \n Inflation sous-jacente %s",
                         subtitle_month, subtitle_year,
                         graph_subtitle_cpi, graph_subtitle_core_cpi)

yaxis_breaks = seq(from = floor(min(data$value, na.rm = TRUE)/ 0.5) * 0.5,
                   to = ceiling(max(data$value, na.rm = TRUE)/ 0.5) * 0.5, by = 0.5)
# scale_y_continuous(sec.axis = dup_axis(), breaks = yaxis_breaks, labels = function(x) paste0(x, "%")) +


graph_us_inflation = ggplot(data = data,
                            aes(x = data_time, y = value, colour = label)) +
  facet_wrap(~type, scales = "free") +
  geom_line(size = 1) +
  ggtitle("Inflation aux Etats-Unis") +
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

graph_us_inflation %>%  ggsave(filename = file_graph, width = 12, height = 7)
