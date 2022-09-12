
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

date = gsub("-","",Sys.Date())
link_cahierFI_graph = Sys.getenv("HOME")

file_name = paste0("graph_balance_sheet_gdp", ".pdf" )
file_graph = file.path(link_cahierFI_graph, file_name)
start_time = as.Date("2007-01-01")

fredr_set_key("1e1376b050a44076281adda2fe2e1a32")

bs_boj = fredr(series_id = "JPNASSETS", observation_start = start_time) %>%
  mutate( year = lubridate::year(date),
          quarter = lubridate::quarter(date)) %>%
  group_by(year, quarter) %>%
  summarize(value_ = mean(value)) %>%
  mutate(label = "Bank of Japan",
         type = "Bank of Japan") %>%
  mutate(Mday = case_when(quarter == 1 ~ "-01-01",
                           quarter == 2 ~ "-04-01",
                           quarter == 3 ~ "-07-01",
                           quarter == 4 ~ "-10-01"),
         date = as.Date(paste0(year, Mday))) 


gdp_jp = fredr(series_id = "JPNNGDP", observation_start = start_time) 

bs_boj = bs_boj %>%
  left_join(gdp_jp) %>%
  mutate(ratio = value_ / 10 / value * 100 ) %>%
  ungroup() %>%
  dplyr::select(date, ratio, label, type) %>%
  rename(value = ratio)


bs_fed = fredr(series_id = "WALCL", observation_start = start_time) %>%
  mutate(year = lubridate::year(date),
         quarter = lubridate::quarter(date)) %>%
  group_by(year, quarter) %>%
  summarize(value_ = mean(value)) %>%
  mutate(label = "Federal Reserve Bank",
         type = "European Central Bank and Federal Reserve") %>%
  mutate(Mday = case_when(quarter == 1 ~ "-01-01",
                          quarter == 2 ~ "-04-01",
                          quarter == 3 ~ "-07-01",
                          quarter == 4 ~ "-10-01"),
         date = as.Date(paste0(year, Mday))) %>%
  ungroup() 

gdp_us = fredr(series_id = "GDP", observation_start = start_time)

bs_fed = bs_fed %>%
  left_join(gdp_us) %>%
  mutate(ratio = value_ / 1000 / value * 100 ) %>%
  dplyr::select(date, ratio, label, type) %>%
  rename(value = ratio)

bs_ecb = fredr(series_id = "ECBASSETS", observation_start = start_time) %>%
  mutate(year = lubridate::year(date),
         quarter = lubridate::quarter(date)) %>%
  group_by(year, quarter) %>%
  summarize(value_ = mean(value)) %>%
  mutate(label = "European Central Bank",
         type = "European Central Bank and Federal Reserve") %>%
  mutate(Mday = case_when(quarter == 1 ~ "-01-01",
                          quarter == 2 ~ "-04-01",
                          quarter == 3 ~ "-07-01",
                          quarter == 4 ~ "-10-01"),
         date = as.Date(paste0(year, Mday))) %>%
  ungroup() 

gdp_ea = fredr(series_id = "EUNNGDP", observation_start = start_time) 

gdp_ea_year = gdp_ea %>% 
  mutate(year= lubridate::year(date)) %>% 
  group_by(year) %>% 
  summarise(val = sum(value))

bs_ecb = bs_ecb %>%
  left_join(gdp_ea) %>%
  mutate(ratio = value_ * 100 / value / 4 ) %>%
  dplyr::select(date, ratio, label, type) %>%
  rename(value = ratio)

data = rbind(bs_fed, bs_boj, bs_ecb)

data_time = data$date

subtitle_month = gsub("\\\\.","",
                      lubridate::month(max(data_time), label = TRUE))
subtitle_year = lubridate::year(max(data_time)) 
xaxis_breaks = seq.Date(from = min(data_time), to = max(data_time), by = "6 months")
graph_subtitle = sprintf("Dernier point : %s %s, source : FED, BCE, BOJ",
                         subtitle_month, subtitle_year)

graph_bs_cb_gdp = ggplot(data = data,
               aes(x = data_time, y = value, colour = label )) +
  facet_wrap(~type, scales = "free") +
  geom_line(size = 1) +
  ggtitle("Bilan des banques centrales en proportion du PIB %") +
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

graph_bs_cb_gdp  %>%  ggsave(filename = file_graph, width = 12, height = 7)
