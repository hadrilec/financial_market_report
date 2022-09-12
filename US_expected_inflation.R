
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

link = 'https://api.db.nomics.world/v22/series/SCSMICH/MPX1PX5?limit=1000&offset=0&q=inflation%20michigan&observations=1&align_periods=1&dimensions=%7B%7D'
data <- rdbnomics::rdb_by_api_link(link)

data = data %>%
  filter(period >= '2007-01-01')

link_cahierFI_graph = Sys.getenv("HOME")

file_name = paste0("graph_US_expected_inflation", ".pdf" )
file_graph = file.path(link_cahierFI_graph, file_name)

data_time = data$period

subtitle_month = gsub("\\\\.","",
                      lubridate::month(max(data_time), label = TRUE))

subtitle_year = lubridate::year(max(data_time))

xaxis_breaks = seq.Date(from = min(data_time), to = max(data_time), by = "6 months")

max_cpi = data %>%
  group_by(series_name) %>%
  filter(period == max(period)) %>%
  mutate(month = month(period, label = T, abbr = T)) %>%
  mutate(month = gsub("\\\\.","", month)) %>%
  mutate(value = round(value, 1))

subtitle_data = sprintf("%s: %s%%, %s: %s%%",
                        max_cpi[1,"series_name"], max_cpi[1,"value"],
                        max_cpi[2,"series_name"], max_cpi[2,"value"])


graph_subtitle = sprintf("Dernier point : %s %s, source : University of Michigan\n %s",
                         subtitle_month, subtitle_year, subtitle_data)

yaxis_breaks = seq(from = floor(min(data$value, na.rm = TRUE)/ 0.5) * 0.5,
                   to = ceiling(max(data$value, na.rm = TRUE)/ 0.5) * 0.5, by = 0.5)
# scale_y_continuous(sec.axis = dup_axis(), breaks = yaxis_breaks, labels = function(x) paste0(x, "%")) +


graph_us_exp_inflation = ggplot(data = data,
                            aes(x = period, y = value, colour = series_name)) +
  geom_line(size = 1) +
  geom_point() +
  ggtitle("Inflation anticipée par les ménages US") +
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

graph_us_exp_inflation

graph_us_exp_inflation %>%  ggsave(filename = file_graph, width = 12, height = 7)

