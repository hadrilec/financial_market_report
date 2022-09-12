
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
library(pRev)

#  LANGUAGE="fr_FR.utf8"

link_cahierFI_graph = "M:/Usuels.dsc/pRev/FI/cahier_FI/graph"

file_name = paste0("graph_US_labor_market", ".pdf" )
file_graph = file.path(link_cahierFI_graph, file_name)

fredr_set_key("1e1376b050a44076281adda2fe2e1a32")

us_unem_rate = fredr(series_id = "UNRATE",
                         observation_start = as.Date("2007-01-01"))

us_unem_rate[,"label"] = "Chômage"
us_unem_rate[,"type"] = "Chômage en %"

us_hire = fredr(series_id = "JTSHIL",
                     observation_start = as.Date("2007-01-01"))
us_hire[,"value"] = us_hire[,"value"] / 1000
us_hire[,"label"] = "Embauches"
us_hire[,"type"] = "Emploi en millions"

us_separation = fredr(series_id = "JTSTSL",
                     observation_start = as.Date("2007-01-01"))
us_separation[,"value"] = us_separation[,"value"] / 1000
us_separation[,"label"] = "Séparations"
us_separation[,"type"] = "Emploi en millions"

max_values = us_unem_rate %>%
  arrange(desc(date)) %>%
  slice(1:3) %>%
  mutate(month = month(date, label = T, abbr = T)) %>%
  mutate(month = gsub("\\\\.","", month))

data = rbind(us_separation, us_hire, us_unem_rate)

data_time = data$date

subtitle_month = gsub("\\\\.","", lubridate::month(max(data_time), label = TRUE))

subtitle_year = lubridate::year(max(data_time))

xaxis_breaks = seq.Date(from = min(data_time), to = max(data_time), by = "6 months")


graph_subtitle_ = sprintf("%s : %s%%, %s : %s%%, %s : %s%% ",
                        max_values[1,"month"], max_values[1,"value"],
                        max_values[2,"month"], max_values[2,"value"],
                        max_values[3,"month"], max_values[3,"value"])

graph_subtitle =
  sprintf("Dernier point : %s %s, Chômage %s, source : Bureau of Labor Statistics",
                         subtitle_month, subtitle_year, graph_subtitle_)

graph_us_labor_market = ggplot(data = data,
                            aes(x = date, y = value, colour = label)) +
  facet_wrap(~type, scales = "free") +
  geom_line(size = 1) +
  ggtitle("Marché du travail américain") +
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

data %>%
  filter(date >= "2019-07-01") %>%
  filter(series_id == "UNRATE")

graph_us_labor_market + ggsave(filename = file_graph, width = 12, height = 7)

export_graph(graph_us_labor_market, perim = "FI", folder_name = "us_labor_market", update = TRUE)
