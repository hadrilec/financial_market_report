
library(RJSONIO)
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

getDataUKmonthly = function(datasetID, seriesKey){
  link = sprintf("https://api.ons.gov.uk/dataset/%s/timeseries/%s/data", datasetID, seriesKey)
  
  data = RJSONIO::fromJSON(link)
  data = data[['months']] 
  
  list_df = list()
  
  for(i in 1:length(data)){
    df = as.data.frame(data[[i]])
    date = lubridate::mdy(paste0(df[3,1], "1st, ", df[8,1]))
    list_df[[i]] = data.frame(date = date, value = df[7,1])
  }
  
  return(dplyr::bind_rows(list_df)) 
}

uk_unem = getDataUKmonthly("LMS", "MGSX")

uk_cpi = getDataUKmonthly("MM23", "L55O")

uk_core_cpi = getDataUKmonthly("MM23", "DKO8")
# DKO8

link_cahierFI_graph = Sys.getenv("HOME")

file_name = paste0("graph_UK_inflation_labor_market", ".pdf" )
file_graph = file.path(link_cahierFI_graph, file_name)
start_time = as.Date("2012-01-01")

uk_unem[,"label"] = "Chômage"
uk_unem[,"type"] = "Chômage"

uk_cpi[,"label"] = "Inflation"
uk_cpi[,"type"] = "Inflation"

uk_core_cpi[,"label"] = "Inflation sous-jacente"
uk_core_cpi[,"type"] = "Inflation"

# uk_core_cpi = uk_core_cpi %>%
#   mutate(value = as.numeric(value)) %>%
#   mutate(value = (value / dplyr::lag(value) - 1) * 100)

data = rbind(uk_cpi, uk_core_cpi, uk_unem)

data = data %>% 
  mutate(value = as.numeric(value)) %>%
  filter(date >= start_time)

max_cpi = uk_cpi %>%
  arrange(desc(date)) %>%
  slice(1:3) %>%
  mutate(month = month(date, label = T, abbr = T)) %>%
  mutate(month = gsub("\\\\.","", month))

max_core_cpi = uk_core_cpi %>%
  arrange(desc(date)) %>%
  slice(1:3) %>%
  mutate(month = month(date, label = T, abbr = T)) %>%
  mutate(month = gsub("\\\\.","", month)) 

max_unem = uk_unem %>%
  arrange(desc(date)) %>%
  slice(1:3) %>%
  mutate(month = month(date, label = T, abbr = T)) %>%
  mutate(month = gsub("\\\\.","", month))


subtitle_unem = sprintf("Chômage %s : %s%%, %s : %s%%, %s : %s%% ",
                        max_unem[1,"month"], max_unem[1,"value"],
                        max_unem[2,"month"], max_unem[2,"value"],
                        max_unem[3,"month"], max_unem[3,"value"])
subtitle_cpi = sprintf("Inflation %s : %s%%, %s : %s%%, %s : %s%% ",
                       max_cpi[1,"month"], max_cpi[1,"value"],
                       max_cpi[2,"month"], max_cpi[2,"value"],
                       max_cpi[3,"month"], max_cpi[3,"value"])
subtitle_core_cpi = sprintf("Inflation sous-jacente %s : %s%%, %s : %s%%, %s : %s%% ",
                       max_core_cpi[1,"month"], max_core_cpi[1,"value"],
                       max_core_cpi[2,"month"], max_core_cpi[2,"value"],
                       max_core_cpi[3,"month"], max_core_cpi[3,"value"])

data_time = data$date

subtitle_month = gsub("\\\\.","",
                      lubridate::month(max(data_time), label = TRUE))

subtitle_year = lubridate::year(max(data_time)) 

xaxis_breaks = seq.Date(from = min(data_time), to = max(data_time), by = "6 months")

graph_subtitle = sprintf("Dernier point : %s %s, source : ONS \n %s \n %s \n %s",
                         subtitle_month, subtitle_year,
                         subtitle_unem, subtitle_cpi, subtitle_core_cpi)

graph_uk_labor_market = ggplot(data = data,
                               aes(x = date, y = value, colour = label)) +
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

graph_uk_labor_market %>%  ggsave(filename = file_graph, width = 12, height = 7)



