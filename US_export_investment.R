
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

file_name = paste0("graph_US_export_investment", ".pdf" )
file_graph = file.path(link_cahierFI_graph, file_name)

fredr_set_key("1e1376b050a44076281adda2fe2e1a32")

us_priv_invst = fredr(series_id = "GPDI", observation_start = as.Date("2014-01-01"))

us_export = fredr(series_id = "NETEXP", observation_start = as.Date("2014-01-01"))

df_us = rbind(us_export, us_priv_invst)

df_us = df_us %>%
  mutate(name = case_when(series_id == "NETEXP" ~ "Balance commerciale",
                          series_id == "GPDI" ~ "Investissement privé"))


last_export = us_export %>%
  arrange(desc(date)) %>%
  slice(1:3) %>%
  mutate(month = month(date, label = T, abbr = T)) %>%
  mutate(month = gsub("\\\\.","", month)) %>%
  mutate(value = round(value)) %>%
  pull(value)

df = df_us

subtitle_day = lubridate::day(max(df$date))
subtitle_month = gsub("\\\\.","", 
                      lubridate::month(max(df$date), label = TRUE))
subtitle_year = lubridate::year(max(df$date)) 
xaxis_breaks = seq.Date(from = min(df$date),
                        to = max(df$date), by = "3 months")

graph_subtitle_ = sprintf("Balance commerciale: %s \n ",last_export)

graph_subtitle = sprintf("Dernier point : %s %s %s, source : FED ",
                         subtitle_day, subtitle_month, subtitle_year)

# df$value = df$value / 1000000000
coeff = 1000
yaxis_breaks = seq(from = floor(min(df$value, na.rm = TRUE)/ coeff) * coeff,
                   to = ceiling(max(df$value, na.rm = TRUE)/ coeff) * coeff, by = coeff)

graph_us_export_invest = ggplot(data = df,
               aes(x = date, y = value, colour = name
               )) + 
  facet_wrap(~name, scales = "free") +
  geom_line(size = 1) +
  ggtitle("Investissement privé et balance commerciale américaine") +
  labs(subtitle = graph_subtitle, y = "milliards de dollars") + 
  scale_y_continuous(sec.axis = dup_axis()) +
  scale_x_date(breaks = xaxis_breaks, date_labels = "%b %y" ) +
  theme_stata() +
  theme(
    plot.caption = element_text(hjust = 0.5),
    plot.title   = element_text(lineheight = 0.8, face = "bold", hjust = 0.5, size = 18),
    axis.text.x  = element_text(angle = 45, hjust = 1),
    axis.text.y  = element_text(angle = 0, hjust = 1),
    axis.title.x = element_blank(),
    # axis.title.y = element_blank(),
    legend.title = element_blank(),
    legend.position = "bottom"
  ) 

graph_us_export_invest %>%  ggsave(filename = file_graph, width = 12, height = 7)
