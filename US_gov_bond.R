
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

date = gsub("-","",Sys.Date())
link_cahierFI_graph = "M:/Usuels.dsc/pRev/FI/cahier_FI/graph"

file_name = paste0("graph_US_gov_bond", ".pdf" )
file_graph = file.path(link_cahierFI_graph, file_name)

fredr_set_key("1e1376b050a44076281adda2fe2e1a32")

us_bond_10Y_infl = fredr(series_id = "DFII10",
                         observation_start = as.Date("2014-01-01"))

us_bond_10Y = fredr(series_id = "DGS10",
                    observation_start = as.Date("2014-01-01"))
#  all( us_bond_10Y$date == us_bond_10Y_infl$date)

us_breakeven = data.frame(date = us_bond_10Y$date,
                          series_id = "breakeven",
                          value = us_bond_10Y$value - us_bond_10Y_infl$value)
# 10-Year Breakeven Inflation Rate (T10YIE)

df_us_bond = rbind(us_bond_10Y, us_bond_10Y_infl, us_breakeven)

df_us_bond = df_us_bond %>%
  mutate(name = case_when(series_id == "DFII10" ~ "Obligations indexées sur l'inflation",
                          series_id == "DGS10" ~ "Obligations",
                          series_id == "breakeven" ~ "Point-mort"))


max_us10y = us_bond_10Y %>%
  arrange(desc(date)) %>%
  slice(1:3) %>%
  mutate(month = month(date, label = T, abbr = T)) %>%
  mutate(month = gsub("\\\\.","", month)) %>%
  mutate(value = round(value,1))

max_usbreakeven = us_breakeven %>%
  arrange(desc(date)) %>%
  slice(1:3) %>%
  mutate(month = month(date, label = T, abbr = T)) %>%
  mutate(month = gsub("\\\\.","", month)) %>%
  mutate(value = round(value,1))

subtitle_day = lubridate::day(max(df_us_bond$date))
subtitle_month = gsub("\\\\.","",
                      lubridate::month(max(df_us_bond$date), label = TRUE))
subtitle_year = lubridate::year(max(df_us_bond$date)) 
xaxis_breaks = seq.Date(from = min(df_us_bond$date), to = max(df_us_bond$date), by = "6 months")

graph_subtitle_bond = sprintf("Bond 10 ans: %s%%, Point-mort: %s%%",
                               max_us10y[1,"value"],
                               max_usbreakeven[1,"value"])

graph_subtitle = sprintf("Dernier point : %s %s %s, source : FED \n %s",
                         subtitle_day, subtitle_month, subtitle_year,
                         graph_subtitle_bond)

yaxis_breaks = seq(from = floor(min(df_us_bond$value, na.rm = TRUE) / 0.5) * 0.5,
                   to = ceiling(max(df_us_bond$value, na.rm = TRUE)/ 0.5) * 0.5, by = 0.5)
# scale_y_continuous(sec.axis = dup_axis(), breaks = yaxis_breaks, labels = function(x) paste0(x, "%")) +

df_us_bond = df_us_bond %>% drop_na()

graph_us_gov_bond = ggplot(data = df_us_bond,
               aes(x = date, y = value, colour = name
                   # , linetype = name
               )) +
  geom_line(size = 1) +
  ggtitle("Taux souverains américains à 10 ans et point mort d'inflation") +
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

graph_us_gov_bond + ggsave(filename = file_graph, width = 12, height = 7)

export_graph(graph_us_gov_bond, perim = "FI", folder_name = "us_gov_bond")
