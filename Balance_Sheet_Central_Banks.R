
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

file_name = paste0("graph_balance_sheet", ".pdf" )
file_graph = file.path(link_cahierFI_graph, file_name)
start_time = as.Date("2007-01-01")

fredr_set_key("1e1376b050a44076281adda2fe2e1a32")

bs_boj = fredr(series_id = "JPNASSETS", observation_start = start_time) %>%
  mutate(label = "Bank of Japan")
bs_boj = as.data.frame(bs_boj)
bs_boj[,"value"] = 100 * bs_boj[,"value"] / bs_boj[1,"value"]

bs_fed = fredr(series_id = "WALCL", observation_start = start_time) %>%
  mutate(label = "Federal Reserve Bank")
bs_fed = as.data.frame(bs_fed)
bs_fed[,"value"] = 100 * bs_fed[,"value"] / bs_fed[1,"value"]

bs_ecb = fredr(series_id = "ECBASSETS", observation_start = start_time) %>%
  mutate(label = "European Central Bank")
bs_ecb = as.data.frame(bs_ecb)
bs_ecb[,"value"] = 100 * bs_ecb[,"value"] / bs_ecb[1,"value"]

data = rbind(bs_fed, bs_boj, bs_ecb)

data_time = data$date

subtitle_month = gsub("\\\\.","",
                      lubridate::month(max(data_time), label = TRUE))
subtitle_year = lubridate::year(max(data_time)) 
xaxis_breaks = seq.Date(from = min(data_time), to = max(data_time), by = "6 months")
graph_subtitle = sprintf("Dernier point : %s %s, source : FED, BCE, BOJ",
                         subtitle_month, subtitle_year)

# yaxis_breaks = seq(from = floor(min(data$obsValue, na.rm = TRUE) / 0.5) * 0.5,
#                    to = ceiling(max(data$obsValue, na.rm = TRUE)/ 0.5) * 0.5, by = 0.5)
# scale_y_continuous(sec.axis = dup_axis(), breaks = yaxis_breaks, labels = function(x) paste0(x, "%")) +


graph_bs_cb = ggplot(data = data,
               aes(x = data_time, y = value, colour = label )) +
  geom_line(size = 1) +
  ggtitle("Bilan des banques centrales (base 100 en janvier 2007)") +
  labs(subtitle = graph_subtitle) + 
  scale_y_continuous(sec.axis = dup_axis()) + 
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

graph_bs_cb %>%  ggsave(filename = file_graph, width = 12, height = 7)
