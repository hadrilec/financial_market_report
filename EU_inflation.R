
library(rdbnomics)
library(rsdmx)
library(ggthemes)
library(grid)
library(lubridate)
library(eurostat)
library(fredr)
library(dplyr)
library(ggplot2)

date = gsub("-","",Sys.Date())
link_cahierFI_graph = "M:/Usuels.dsc/pRev/FI/cahier_FI/graph"

file_name = paste0("graph_ZE_HICP", ".pdf" )
file_graph_ZE_HICP = file.path(link_cahierFI_graph, file_name)

  
HICP = eurostat::get_eurostat("prc_hicp_manr") %>% 
  filter(geo == "EA",  # Euro Area
         coicop %in% c("CP00",  #all-items HICP
                       "TOT_X_NRG_FOOD_NP"),  # Overall index excluding energy and unprocessed food
         time >= as.Date("2002-01-01", "%Y-%m-%d")
  )

HICP = HICP %>% mutate(name = case_when(coicop == "CP00" ~ "Inflation",
                                        coicop == "TOT_X_NRG_FOOD_NP" ~ "Inflation sous-jacente"))

max_cpi = HICP %>%
  filter(coicop == "CP00") %>%
  arrange(desc(time)) %>%
  slice(1:3) %>%
  mutate(month = month(time, label = T, abbr = T)) %>%
  mutate(month = gsub("\\\\.","", month))

max_core_cpi = HICP %>%
  filter(coicop == "TOT_X_NRG_FOOD_NP") %>%
  arrange(desc(time)) %>%
  slice(1:3) %>%
  mutate(month = month(time, label = T, abbr = T)) %>%
  mutate(month = gsub("\\\\.","", month))

subtitle_day = lubridate::day(max(HICP$time))
subtitle_month = gsub("\\\\.","", lubridate::month(max(HICP$time), label = TRUE))
subtitle_year = lubridate::year(max(HICP$time)) 
xaxis_breaks = seq.Date(from = min(HICP$time), to = max(HICP$time), by = "6 months")

graph_subtitle_cpi =  sprintf("Inflation %s : %s%%, %s : %s%%, %s : %s%% ",
                           max_cpi[1,"month"], max_cpi[1,"values"],
                           max_cpi[2,"month"], max_cpi[2,"values"],
                           max_cpi[3,"month"], max_cpi[3,"values"])

graph_subtitle_core_cpi =  sprintf("Inflation sous-jacente %s : %s%%, %s : %s%%, %s : %s%% ",
                              max_core_cpi[1,"month"], max_core_cpi[1,"values"],
                              max_core_cpi[2,"month"], max_core_cpi[2,"values"],
                              max_core_cpi[3,"month"], max_core_cpi[3,"values"])

graph_subtitle = sprintf("Dernier point : %s %s %s, source : Eurostat \n %s \n %s",
                         subtitle_day, subtitle_month, subtitle_year,
                         graph_subtitle_cpi, graph_subtitle_core_cpi)


yaxis_breaks = seq(from = floor(min(HICP$values, na.rm = TRUE)/ 0.5) * 0.5,
                   to = ceiling(max(HICP$values, na.rm = TRUE)/ 0.5) * 0.5, by = 0.5)
# scale_y_continuous(sec.axis = dup_axis(), breaks = yaxis_breaks, labels = function(x) paste0(x, "%")) +


graph_ZE_HICP = ggplot(data = HICP, aes(x = time, y = values, colour = name, linetype = name)) +
  geom_line(size = 1) +
  ggtitle("Inflation en zone euro") +
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

graph_ZE_HICP + ggsave(file_graph_ZE_HICP, width = 12, height = 7)

export_graph(graph_ZE_HICP, folder_name = "inflation_zone_euro", perim = "FI", update = TRUE)


