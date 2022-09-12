
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
file_name = paste0("graph_FI_market_volatility", ".pdf" )
file_graph = file.path(link_cahierFI_graph, file_name)

start_time = as.Date("2015-01-01")

source("M:/Usuels.dsc/pRev/FI/dataInsight_query_excel_list.R")

list_var_DI = c("CBOEVIXC.D7", "VDAXNX.D", "VFTSEVOLATO.D")

list_var_pRev = c("vsp_500",	"vdax",	"vftse")

list_label = c("Volatilité S&P 500", "Volatilité DAX", "Volatilité Footsie")

getDataDI("VDAXNX.D", "VFTSEVOLATO.D")
getDataDI("CBOEVIXC.D7")

list_df = list()

for(ivar in 1:length(list_var_pRev)){
  df = get(list_var_DI[ivar])
  names(df)[2] = "value"
  df[,"label"] = list_label[ivar]
  list_df[[ivar]] = df
}

data = bind_rows(list_df)

data = data %>%
  filter(time >= start_time)

data_time = data$time

subtitle_month = gsub("\\\\.","",
                      lubridate::month(max(data_time), label = TRUE))
subtitle_year = lubridate::year(max(data_time)) 
xaxis_breaks = seq.Date(from = min(data_time), to = max(data_time), by = "6 months")
graph_subtitle = sprintf("Dernier point : %s %s, source : DataInsight",
                         subtitle_month, subtitle_year)

yaxis_breaks = seq(from = floor(min(data$value, na.rm = TRUE) / 5) * 5,
                   to = ceiling(max(data$value, na.rm = TRUE)/ 5) * 5, by = 5)
# scale_y_continuous(sec.axis = dup_axis(), breaks = yaxis_breaks, labels = function(x) paste0(x, "%")) +

graph_fi_market_vol = ggplot(data = data,
               aes(x = time, y = value, colour = label )) +
  geom_line(size = 1) +
  ggtitle("Volatilité des indices boursiers des pays avancés") +
  labs(subtitle = graph_subtitle) + 
  scale_x_date(breaks = xaxis_breaks, date_labels = "%b %y" ) +
  scale_y_continuous(sec.axis = dup_axis(), breaks = yaxis_breaks) +
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

graph_fi_market_vol + ggsave(filename = file_graph, width = 12, height = 7)

export_minio_graph(graph_fi_market_vol, perim = "FI", folder_name = "fi_market_vol")
