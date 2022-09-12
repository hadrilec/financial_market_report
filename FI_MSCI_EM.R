
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

link_cahierFI_graph = "M:/Usuels.dsc/pRev/FI/cahier_FI/graph"
file_name = paste0("graph_FI_msci_em", ".pdf" )
file_graph = file.path(link_cahierFI_graph, file_name)

start_time = as.Date("2007-01-01")

source("M:/Usuels.dsc/pRev/FI/dataInsight_query_excel_list.R")

list_var_DI = c("M661808UX.D", "D111REXEURD.D")

list_var_pRev = c("msci_usd",	"msci_eur")

list_label = c("Dollar", "Euro")

getDataDI("M661808UX.D", "D111REXEURD.D")

D111REXEURD.D$D111REXEURD.D = M661808UX.D$M661808UX.D / D111REXEURD.D$D111REXEURD.D 

list_df = list()

for(ivar in 1:length(list_var_pRev)){
  df = get(list_var_DI[ivar])
  df = df[which(df[,"time"] >= start_time),]
  names(df)[2] = "value"
  df[,"label"] = list_label[ivar]
  list_df[[ivar]] = df
}

data = bind_rows(list_df)

data_time = data$time

subtitle_month = gsub("\\\\.","",
                      lubridate::month(max(data_time), label = TRUE))
subtitle_year = lubridate::year(max(data_time)) 
xaxis_breaks = seq.Date(from = min(data_time), to = max(data_time), by = "6 months")
graph_subtitle = sprintf("Dernier point : %s %s, source : DataInsight",
                         subtitle_month, subtitle_year)


yaxis_breaks = seq(from = floor(min(data$value, na.rm = TRUE) / 200) * 200,
                   to = ceiling(max(data$value, na.rm = TRUE)/ 200) * 200, by = 200)
# scale_y_continuous(sec.axis = dup_axis(), breaks = yaxis_breaks, labels = function(x) paste0(x, "%")) +



graph_fi_msci = ggplot(data = data,
               aes(x = data_time, y = value, colour = label )) +
  geom_line(size = 1) +
  ggtitle("Indices MSCI EM en dollar et en euro") +
  labs(subtitle = graph_subtitle) + 
  scale_y_continuous(sec.axis = dup_axis(), breaks = yaxis_breaks) +
  scale_x_date(breaks = xaxis_breaks, date_labels = "%b %y" ) +
  theme_stata() +
  theme(
    plot.title   = element_text(lineheight = 0.8, face = "bold", hjust = 0.5, size = 18),
    axis.text.x  = element_text(angle = 45, hjust = 1),
    axis.text.y  = element_text(angle = 0, hjust = 1),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.title = element_blank(),
    text = element_text(size = 15),
    legend.position = "bottom"
  ) 

graph_fi_msci + ggsave(filename = file_graph, width = 12, height = 7)