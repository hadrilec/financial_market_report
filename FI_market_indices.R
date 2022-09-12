
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
file_name = paste0("graph_FI_market_indices", ".pdf" )
file_graph = file.path(link_cahierFI_graph, file_name)

start_time = as.Date("2007-01-01")

source("M:/Usuels.dsc/pRev/FI/dataInsight_query_excel_list.R")

list_var_DI = c("OFRACAC.D", "DSP5M915.D",
                "DEUN1PS.D", "NI225X.D", "D112SHIFTSE100.D")

list_var_pRev = c("cac","sp_500",
                  "dax","niky",	"ftse")

list_label = c("CAC 40","S&P 500",
               "DAX", "NIKKEI", "FOOTSIE")

getDataDI("OFRACAC.D", "DSP5M915.D",
          "DEUN1PS.D", "NI225X.D", "D112SHIFTSE100.D")

list_df = list()

for(ivar in 1:length(list_var_pRev)){
  df = get(list_var_DI[ivar])
  df = df[which(df[,"time"] >= start_time),]
  names(df)[2] = "value"
  df = df[!is.na(df[,"value"]),]
  df[,"year"] = year(df[,"time"])
  df[,"month"] = month(df[,"time"])
  df[,"value"] = 100 * df[,"value"] / df[max(which(df[,"year"] == 2015 & df[,"month"] == 1)),"value"]
  df[,"label"] = list_label[ivar]
  list_df[[ivar]] = df
}

data = bind_rows(list_df)

data = data %>%
  filter(time <= today())

data_time = data$time

subtitle_day = lubridate::day(max(data_time))
subtitle_month = gsub("\\\\.","",lubridate::month(max(data_time), label = TRUE))
subtitle_year = lubridate::year(max(data_time)) 
xaxis_breaks = seq.Date(from = min(data_time), to = max(data_time), by = "6 months")
graph_subtitle = sprintf("Dernier point : %s %s %s, source : DataInsight",
                         subtitle_day, subtitle_month, subtitle_year)

yaxis_breaks = seq(from = floor(min(data$value, na.rm = TRUE) / 10) * 10,
                   to = ceiling(max(data$value, na.rm = TRUE)/ 10) * 10, by = 10)
# scale_y_continuous(sec.axis = dup_axis(), breaks = yaxis_breaks, labels = function(x) paste0(x, "%")) +
  
graph_fi_market_indices = ggplot(data = data,
               aes(x = time, y = value, colour = label )) +
  geom_line(size = 1) +
  ggtitle("Indices boursiers des pays avancés (base 100 janvier 2015)") +
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
    legend.position = "bottom"
  ) 

graph_fi_market_indices + ggsave(filename = file_graph, width = 12, height = 7)

export_graph(graph_fi_market_indices, perim = "FI",
             folder_name = "fi_market_indices_hist", update = TRUE)

# 
#  export to csv
# 

link_cahierFI_excel = "M:/Usuels.dsc/pRev/FI/cahier_FI/excel"
file_excel = file.path(link_cahierFI_excel, paste0(gsub(".pdf|graph_","",file_name),".csv"))

df = as.data.frame(data)
var_name = "label"
value_name = "value"
date_name = "time"

list_var = unique(df[,var_name])
df_ = data.frame(unique(df[,date_name]))
names(df_)[1] = date_name

for(var in list_var){
  df2 = df[which(df[,var_name] == var), c(date_name, value_name)]
  df2 = as.data.frame(df2)
  names(df2)[2] = var
  df_ = merge(df_, df2, by = date_name, all.x = T, all.y = T)
}

write.csv(df_, file = file_excel, row.names = FALSE) 



data_excel = data.frame(df_)

link_prod = "M:/Usuels.dsc/pRev/FI/prod"
link_prod_code =  file.path(link_prod, "code")
link_prod_excel =  file.path(link_prod, "excel")
file_path = file.path(link_prod_excel, "data_fiche_marche_fi.xlsx")
sheetName = "5_FI_market_indices"

source(file.path(link_prod_code, "write_update.xlsx.R"))

write_update.xlsx(data_excel, sheetName, file_path)
