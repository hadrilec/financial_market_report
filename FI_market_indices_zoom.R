
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
file_name = paste0("graph_FI_market_indices_zoom", ".pdf" )
file_name_growth = paste0("graph_FI_market_indices_zoom_growth", ".pdf" )
file_graph = file.path(link_cahierFI_graph, file_name)
file_graph_growth = file.path(link_cahierFI_graph, file_name_growth)
comparison_year = 2020
comparison_month = 2
month_comparison = as.character(month(comparison_month, label = T, abbr= F))
start_time = as.Date("2017-01-01")

source("M:/Usuels.dsc/pRev/FI/dataInsight_query_excel_list.R")
# if(!exists("export_graph")){
#   source("M:/Usuels.dsc/pRev/FI/export_graph.R")
# }

list_var_DI = c("OFRACAC.D", "DSP5M915.D",
                "DEUN1PS.D", "NI225X.D", "D112SHIFTSE100.D")

list_var_pRev = c("cac","sp_500",
                  "dax","niky",	"ftse")

list_label = c("CAC 40","S&P 500",
               "DAX", "NIKKEI", "FOOTSIE")

getDataDI("OFRACAC.D", "DSP5M915.D",
          "DEUN1PS.D", "NI225X.D", "D112SHIFTSE100.D")

# df = get_DI("JPNN1PS.D")

list_df = list()

for(ivar in 1:length(list_var_pRev)){
  df = get(list_var_DI[ivar])
  df = df[which(df[,"time"] >= start_time),]
  names(df)[2] = "value"
  df = df[!is.na(df[,"value"]),]
  rownames(df) = NULL
  df[,"year"] = year(df[,"time"])
  df[,"month"] = month(df[,"time"])
  # df[,"day"] = day(df[,"time"])
  row = max(which(df[,"year"] == comparison_year & df[,"month"] == comparison_month))
  df[,"value_nominal"] = df[,"value"]
  df[,"value"] = 100 * df[,"value"] / df[row,"value"]
  df[,"label"] = list_label[ivar]
  list_df[[ivar]] = df
}

data = bind_rows(list_df)



data = data %>%
  filter(time <= today()) %>% 
  group_by(label) %>% 
  arrange(time) %>% 
  mutate(growth = 100 * (value/dplyr::lag(value)-1))

# data = data %>%
#   filter(time <= "2019-12-31")

last_values = data %>%
  group_by(label) %>%
  filter(time == max(time)) %>%
  mutate(value = round(value))

caption = ""

for(i in 1:nrow(last_values)){
  caption = paste0(caption, sprintf("%s: %s,  ",last_values[i,"label"], last_values[i,"value"] ))
}

data_time = data$time

subtitle_day = lubridate::day(max(data_time))

subtitle_month = gsub("\\\\.","",
                      lubridate::month(max(data_time), label = TRUE))
subtitle_year = lubridate::year(max(data_time)) 
xaxis_breaks = seq.Date(from = min(data_time), to = max(data_time), by = "6 months")
graph_subtitle = sprintf("Dernier point : %s %s %s, source : DataInsight \n %s",
                         subtitle_day, subtitle_month, subtitle_year, caption)
coeff = 5
yaxis_breaks = seq(from = floor(min(data$value, na.rm = TRUE)/ coeff) * coeff,
                   to = ceiling(max(data$value, na.rm = TRUE)/ coeff) * coeff, by = coeff)
# scale_y_continuous(sec.axis = dup_axis(), breaks = yaxis_breaks, labels = function(x) paste0(x, "%")) +
  
graph_fi_market_indices = ggplot(data = data,
               aes(x = time, y = value, colour = label )) +
  geom_line(size = 1) +
  ggtitle(sprintf("Indices boursiers des pays avancés (base 100 %s %s)",
                  month_comparison, comparison_year)) +
  labs(subtitle = graph_subtitle) + 
  scale_y_continuous(sec.axis = dup_axis(), breaks = yaxis_breaks) +
  scale_x_date(breaks = xaxis_breaks, date_labels = "%b %y" ) +
  theme_stata() +
  theme(
    plot.caption = element_text(hjust = 0.5),
    plot.title   = element_text(lineheight = 0.8, face = "bold", hjust = 0.5, size = 18),
    axis.text.x  = element_text(angle = 45, hjust = 1),
    axis.text.y  = element_text(angle = 0, hjust = 1),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.title = element_blank(),
    text = element_text(size = 15),
    legend.position = "bottom"
  ) 

graph_fi_market_indices + ggsave(filename = file_graph, width = 12, height = 7)

export_minio_graph(graph_fi_market_indices, folder_name = "fi_market_indices", perim = "FI")

data_growth = data %>% 
  filter(time >= "2019-01-01") %>% 
  drop_na()

graph_subtitle2 = sprintf("Dernier point : %s %s %s, source : DataInsight",
                         subtitle_day, subtitle_month, subtitle_year)

data_growth_ = data_growth %>% 
  filter(time >= "2019-07-01") %>% 
  drop_na()

graph_fi_market_indices_growth =
ggplot(data = data_growth_,
       aes(x = time, y = growth, colour = label )) +
  geom_line(size = 0.8) +
  # ggtitle("Indices boursiers des pays avancés (taux de croissance en %)") +
  ggtitle(sprintf("Indices boursiers des pays avancés (base 100 %s %s)",
                  month_comparison, comparison_year)) +
  labs(subtitle = graph_subtitle2) +
  facet_wrap(~label, scales = "free") + 
  # scale_y_continuous(sec.axis = dup_axis(), breaks = yaxis_breaks) +
  scale_x_date(breaks = xaxis_breaks, date_labels = "%b %y" ) +
  theme_stata() +
  theme(
    plot.caption = element_text(hjust = 0.5),
    plot.title   = element_text(lineheight = 0.8, face = "bold", hjust = 0.5, size = 18),
    axis.text.x  = element_text(angle = 45, hjust = 1),
    axis.text.y  = element_text(angle = 0, hjust = 1),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.title = element_blank(),
    text = element_text(size = 15),
    legend.position = "bottom"
  ) + ggsave(filename = file_graph_growth, width = 12, height = 7)

graph_fi_market_indices_growth

export_graph(graph_fi_market_indices_growth, folder_name = "fi_market_indices_growth", perim = "FI")

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



