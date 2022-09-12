
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
file_name = paste0("graph_FI_em_market_indices_zoom_two_plots", ".pdf" )
file_graph = file.path(link_cahierFI_graph, file_name)
comparison_year = 2019
comparison_month = 1
month_comparison = as.character(month(comparison_month, label = T, abbr= F))
start_time = as.Date("2019-01-01")

source("M:/Usuels.dsc/pRev/FI/dataInsight_query_excel_list.R")


list_var_DI = c("RUSN1PS.D", "ARGENBCX.D",
                 "CHNN1PS.D", "BRAN1PS.D", "INBSEX.D", "M186SPASTT01XO.M")

list_var_pRev = c("rus","arg",
                  "chi", "bre","ind",	"tur")

list_label = c("Russie","Argentine",
               "Chine", "Brésil", "Inde", "Turquie")

getDataDI("RUSN1PS.D", "CHNN1PS.D", "BRAN1PS.D", "INBSEX.D")
getDataDI("M186SPASTT01XO.M")
getDataDI("ARGENBCX.D")

list_df = list()

for(ivar in 1:length(list_var_pRev)){
  df = get(list_var_DI[ivar])
  df = df[which(df[,"time"] >= start_time),]
  names(df)[2] = "value"
  df = df[!is.na(df[,"value"]),]
  df[,"year"] = year(df[,"time"])
  df[,"month"] = month(df[,"time"])
  row = max(which(df[,"year"] == comparison_year & df[,"month"] == comparison_month))
  df[,"value"] = df[,"value"] * 100 / df[row,"value"]
  # if(list_var_DI[ivar] == "ARGENBCX.D"){
  #   df[,"value"] = df[,"value"] /(400/250)
  # }
  df[,"label"] = list_label[ivar]
  list_df[[ivar]] = df
}

data = bind_rows(list_df)

data = data %>%
  filter(time <= today())

last_values = data %>%
  group_by(label) %>%
  filter(time == max(time)) %>%
  mutate(value = round(value))

caption = ""

for(i in 1:nrow(last_values)){
  caption = paste0(caption, sprintf("%s: %s,  ",last_values[i,"label"], last_values[i,"value"] ))
}

data_time = data$time

data = data %>%
  mutate(type = case_when(label == "Argentine" ~ "Argentine", TRUE ~ "Autres pays émergents"))

subtitle_day = lubridate::day(max(data_time))
subtitle_month = gsub("\\\\.","",
                      lubridate::month(max(data_time), label = TRUE))
subtitle_year = lubridate::year(max(data_time)) 
xaxis_breaks = seq.Date(from = min(data_time), to = max(data_time), by = "1 months")
graph_subtitle = sprintf("Dernier point : %s %s %s, source : DataInsight \n %s",
                         subtitle_day, subtitle_month, subtitle_year, caption)

yaxis_breaks = seq(from = floor(min(data$value, na.rm = TRUE) / 50) * 50,
                   to = ceiling(max(data$value, na.rm = TRUE)/ 50) * 50, by = 50)
# scale_y_continuous(sec.axis = dup_axis(), breaks = yaxis_breaks, labels = function(x) paste0(x, "%")) +


graph_fi_em_market_indices = ggplot(data = data, aes(x = data_time , y = value, colour = label )) +
  geom_line(size = 1) +
  facet_wrap(~type, scales = "free") +
  ggtitle(sprintf("Indices boursiers des pays émergents (base 100 %s %s)", month_comparison,comparison_year)) +
  labs(subtitle = graph_subtitle) + 
  # scale_y_continuous(breaks = yaxis_breaks ) +
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

graph_fi_em_market_indices + ggsave(filename = file_graph, width = 12, height = 7)