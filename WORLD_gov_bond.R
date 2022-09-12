
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


link_cahierFI_graph = "M:/Usuels.dsc/pRev/FI/cahier_FI/graph"

file_name = paste0("graph_WORLD_gov_bond", ".pdf" )
file_graph = file.path(link_cahierFI_graph, file_name)

fredr_set_key("1e1376b050a44076281adda2fe2e1a32")
start_time = as.Date("2011-01-01")


source("M:/Usuels.dsc/pRev/FI/dataInsight_query_excel_list.R")

list_var_DI = c("FRARIBBGFT2.D", "DEURIBBGFT.D", "ESPRIBBGFT.D",
                "ITARIBBGFT.D", "USARIBBGFT1.D", "GBRRIBBGFT2.D", "JPNRIBBGFT2.D")

list_var_pRev = c("fr_gov10y","de_gov10y", "es_gov10y",
                  "it_gov10y", "us_gov10y", "uk_gov10y", "jp_gov10y")

list_label = c("France", "Allemagne", "Espagne",
               "Italie", "Etats-Unis", "Royaume-Uni", "Japon")

getDataDI("FRARIBBGFT2.D", "DEURIBBGFT.D", "ESPRIBBGFT.D",
          "ITARIBBGFT.D", "USARIBBGFT1.D", "GBRRIBBGFT2.D", "JPNRIBBGFT2.D")

list_df = list()

for(ivar in 1:length(list_var_pRev)){
  df = get(list_var_DI[ivar])
  names(df)[2] = "value"
  df[,"label"] = list_label[ivar]
  list_df[[ivar]] = df
}

data_10y = bind_rows(list_df)

data = data_10y %>%
  filter(label %in%  c("France", "Allemagne", "Etats-Unis", "Royaume-Uni", "Japon")) %>%
  dplyr::rename(date = time) %>%
  filter(date >= start_time)

# al_bond_10Y= fredr(series_id = "IRLTLT01DEM156N",
#                    observation_start = start_time) %>%
#   mutate(label = "Allemagne")
# 
# fr_bond_10Y= fredr(series_id = "IRLTLT01FRM156N",
#                    observation_start = start_time) %>%
#   mutate(label = "France")
# 
# jp_bond_10Y= fredr(series_id = "IRLTLT01JPM156N",
#                    observation_start = start_time) %>%
#   mutate(label = "Japon")
# 
# uk_bond_10Y= fredr(series_id = "IRLTLT01GBM156N",
#                    observation_start = start_time)  %>%
#   mutate(label = "Royaume-Uni")
# 
# us_bond_10Y = fredr(series_id = "DGS10",
#                     observation_start = start_time) %>%
#   mutate(label = "Etats-Unis")


# data = rbind(us_bond_10Y, al_bond_10Y, uk_bond_10Y, fr_bond_10Y, jp_bond_10Y) 

last_values = data %>% 
  group_by(label) %>% 
  filter(date == max(date)) %>% 
  mutate(value = round(value,1)) %>% 
  mutate(date_ = paste(gsub("\\\\.","",month(date, label = TRUE)), year(date))) %>% 
  as.data.frame()

data_time = last_values$date

subtitle_day =   lubridate::day(min(data_time, na.rm = T))

subtitle_month = gsub("\\\\.","",
                      lubridate::month(min(data_time, na.rm = T), label = TRUE))

subtitle_year = lubridate::year(min(data_time, na.rm = T)) 

xaxis_breaks = seq.Date(from = min(data$date, na.rm = T), to = max(data$date, na.rm = T), by = "6 months")


subtitle_values = ""

for(i in 1:nrow(last_values)){
  subtitle_values = paste(subtitle_values, sprintf("%s : %s%%,",last_values[i,"label"], last_values[i,"value"]))
}

graph_subtitle = sprintf("Dernier point : %s %s %s, source : DataInsight\n %s",
                         subtitle_day, subtitle_month, subtitle_year,subtitle_values)

yaxis_breaks = seq(from = floor(min(data$value, na.rm = TRUE)/ 0.5) * 0.5,
                   to = ceiling(max(data$value, na.rm = TRUE)/ 0.5) * 0.5, by = 0.5)


data = data %>% 
  tidyr::drop_na()

graph_WORLD_gov_bond = ggplot(data = data,
                            aes(x = date, y = value, colour = label)) +
  # facet_wrap(~type, scales = "free") +
  geom_line(size = 1) +
  ggtitle("Taux souverains des pays avancés à 10 ans") +
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

graph_WORLD_gov_bond + ggsave(filename = file_graph, width = 12, height = 7)

# export_graph(graph_WORLD_gov_bond, perim = "FI", folder_name = "WORLD_gov_bond_hist", update = TRUE)

# 
#  export to csv
# 
link_cahierFI_excel = "M:/Usuels.dsc/pRev/FI/cahier_FI/excel"
file_excel = file.path(link_cahierFI_excel, paste0(gsub(".pdf|graph_","",file_name),".csv"))

df = as.data.frame(data)
var_name = "label"
value_name = "value"
date_name = "date"

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
sheetName = "2_WORLD_gov_bond"

source(file.path(link_prod_code, "write_update.xlsx.R"))

write_update.xlsx(data_excel, sheetName, file_path)



