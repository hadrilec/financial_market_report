
library(rdbnomics)
library(rsdmx)
library(ggthemes)
library(grid)
library(lubridate)
library(eurostat)
library(fredr)
library(dplyr)
library(ggplot2)

# https://www.aft.gouv.fr/en/oatis-key-figures#rendement

link_cahierFI_graph = "M:/Usuels.dsc/pRev/FI/cahier_FI/graph"
file_name = paste0("graph_OAT_breakeven", ".pdf" )
file_graph = file.path(link_cahierFI_graph, file_name)

file_data = "M:/Usuels.dsc/pRev/FI/cahier_FI/data/2021-05-01_rend_tit-ref-oati_exl.xls"

data = readxl::read_xls(file_data)

label = data[4,] %>% select(-1) %>% t() %>% as.data.frame()
data = data[-c(1:5),]
label[,1] = as.character(label[,1])
label[3,1] = paste(label[3,1], "-", label[2,1])
label[6,1] = paste(label[6,1], "-", label[5,1])
labels = label[,1]
names(data) = c("time", as.character(labels))
data = as.data.frame(data)
data[,1] = as.numeric(as.character(data[,1]))
data[,1] = as.Date(data[,1], origin = "1899-12-30")

list_df = list()

for(i in 2:ncol(data)){
  df = data[,c(1,i)]
  df[,2] = as.numeric(as.character(data[,i])) * 100

  df[,"label"] = as.character(names(data)[i])
  names(df)[2] = "value"

  list_df[[i-1]] = df
}

data_ = bind_rows(list_df) %>% drop_na()
data_[,2] = as.numeric(as.character(data_[,2]))

last_data = data_ %>%
  filter(label == "Point-mort d'inflation") %>%
  filter(time == max(time)) %>%
  pull(value) %>%
  round(1)

last_data_OAT = data_ %>%
  filter(label == "OAT") %>%
  filter(time == max(time)) %>%
  pull(value) %>%
  round(1)

last_point = sprintf("Point-mort d'inflation : %s%%", last_data)

subtitle_day = lubridate::day(max(data_$time))
subtitle_month = gsub("\\\\.","", lubridate::month(max(data_$time), label = TRUE))
subtitle_year = lubridate::year(max(data_$time))
xaxis_breaks = seq.Date(from = min(data_$time), to = max(data_$time), by = "3 months")

graph_subtitle = sprintf("Dernier point : %s %s %s, source : Bloomberg\n %s",
                         subtitle_day, subtitle_month, subtitle_year, last_point)


yaxis_breaks = seq(from = floor(min(data_$value, na.rm = TRUE) / 0.5) * 0.5,
                   to = ceiling(max(data_$value, na.rm = TRUE)/ 0.5) * 0.5, by = 0.5)


graph_OAT =
  ggplot(data = data_, aes(x = time, y = value, colour = label)) +
  geom_line(size = 1) +
  ggtitle("Rendement de l'OAT à 10 ans et point mort d'inflation") +
  # labs(subtitle = graph_subtitle) +
  scale_y_continuous(sec.axis = dup_axis(), breaks = yaxis_breaks, labels = function(x) paste0(x, "%")) +
  # scale_x_date(breaks = xaxis_breaks, date_labels = "%b %y" ) +
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
graph_OAT

graph_OAT + ggsave(filename = file_graph, width = 12, height = 7)


data_2 = data_ %>%
  filter(str_detect(label, "2029"))

graph_OAT =
  ggplot(data = data_2, aes(x = time, y = value, colour = label)) +
  geom_line(size = 1) +
  ggtitle("Rendement de l'OAT à 10 ans et point mort d'inflation") +
  # labs(subtitle = graph_subtitle) +
  scale_y_continuous(sec.axis = dup_axis(), breaks = yaxis_breaks, labels = function(x) paste0(x, "%")) +
  # scale_x_date(breaks = xaxis_breaks, date_labels = "%b %y" ) +
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
graph_OAT
