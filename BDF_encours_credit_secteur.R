
library(tidyverse)
library(lubridate)
library(rwebstat)

link_cahierFI_graph = Sys.getenv("HOME")
file_name = paste0("graph_BDF_encours_credit_secteur", ".pdf" )
file_graph = file.path(link_cahierFI_graph, file_name)


webstat_client_ID <- 'd85fb7da-a306-469f-9b60-2e35d251611b'

catalogue = w_datasets()

df = rwebstat::w_data(dataset_name = "DIREN", series_name = "M.FR.CR.LME.EN.01.N.*.TT")
# df2 = rwebstat::w_data(dataset_name = "DIREN", series_name = "M.FR.CR.LME.EN+ME.01.N.*.TT")
# 07 a la place de 01 pour obtenir les glissements plutot que les encours

metadata = rwebstat::w_meta(df) %>%
  slice(2, 4) %>%
  select(-1) %>%
  t() %>%
  as.data.frame()

metadata[,"variable"] = as.character(rownames(metadata))
names(metadata) = c("title", "title_long", "variable")

data = df %>%
  pivot_longer(-date, names_to = "variable", values_to = "value") %>%
  left_join(metadata) %>%
  separate(title, paste0("col", 1:3), sep = "\\,", remove = F) %>%
  mutate(title_ = gsub("Crédits mobilisés et mobilisables", "", title)) %>%
  filter(variable != "DIREN.M.FR.CR.LME.EN.01.N.NC.TT") %>%
  mutate(col2 = case_when(variable == "DIREN.M.FR.CR.LME.EN.01.N.ZZ.TT" ~ "TOTAL",
                          TRUE ~ as.character(col2))) %>%
  mutate(value = value / 1000)
# exclusion des non classés

data_GA = data %>%
  mutate(month = month(date)) %>%
  arrange(date) %>%
  group_by(month, col2) %>%
  mutate(GA = 100*(value/dplyr::lag(value)-1)) %>%
  group_by(col2) %>%
  filter(date == max(date)) %>%
  select(GA, col2) %>%
  mutate(GA = case_when(GA > 0 ~ paste0("+", round(GA,1), "%"),
                        TRUE ~ as.character(paste0(round(GA,1), "%"))))

test = data %>% filter(is.na(col3))

last_values_date =
  data %>%
  pull(date) %>%
  max()

data_plot = data %>%
  left_join(data_GA, by = "col2") %>%
  mutate(col2 = paste(col2, GA))

gg =
  ggplot(data_plot, aes(x = date, y = value, colour = col2)) +
    geom_line() +
    facet_wrap(~col2, scales = "free") +
    ggtitle("Encours de crédits aux entreprises mobilisés et mobilisables") +
    labs(subtitle = sprintf("Dernier point : %s, unité : milliards d'euros", last_values_date),
         caption = "Glissements annuels du dernier mois affichés dans les titres") +
    ggthemes::theme_stata() +
    theme(
      plot.title   = element_text(lineheight = 0.8, face = "bold", hjust = 0.5, size = 18),
      axis.text.x  = element_text(angle = 45, hjust = 1),
      axis.text.y  = element_text(angle = 0, hjust = 1),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      legend.title = element_blank(),
      legend.position = "bottom"
    ) 
gg %>% ggsave(filename = file_graph, width = 15, height = 10)

bdf_series_credit = c(
"DIREN.M.FR.CR.LME.EN.01.N.AZ.TT",
"DIREN.M.FR.CR.LME.EN.01.N.BE.TT",
"DIREN.M.FR.CR.LME.EN.01.N.C.TT",
"DIREN.M.FR.CR.LME.EN.01.N.FZ.TT",
"DIREN.M.FR.CR.LME.EN.01.N.G.TT",
"DIREN.M.FR.CR.LME.EN.01.N.H.TT",
"DIREN.M.FR.CR.LME.EN.01.N.I.TT",
"DIREN.M.FR.CR.LME.EN.01.N.JZ.TT",
"DIREN.M.FR.CR.LME.EN.01.N.LZ.TT",
"DIREN.M.FR.CR.LME.EN.01.N.MN.TT",
"DIREN.M.FR.CR.LME.EN.01.N.NC.TT",
"DIREN.M.FR.CR.LME.EN.01.N.PS.TT",
"DIREN.M.FR.CR.LME.EN.01.N.ZZ.TT",
"DIREN.M.FR.CR.LME.EN.07.N.AZ.TT",
"DIREN.M.FR.CR.LME.EN.07.N.BE.TT",
"DIREN.M.FR.CR.LME.EN.07.N.C.TT",
"DIREN.M.FR.CR.LME.EN.07.N.FZ.TT",
"DIREN.M.FR.CR.LME.EN.07.N.G.TT",
"DIREN.M.FR.CR.LME.EN.07.N.H.TT",
"DIREN.M.FR.CR.LME.EN.07.N.I.TT",
"DIREN.M.FR.CR.LME.EN.07.N.JZ.TT",
"DIREN.M.FR.CR.LME.EN.07.N.LZ.TT",
"DIREN.M.FR.CR.LME.EN.07.N.MN.TT",
"DIREN.M.FR.CR.LME.EN.07.N.NC.TT",
"DIREN.M.FR.CR.LME.EN.07.N.PS.TT",
"DIREN.M.FR.CR.LME.EN.07.N.ZZ.TT",
"DIREN.M.FR.CR.LME.ME.01.N.AZ.TT",
"DIREN.M.FR.CR.LME.ME.01.N.BE.TT",
"DIREN.M.FR.CR.LME.ME.01.N.C.TT",
"DIREN.M.FR.CR.LME.ME.01.N.FZ.TT",
"DIREN.M.FR.CR.LME.ME.01.N.G.TT",
"DIREN.M.FR.CR.LME.ME.01.N.H.TT",
"DIREN.M.FR.CR.LME.ME.01.N.I.TT",
"DIREN.M.FR.CR.LME.ME.01.N.JZ.TT",
"DIREN.M.FR.CR.LME.ME.01.N.LZ.TT",
"DIREN.M.FR.CR.LME.ME.01.N.MN.TT",
"DIREN.M.FR.CR.LME.ME.01.N.NC.TT",
"DIREN.M.FR.CR.LME.ME.01.N.PS.TT",
"DIREN.M.FR.CR.LME.ME.01.N.ZZ.TT",
"DIREN.M.FR.CR.LME.ME.07.N.AZ.TT",
"DIREN.M.FR.CR.LME.ME.07.N.BE.TT",
"DIREN.M.FR.CR.LME.ME.07.N.C.TT",
"DIREN.M.FR.CR.LME.ME.07.N.FZ.TT",
"DIREN.M.FR.CR.LME.ME.07.N.G.TT",
"DIREN.M.FR.CR.LME.ME.07.N.H.TT",
"DIREN.M.FR.CR.LME.ME.07.N.I.TT",
"DIREN.M.FR.CR.LME.ME.07.N.JZ.TT",
"DIREN.M.FR.CR.LME.ME.07.N.LZ.TT",
"DIREN.M.FR.CR.LME.ME.07.N.MN.TT",
"DIREN.M.FR.CR.LME.ME.07.N.NC.TT",
"DIREN.M.FR.CR.LME.ME.07.N.PS.TT",
"DIREN.M.FR.CR.LME.ME.07.N.ZZ.TT")
