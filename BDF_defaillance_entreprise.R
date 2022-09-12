
library(tidyverse)
library(rwebstat)
library(lubridate)
library(RColorBrewer)

link_cahierFI_graph = Sys.getenv("HOME")
file_name = paste0("graph_BDF_defaillance_entreprise", ".pdf" )
file_graph = file.path(link_cahierFI_graph, file_name)

webstat_client_ID <- 'd85fb7da-a306-469f-9b60-2e35d251611b'

# BDF datasets available
dataset_all = w_datasets()

# BDF series available in Observatoire des entreprises
datasets_ = w_series_list("DIREN") 

# 
# ENTREPRISE PAR SECTEUR ####
# 

df_defaillance_secteur = 
  datasets_ %>% 
  dplyr::filter(FREQ == "M") %>% 
  dplyr::filter(REF_AREA == "FR") %>% 
  # nombres de défaillances
  dplyr::filter(DIREN_OBJET == "DF") %>% 
  # cumul sur 12 mois
  dplyr::filter(DIREN_MESURE == "03") %>% 
  dplyr::filter(DIREN_SECTACT != "ZZ")

series_defaillance_secteur = paste0(df_defaillance_secteur[,1], collapse = "+")

data_defaillance_secteur = w_data(dataset_name = "DIREN",
                                  series_defaillance_secteur)

metadata_defaillance_secteur = 
  w_meta(data_defaillance_secteur) %>% 
  t() %>% 
  as.data.frame() %>% 
  mutate(series = rownames(.)) %>% 
  dplyr::rename(title = V2) %>% 
  select(title, series)

data_defaillance_secteur_ = data_defaillance_secteur %>% 
  mutate(date = as.Date(as.character(date))) %>% 
  pivot_longer(-date, names_to = "series", values_to = "value") %>% 
  left_join(metadata_defaillance_secteur, by = "series") %>% 
  separate(col = "series", into = paste0("col", 1:10), remove = F) %>% 
  dplyr::rename(secteur = col9) %>% 
  dplyr::filter(!secteur %in% c("IM", "HS")) %>% 
  dplyr::rename(type = col10) %>%
  mutate(type_label = case_when(type %in% c("TT", "NP")  ~ "Toutes tailles",
                                type == "PM" ~ "PME",
                                TRUE ~ as.character(type))) %>% 
  select(-contains("col")) %>% 
  mutate(title = gsub(", Cumul mensuel \\(CVS\\-CJO\\)|Défaillances : |\\, Unités légales|, PME|, nombre cumulé sur 12 mois", "",
                      as.character(title))) %>% 
  mutate(title = gsub("action sociale et services aux ménages", "", title)) %>% 
  mutate(title = paste(secteur, title, sep = " - "))

data_plot = 
  data_defaillance_secteur_ %>% 
  dplyr::filter(date >= "2015-01-01") 

data_tot = w_data(dataset_name = "DIREN", "DIREN.M.FR.DE.UL.DF.03.N.ZZ.TT") %>% 
  pivot_longer(-date, names_to = "series", values_to = "value") %>% 
  # mutate(title = "TOTAL") %>% 
  mutate(type_label = "Toutes tailles") %>% 
  dplyr::filter(date >= "2015-01-01") 

colors_ = c(brewer.pal(7,'Set1'), brewer.pal(8,'Set2'), brewer.pal(11,'Set3'),
            brewer.pal(8, 'Pastel1'), brewer.pal(8, 'Pastel2'),
            brewer.pal(8, 'Dark2'))

last_values = data_plot %>% 
  drop_na() %>% 
  dplyr::filter(date == max(date)) %>% 
  dplyr::filter(type == "TT") 

last_date = last_values %>% pull(date) %>% max()

last_value_tot = last_values %>% 
  pull(value) %>% 
  sum(na.rm = T)

subtt = sprintf("Source : Banque de France, Dernier point : %s, Défaillances totales : %s, NB : chiffres cumulés sur 12 mois", last_date, last_value_tot)

gg_defaillance_entreprise = 
ggplot(data_plot, aes(x = date, y = value)) +
  facet_wrap(~type_label) +
  geom_bar(stat = "identity", position = "stack", aes(fill = title)) +
  # geom_point(data = data_tot, show.legend = F) +
  scale_fill_manual(values = colors_) +
  scale_x_date(expand = c(0.01, 0.01)) +
  ggthemes::theme_stata() +
  ggtitle("Défaillances d'entreprises") +
  labs(subtitle = subtt) + 
  guides(fill = guide_legend(ncol = 3)) +
  theme(
    plot.caption = element_text(hjust = 0.5),
    plot.title   = element_text(lineheight = 0.8, face = "bold", hjust = 0.5, size = 18),
    axis.text.x  = element_text(angle = 45, hjust = 1),
    axis.text.y  = element_text(angle = 0, hjust = 1),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.title = element_blank(),
    legend.position = "bottom"
  ) 


gg_defaillance_entreprise %>%  ggsave(filename = file_graph, width = 15, height = 10)


