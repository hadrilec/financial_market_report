
library(tidyverse)
library(rwebstat)
library(lubridate)
library(RColorBrewer)

link_cahierFI_graph = Sys.getenv("HOME")
file_name = paste0("graph_BDF_epargne_menage_agregat_M3", ".pdf" )
file_graph = file.path(link_cahierFI_graph, file_name)

webstat_client_ID <- 'd85fb7da-a306-469f-9b60-2e35d251611b'

bsi_series =
  w_series_list("BSI1") %>%
  filter(FREQ == "M") %>%
  filter(REF_AREA == "FR")

# BSI1.M.FR.Y.V.L22.L.1.U6.2300.Z01.E
# Dépôts à terme négocié, jusqu'à 2 ans, encours (CVS-CJO)

# BSI1.M.FR.Y.V.L23.D.1.U6.2300.Z01.E
# Dépôts remboursables avec préavis, jusqu'à 3 mois, encours (CVS-CJO)

# Dépôts à vue
# BSI1.M.FR.Y.V.L21.A.1.U6.2300.Z01.E

# BSI1.M.FR.Y.V.L30.A.1.U6.2300.Z01.E
# Titres d'OPC monétaires, encours (CVS-CJO)

# BSI1.M.FR.Y.U.L24.A.1.U6.2300.Z01.E
# Pensions

# BSI1.M.FR.Y.V.L40.L.1.U6.2300.Z01.E
# titres de créances < 2 ans

# BSI1.M.FR.Y.V.AXG.X.1.U5.2300.Z01.E
# Avoirs monétaires bruts vis-à-vis du reste de la zone euro, encours (CVS-CJO)

# Engagements monétaires bruts vis-à-vis du reste de la zone euro, encours (CVS-CJO)
# BSI1.M.FR.Y.V.LXG.X.1.U5.2300.Z01.E

list_series_ = c("BSI1.M.FR.Y.V.L22.L.1.U6.2300.Z01.E",
                 "BSI1.M.FR.Y.V.L23.D.1.U6.2300.Z01.E",
                 "BSI1.M.FR.Y.V.L21.A.1.U6.2300.Z01.E",
                 "BSI1.M.FR.Y.V.L30.A.1.U6.2300.Z01.E",
                 "BSI1.M.FR.Y.U.L24.A.1.U6.2300.Z01.E",
                 "BSI1.M.FR.Y.V.L40.L.1.U6.2300.Z01.E",
                 "BSI1.M.FR.Y.V.AXG.X.1.U5.2300.Z01.E",
                 "BSI1.M.FR.Y.V.LXG.X.1.U5.2300.Z01.E")

list_series = paste0(list_series_, collapse = "+")

data = w_data(dataset_name = "BSI1",
               series_name = list_series)

meta = rwebstat::w_meta(data)

bsi_series_ = bsi_series %>%
  mutate(variable = .[[1]]) %>%
  filter(variable %in% list_series_) %>%
  select(variable, Title)

data_ = data %>%
  mutate(date = as.Date(as.character(date))) %>%
  pivot_longer(-date, names_to = "variable", values_to = "value") %>%
  left_join(bsi_series_, by = "variable") %>%
  mutate(title = gsub(", encours \\(CVS-CJO\\)", "", Title)) %>%
  mutate(value = value / 1000) %>%
  mutate(month = month(date)) %>%
  group_by(date) %>%
  mutate(total = sum(value, na.rm = T)) %>%
  group_by(variable) %>%
  mutate(GT = 100 * (value/dplyr::lag(value)-1)) %>%
  mutate(flux = value - dplyr::lag(value)) %>%
  group_by(variable, month) %>%
  mutate(GA = 100 * (value/dplyr::lag(value)-1)) %>%
  mutate(contrib = 100 * (value - dplyr::lag(value)) / dplyr::lag(total)) %>%
  ungroup() %>%
  select(-total, -Title, -month) %>%
  dplyr::rename(encours = value) %>%
  pivot_longer(-c("date", "variable", "title"),
               names_to = "type", values_to = "value") %>%
  mutate(type_label = case_when(type == "encours" ~ "Agrégat monétaire M3 en milliards d'euros",
                                type == "GA" ~ "Glissement annuel en %",
                                type == "GT" ~ "Glissement trimestriel en %",
                                type == "flux" ~ "Flux en milliards d'euros",
                                type == "contrib" ~ "Contribution au glissement annuel de M3 en %"))

type_label_order = data_ %>%
  distinct(type, type_label) %>%
  mutate(type = factor(type, levels = c("encours", "flux", "contrib","GT" ,"GA"))) %>%
  arrange(type) %>%
  pull(type_label) %>%
  as.character()

colors_ = c(brewer.pal(7,'Set1'), brewer.pal(8,'Set2'), brewer.pal(11,'Set3'),
            brewer.pal(8, 'Pastel1'), brewer.pal(8, 'Pastel2'),
            brewer.pal(8, 'Dark2'))

last_values = data_ %>%
  group_by(variable) %>%
  filter(date == max(date)) %>%
  mutate(value = round(value, 1)) %>%
  filter(type == "flux")

variable_order = data_ %>%
  group_by(variable) %>%
  filter(date == max(date)) %>%
  filter(type == "encours") %>%
  arrange(desc(value)) %>%
  pull(title) %>%
  as.character()

data_plot = data_ %>%
  filter(date >= "2018-01-01") %>%
  mutate(type_label = factor(type_label, levels = type_label_order)) %>%
  mutate(title = factor(title, levels = variable_order)) %>%
  filter(type != "GA") %>%
  filter(type != "GT")

data_plot_GT = data_ %>%
  filter(date >= "2018-01-01") %>%
  mutate(type_label = factor(type_label, levels = type_label_order)) %>%
  mutate(title = factor(title, levels = variable_order)) %>%
  filter(type == "GT") %>%
  filter(title != "Pensions") %>%
  filter(!str_detect(title, "Avoirs moné|Titre de créan"))

depot_avue_val = last_values %>%
  filter(title == "Dépôts à vue ") %>%
  pull(value)

depot_term_remb_val = last_values %>%
  filter(variable %in% c("BSI1.M.FR.Y.V.L23.D.1.U6.2300.Z01.E", "BSI1.M.FR.Y.V.L22.L.1.U6.2300.Z01.E")) %>%
  pull(value) %>%
  sum()



last_date = last_values %>% pull(date) %>% max()

last_values_subtt = sprintf("Dernier point : %s, flux de dépôts à vue : %s, flux de dépôts à termes et remboursables : %s milliards d'euros",
                            last_date, depot_avue_val, depot_term_remb_val)

def_M3 = "M3 - M2 (résidents) = Titres d'OPC monétaires + Pensions + Titres de créances < 2 ans"
agregat_definition = sprintf("M1 = dépôts à vue, M2 - M1 = dépôts à termes et remboursables, %s", def_M3)
subtt = sprintf("Source: Banque de France, %s\n%s", agregat_definition, last_values_subtt)

caption_ga = "L'empilement des glissements annuels permet de simplifier la visualisation"
caption_pension = "La pension de titres est une cession temporaire de titres financiers. L'opération répond au besoin de liquidité des marchés"
caption_depot_avue = "Dépôts à vue = compte courant + compte chèque"
caption_depot_terme = "Un dépôt à terme est une somme bloquée sur un compte bancaire par un particulier ou une entreprise en contrepartie du versement d'intérêts."
caption_depot_rmb = ""
caption = sprintf("%s\n%s\n%s\n%s", caption_ga, caption_pension, caption_depot_avue, caption_depot_terme)

gg_epargne  =
ggplot() +
  facet_wrap(~type_label, scales = "free") +
  geom_bar(data = data_plot, aes(x = date, y = value, fill = title),
           stat = "identity", position = "stack") +
  geom_bar(data = data_plot_GT, aes(x = date, y = value, fill = title),
           stat = "identity", position = "dodge") +
  scale_fill_manual(values = colors_) +
  scale_x_date(expand = c(0.01, 0.01)) +
  ggthemes::theme_stata() +
  guides(fill = guide_legend(ncol = 3)) +
  ggtitle("Epargne des ménages en France - agrégat monétaire M3") +
  labs(caption = caption, subtitle = subtt) +
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

gg_epargne  %>%  ggsave(filename = file_graph, width = 15, height = 10)

data_epargne = data_ %>% 
  filter(str_detect(type_label, "Flux")) %>% 
  filter(date >= "2008-01-01") %>% 
  filter(!str_detect(title, "Engagements monétaires|Avoirs monétaires|Pension"))

file_name = paste0("graph_epargne_menage_flux", ".pdf" )
file_graph = file.path(link_cahierFI_graph, file_name)

ggplot() +
facet_wrap(~type_label, scales = "free") +
geom_bar(data = data_epargne, aes(x = date, y = value, fill = title),
         stat = "identity", position = "stack")+
scale_fill_manual(values = colors_) +
scale_x_date(expand = c(0.01, 0.01)) +
ggthemes::theme_stata() +
guides(fill = guide_legend(ncol = 3)) +
ggtitle("Epargne des ménages en France") +
theme(
  plot.caption = element_text(hjust = 0.5),
  plot.title   = element_text(lineheight = 0.8, face = "bold", hjust = 0.5, size = 18),
  axis.text.x  = element_text(angle = 45, hjust = 1),
  axis.text.y  = element_text(angle = 0, hjust = 1),
  axis.title.x = element_blank(),
  axis.title.y = element_blank(),
  legend.title = element_blank(),
  legend.position = "bottom"
) %>%  ggsave(filename = file_graph, width = 15, height = 10)
