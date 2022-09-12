
library(tidyverse)
library(rwebstat)
library(lubridate)
library(RColorBrewer)

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

bsi_series_ = bsi_series %>% 
  mutate(variable = .[[1]]) %>% 
  filter(variable %in% list_series_) %>% 
  select(variable, Title)

data_ = data %>% 
  mutate(date = as.Date(date)) %>% 
  pivot_longer(-date, names_to = "variable", values_to = "value") %>% 
  left_join(bsi_series_, by = "variable") %>% 
  mutate(title = gsub(", encours \\(CVS-CJO\\)", "", Title)) %>%
  mutate(value = value / 1000) %>% 
  mutate(month = month(date)) %>% 
  group_by(date) %>% 
  mutate(total = sum(value, na.rm = T)) %>% 
  group_by(variable) %>% 
  mutate(flux = value - dplyr::lag(value)) %>% 
  mutate(contrib = 100 * flux / total) %>% 
  group_by(variable, month) %>% 
  mutate(GA = 100 * (value/dplyr::lag(value)-1)) %>% 
  ungroup() %>% 
  select(-total, -Title, -month) %>% 
  dplyr::rename(encours = value) %>% 
  pivot_longer(-c("date", "variable", "title"),
               names_to = "type", values_to = "value") %>% 
  mutate(type_label = case_when(type == "encours" ~ "Agrégat monétaire M3 en milliards d'euros",
                                type == "GA" ~ "Glissement annuel en %",
                                type == "flux" ~ "Flux en milliards d'euros",
                                type == "contrib" ~ "Contribution à la variation de M3 en %"))
  
type_label_order = data_ %>% 
  distinct(type, type_label) %>% 
  mutate(type = factor(type, levels = c("encours", "flux", "contrib", "GA"))) %>% 
  arrange(type) %>% 
  pull(type_label) %>% 
  as.character()

data_plot = data_ %>% 
  filter(date >= "2000-01-01") %>%
  mutate(type_label = factor(type_label, levels = type_label_order))

colors_ = c(brewer.pal(7,'Set1'), brewer.pal(8,'Set2'), brewer.pal(11,'Set3'),
            brewer.pal(8, 'Pastel1'), brewer.pal(8, 'Pastel2'),
            brewer.pal(8, 'Dark2'))

ggplot(data_plot, aes(x = date, y = value, fill = title)) +
  facet_wrap(~type_label, scales = "free") +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = colors_) +
  scale_x_date(expand = c(0.01, 0.01)) +
  ggthemes::theme_stata() +
  guides(fill = guide_legend(ncol = 3))



