
library(tidyverse)
library(rwebstat)
library(lubridate)

link_cahierFI_graph = Sys.getenv("HOME")
file_name = paste0("graph_BDF_credit_menage", ".pdf" )
file_graph = file.path(link_cahierFI_graph, file_name)

webstat_client_ID <- 'd85fb7da-a306-469f-9b60-2e35d251611b'

# https://www.banque-france.fr/statistiques/credit/credit/credits-aux-particuliers

# BSI1.M.FR.N.R.A210Z.A.1.U6.2254FR.Z01.E
# Crédits à la consommation
credit_conso = rwebstat::w_data(dataset_name = "BSI1", startPeriod = 2000,
                      series_name = "M.FR.N.R.A210Z.A.1.U6.2254FR.Z01.E")

# BSI1.M.FR.N.R.A230Z.A.1.U6.2254FR.Z01.E
# Autres crédits
autre_credit = rwebstat::w_data(dataset_name = "BSI1", startPeriod = 2000,
                                series_name = "M.FR.N.R.A230Z.A.1.U6.2254FR.Z01.E")

# BSI1.M.FR.N.R.A220Z.A.1.U6.2254FR.Z01.E
# Crédits immobiliters
credit_immo = rwebstat::w_data(dataset_name = "BSI1", startPeriod = 2000,
                         series_name = "M.FR.N.R.A220Z.A.1.U6.2254FR.Z01.E")

# BSI1.M.FR.N.R.A26.A.1.U6.2254FR.Z01.E
# Crédits totaux
credit_tot = rwebstat::w_data(dataset_name = "BSI1", startPeriod = 2000,
                              series_name = "M.FR.N.R.A26.A.1.U6.2254FR.Z01.E")

credit_tot_metadata = w_meta(credit_tot)

df = credit_tot %>% 
  rename(value = 2) %>% 
  mutate(diff = value - dplyr::lag(value)) %>% 
  # filter(date >= "2015-01-01") %>% 
  mutate(year = year(date)) 

df_mean = df %>% 
  group_by(year) %>% 
  mutate(mean_diff = mean(diff, na.rm = T),
            min_date = min(date),
            max_date = max(date)) %>%
  ungroup() %>% 
  mutate(diff_mean = 100*(mean_diff / dplyr::lag(mean_diff)-1)) %>% 
  mutate(value_more_ = case_when(date > "2020-02-01" ~ dplyr::lag(value) + 6439,
                                 TRUE ~ as.numeric(value))) %>% 
  mutate(value_more_ = case_when(date > "2020-02-01" ~ dplyr::lag(value_more_) + 6439,
                                 TRUE ~ as.numeric(value))) %>% 
  mutate(drop = 100*(value / value_more_ - 1)) %>% 
  mutate(value_more_ = case_when(date <= "2020-01-01" ~ NA_real_,
                                 TRUE ~ as.numeric(value_more_)))

subtt0 = "source : BDF, serie = M.FR.N.R.A26.A.1.U6.2254FR.Z01.E"
subtt1 = "Les lignes horizontales représentent la moyenne annuelle"
subtt2 = "En 2020 le flux chute de près de 90% par rapport à 2019"

gg1 =
ggplot() +
  geom_line(data = df, aes(x = date, y = diff), colour = "black") +
  geom_segment(data = df_mean, aes(x = date, xend = max_date,
                                   y = mean_diff, yend = mean_diff), 
             colour = "red", linetype = "solid") +
  ggtitle("Flux de crédit totaux des ménages français") +
  labs(subtitle = paste0(subtt0,"\n", subtt1,"\n", subtt2))

subtt = "Ajout de la moyenne mensuelle 2019, en mars et avril 2020"
subtt_ = "L'écart est de -0,5% en mars et -1,4% en avril, entre le réalisé et le contrefactuel"
subtt_2 = "A supposé que l'évolution des taux aurait été la même, on en déduit que cet écart est égal à celui sur les intérêts versés par les ménages"

df_mean_plot = df_mean %>% 
  filter(date >= "2015-01-01")

# hypothèse T2 total moyen :3938488/3 = 1312829
# 1322923 avril
# 1304563 mai
# 1311002 juin

# contre factuel T2 total moyen : 3988086/3 = 1329362
# 1322923 avril
# 1329362 mai
# 1335801 juin

gg2 =
ggplot() +
  geom_line(data = df_mean_plot, aes(x = date, y = value), colour = "black") +
  geom_line(data = df_mean_plot, aes(x = date, y = value_more_),
            colour = "red" , linetype = "dashed") +
  ggtitle("Encours de crédit total des ménages français, contrefactuel en rouge") +
  labs(subtitle = paste0(subtt, "\n", subtt_, "\n", subtt_2))

ggpubr::ggarrange(gg1, gg2, ncol = 1)

date_start = today() %m-% years(5)

data = credit_conso %>% 
  left_join(credit_immo) %>% 
  left_join(autre_credit) %>% 
  pivot_longer(-date, names_to = "variable", values_to = "value") %>% 
  drop_na() %>% 
  mutate(date = as.Date(as.character(date))) %>% 
  mutate(value = value/1000) %>% 
  mutate(label = case_when(variable == "BSI1.M.FR.N.R.A210Z.A.1.U6.2254FR.Z01.E" ~ "Crédits à la consommation",
                           variable == "BSI1.M.FR.N.R.A230Z.A.1.U6.2254FR.Z01.E" ~ "Autres crédits",
                           variable == "BSI1.M.FR.N.R.A220Z.A.1.U6.2254FR.Z01.E" ~ "Crédits immobiliers",
                           variable == "BSI1.M.FR.N.R.A26.A.1.U6.2254FR.Z01.E" ~ "Crédits totaux")) %>% 
  group_by(date) %>% 
  mutate(credit_tot = sum(value)) %>% 
  mutate(month = month(date)) %>% 
  arrange(date) %>% 
  group_by(variable) %>% 
  mutate(flux = value - dplyr::lag(value)) %>% 
  group_by(variable, month) %>% 
  mutate(contrib = 100* (value - dplyr::lag(value))/dplyr::lag(credit_tot)) %>% 
  mutate(GA = 100* (value - dplyr::lag(value))/dplyr::lag(value)) %>% 
  select(-credit_tot) %>% 
  pivot_longer(-c("date", "variable", "label", "month"),
               names_to = "type", values_to = "value")


data_plot = data %>% 
  filter(date >= date_start) %>% 
  # mutate(date = case_when(type == "GA" & label == "Crédits à la consommation" ~ date %m+% days(1),
  #                         type == "GA" & label == "Crédits immobiliers" ~ date %m+% days(2),
  #                         TRUE ~ as.Date(as.character(date)))) %>% 
  mutate(type_ = case_when(type == "value" ~ "Encours en milliards d'euros",
                           type == "contrib" ~ "Contributions au glissement annuel en %",
                           type == "GA" ~ "Glissement annuel en %",
                           type == "flux" ~ "Flux en milliards d'euros",
                           TRUE ~ as.character(type)))

type_order =
  data_plot %>%
  distinct(type, type_) %>% 
  mutate(type = factor(type, levels = c("value", "flux", "contrib", "GA"))) %>% 
  arrange(type) %>% 
  pull(type_) %>% 
  unique()

data_plot = data_plot %>% 
  mutate(type_ = factor(type_, levels = type_order))

data_tot = data %>%  
  filter(type == "value") %>% 
  group_by(date, month) %>% 
  summarise(value = sum(value, na.rm = T)) %>% 
  group_by(month) %>% 
  mutate(contrib = 100* (value - dplyr::lag(value))/dplyr::lag(value)) %>% 
  mutate(type_ = "Contributions au glissement annuel en %", 
         label = "TOTAL") %>%
  filter(date >= date_start) %>% 
  ungroup() 

last_values_tot = data_tot %>% 
  filter(date == max(date))


last_values = data_plot %>% 
  filter(date == max(date)) %>% 
  pull(date) %>% 
  max()

subtt = sprintf("Source : Banque de France, Dernier point : %s", max(data_plot$date))
caption = "l'empilement des glissements annuels permet de simplifier la visualisation"


gg_credit_menage = 
ggplot() +
  facet_wrap(~type_, scales = "free", ncol = 2) +
  geom_bar(data = data_plot, aes(x = date, y = value, fill = label),
    stat = "identity", position = "stack") +
  # geom_point(data = data_tot, aes(x = date, y = contrib), show.legend = F) +
  # geom_line(data = data_tot, aes(x = date, y = contrib),
  #           linetype = "dashed",
  #           show.legend = F) +
  ggtitle("Crédits aux particuliers") +
  labs(subtitle = subtt, caption = caption) +
  scale_x_date(expand = c(0.01, 0.01)) +
  ggthemes::theme_stata() +
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


gg_credit_menage %>%  ggsave(filename = file_graph, width = 15, height = 10)


