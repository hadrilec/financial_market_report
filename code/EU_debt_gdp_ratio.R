
library(eurostat)
library(tidyverse)
library(lemon)
library(insee)

link_cahierFI_graph = Sys.getenv("HOME")
file_name = paste0("graph_EU_GDP_DEBT_COUNTRY", ".pdf" )
file_graph = file.path(link_cahierFI_graph, file_name)

debtQ_raw = get_eurostat("gov_10q_ggdebt") %>%
  filter(geo %in% c("FR", "DE", "ES", "IT")) %>%
  mutate(na_item_label = label_eurostat(na_item, "na_item")) %>%
  mutate(sector_label = label_eurostat(sector, "sector"))

list_na_item = debtQ_raw %>% distinct(na_item, na_item_label)
list_sector = debtQ_raw %>% distinct(sector, sector_label)

debtQ = debtQ_raw %>%
  filter(sector == "S13") %>%
  filter(na_item == "GD") %>%
  filter(unit == "PC_GDP") %>%
  mutate(label = "Dette publique consolidée")


interest_raw =
  get_eurostat("gov_10dd_edpt1") %>%
  mutate(na_item_label = label_eurostat(na_item, "na_item")) %>%
  mutate(sector_label = label_eurostat(sector, "sector")) %>%
  mutate(time = as.Date(gsub("-01-01", "-10-01", time)))

coeff_scale = 20

interest = interest_raw %>%
  filter(na_item == "D41PAY") %>%
  filter(geo %in% c("DE", "FR", "ES", "IT")) %>%
  filter(unit == "PC_GDP") %>%
  mutate(values = values * coeff_scale) %>%
  mutate(label = "Intérêts sur la dette publique")


data =
  debtQ %>%
  bind_rows(interest) %>%
  mutate(geo_label = case_when(geo == "DE" ~ "Allemagne",
                               geo == "FR" ~ "France",
                               geo == "ES" ~ "Espagne",
                               geo == "IT" ~ "Italie")) %>%
  filter(time >= '2000-01-01')

gg_debt = 
ggplot(data = data, aes(x = time, y = values, colour = label)) + #na_item_label
  geom_line() +
  geom_point() +
  scale_color_manual(values = c("blue", "red")) +
  scale_y_continuous(sec.axis = sec_axis(~./coeff_scale)) +
  ggtitle("Dette publique et charge d'intérêts") +
  labs(subtitle = "Source : Eurostat, Unité :  point de pourcentage de PIB") +
  lemon::facet_rep_wrap(~geo_label, repeat.tick.labels = "all") +
  ggthemes::theme_stata() +
  theme(
    plot.title   = element_text(lineheight = 0.8, face = "bold", hjust = 0.5, size = 18),
    axis.text.x  = element_text(angle = 45, hjust = 1),
    axis.text.y  = element_text(angle = 0, hjust = 1),
    axis.text.y.right = element_text(color = "red"),
    axis.line.y.right = element_line(color = "red"),
    axis.ticks.y.right = element_line(color = "red"),
    axis.text.y.left = element_text(color = "blue"),
    axis.line.y = element_line(color = "blue"),
    axis.ticks.y = element_line(color = "blue"),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.title = element_blank(),
    legend.position = "bottom"
  )

gg_debt %>% ggsave(filename = file_graph, width = 15, height = 10)

data_last_date=
  data %>%
  # filter(label == "GD") %>%
  filter(time %in% c(as.Date("2019-10-01"), as.Date("2020-10-01")))

degt_gep_ratio_us = fredr(series_id = "GFDEGDQ188S", observation_start = start_time) %>% arrange(desc(date))

library(insee)

dt = get_dataset_list()

id = get_idbank_list("DETTE-NEGOCIABLE-ETAT", "DETTE-TRIM-APU-2014")

debt = get_insee_idbank("010596756")


dataFR = data %>%
  filter(geo == "FR") %>%
  filter(time >= '2019-01-01') %>%
  arrange(time) %>%
  arrange(na_item)

dataFR

gdp_raw = get_eurostat("nama_10_gdp") %>%
  mutate(na_item_label = label_eurostat(na_item, "na_item")) %>%
  mutate(unit_label = label_eurostat(unit, "unit"))

gdp = gdp_raw %>%
  filter(na_item == "B1GQ") %>%
  filter(unit %in% c("CP_MEUR")) %>%  #"CLV15_MEUR"
  filter(geo %in% c("EA19")) %>%
  filter(time >= "2018-01-01") #%>%
  # mutate(REF_AREA = geo,
  #        gdp = values) %>%
  # mutate(year = lubridate::year(time)) %>%
  # select(REF_AREA, gdp, year)




