
library(tidyverse)
library(eurostat)

link_cahierFI_graph = Sys.getenv("HOME")
file_name = paste0("graph_menage_expected_inflation", ".pdf" )
file_graph = file.path(link_cahierFI_graph, file_name)

data = get_eurostat("ei_bsco_m") %>%
  mutate(indic_label = label_eurostat(indic, "indic")) %>%
  mutate(unit_label = label_eurostat(unit, "unit"))

list_indic = data %>% distinct(indic, indic_label)

# BS-PT-NY : price trends over the next 12 months

df = data %>%
  filter(indic == "BS-PT-NY") %>%
  filter(geo %in% c("DE", "FR", "ES", "IT")) %>%
  filter(s_adj == "SA") %>%
  filter(time >= "2015-01-01")

gg = 
ggplot(df, aes(x = time, y = values)) +
  geom_line() +
  geom_point() +
  facet_wrap(~geo) +
  ggtitle("Anticipations d'inflation par les ménages européens - soldes d'opinion") +
  labs(subtitle = "Source: Eurostat") +
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

df2 = df %>%
  group_by(geo) %>%
  mutate(diff = values - dplyr::lag(values))

file_name = paste0("graph_menage_expected_inflation2", ".pdf" )
file_graph = file.path(link_cahierFI_graph, file_name)

gg = 
ggplot(df2, aes(x = time, y = diff)) +
  geom_line() +
  geom_point() +
  facet_wrap(~geo) +
  ggtitle("Anticipations d'inflation par les ménages européens - soldes d'opinion") +
  labs(subtitle = "Source: Eurostat") +
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
