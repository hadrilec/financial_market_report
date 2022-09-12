
library(tidyverse)
library(eurostat)
library(lubridate)

link_cahierFI_graph =  Sys.getenv("HOME")
file_name = paste0("graph_EU_gov_debt_interest", ".pdf" )
file_graph = file.path(link_cahierFI_graph, file_name)


data = get_eurostat("gov_10dd_edpt1") %>% 
  mutate(na_item_label = label_eurostat(na_item, "na_item"))

list_na_item = data %>% distinct(na_item, na_item_label)

interest = data %>%
  filter(na_item == "D41PAY") %>% 
  filter(geo %in% c("DE", "FR", "ES", "IT")) %>% 
  mutate(geo_label = case_when(geo == "DE" ~ "Allemagne",
                               geo == "FR" ~ "France",
                               geo == "ES" ~ "Espagne",
                               geo == "IT" ~ "Italie"))

interest_eur = interest %>% filter(unit == "MIO_EUR") %>% 
  mutate(label = "Intérêts sur la dette publique en euros")

interest_pct = interest %>% filter(unit == "PC_GDP")

interest_pct2 = interest_pct %>% 
  mutate(values = values * 10000) %>% 
  mutate(label = "Intérêts sur la dette publique en pourcentage de PIB")

# ggplot(interest_pct, aes(x = time, y = values)) + 
#   facet_wrap(~geo) +
#   geom_col()


gg =
ggplot() + 
  facet_wrap(~geo_label, scales = "free") +
  geom_col(data=interest_eur, aes(x = time, y = values, fill = label)) +
  geom_point(data=interest_pct2, aes(x = time, y = values, colour = label)) +
  geom_line(data=interest_pct2, aes(x = time, y = values, colour = label)) +
  scale_fill_manual(values = "grey")+
  scale_color_manual(values = "red") +
  scale_y_continuous(
    # Features of the first axis
    name = "en million d'euros",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~./10000, name="en point de PIB")
  ) +
  ggthemes::theme_stata() +
  ggtitle("Intérêts sur la dette publique") +
  labs(subtitle = "Source: Eurostat",
       caption = "") +
  theme(
    plot.title   = element_text(lineheight = 0.8, face = "bold", hjust = 0.5, size = 18),
    axis.text.x  = element_text(angle = 45, hjust = 1),
    axis.text.y  = element_text(angle = 0, hjust = 1),
    axis.text.y.right = element_text(color = "red"),
    axis.line.y.right = element_line(color = "red"), 
    axis.ticks.y.right = element_line(color = "red"),
    axis.title.x = element_blank(),
    legend.title = element_blank(),
    legend.position = "bottom"
  ) 

gg %>%  ggsave(filename = file_graph, width = 12, height = 7)
