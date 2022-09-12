
library(tidyverse)
library(rwebstat)
library(lubridate)

runtime_start = Sys.time()


link_cahierFI_graph = Sys.getenv("HOME")
file_name = paste0("graph_BDF_credit_menage", ".pdf" )
file_graph = file.path(link_cahierFI_graph, file_name)

list_series = c("BSI1.M.FR.Y.R.A26.A.4.U6.2250.Z01.E",
                "BSI1.M.FR.N.R.A26.A.1.U6.2250.Z01.E",
                "BSI1.M.FR.N.A.L20.A.1.U6.2250.Z01.E")

data = rwebstat::w_data(dataset_name = "BSI1",
                        startPeriod = 2000,
                        series_name = paste0(list_series, collapse = "+"))

metadata = rwebstat::w_meta(data) %>% 
  slice(2) %>% 
  select(-1) %>% 
  pivot_longer(names(.), names_to = "variable", values_to = "title")

df = data %>% 
  pivot_longer(-date, names_to = "variable", values_to = "value") %>% 
  left_join(metadata, by = "variable") %>% 
  mutate(value = as.numeric(value)) %>% 
  mutate(date = as.Date(date)) %>% 
  drop_na()

time_now = with_tz(now(), "Europe/Paris")

subtt = sprintf("Fait le : %s, Source : Banque de France", time_now)

gg_credit_depot_menage = 
ggplot(df, aes(x = date, y = value, colour = title)) +
  facet_wrap(~title, scales = "free", ncol = 1) +
  geom_line(size = 1) +
  ggthemes::theme_stata() +
  ggtitle("Crédits et dépôts des ménages") +
  labs(subtitle = subtt) +
  theme(
    plot.caption = element_text(hjust = 0.5),
    plot.title   = element_text(lineheight = 0.8,
                                face = "bold", hjust = 0.5, size = 18),
    axis.text.x  = element_text(angle = 45, hjust = 1),
    axis.text.y  = element_text(angle = 0, hjust = 1),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.title = element_blank(),
    legend.position = "bottom"
  ) + guides(colour = guide_legend(nrow = 3))

gg_credit_depot_menage %>%  ggsave(filename = file_graph, width = 15, height = 10)
