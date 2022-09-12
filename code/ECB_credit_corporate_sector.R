
library(ggthemes)
library(lubridate)
library(tidyverse)
library(RColorBrewer)


link_cahierFI_graph = Sys.getenv("HOME")
file_name = paste0("graph_ECB_credit_corporate_sector", ".pdf" )
file_graph = file.path(link_cahierFI_graph, file_name)

path_ECB = "https://sdw-wsrest.ecb.europa.eu/service/data/BSI/"
myUrl <- paste0(path_ECB,"Q.U2.N.A.A20EST.A.1.U2..Z01.E")
data <- readSDMX(myUrl)
data <- as.data.frame(data)

df = data %>%
  mutate(time = lubridate::yq(obsTime)) %>%
  filter(time >= "2015-01-01") %>%
  mutate(TITLE = gsub("Loans vis-a-vis euro area NFC", "", TITLE)) %>%
  mutate(TITLE = gsub("reported by MFI excluding ESCB in the euro area \\(stock\\)|\\(|\\)", "", TITLE)) %>%
  mutate(TITLE = gsub("industry activities", "", TITLE)) %>%
  mutate(sector_label = sub(".*industry activities i.e. ", "", TITLE_COMPL)) %>%
  mutate(sector_label = sub(".*industry activities i.e ", "", sector_label)) %>%
  mutate(sector_label = sub("denominated in Euro.*", "", sector_label)) %>%
  mutate(title = paste(TITLE, "-", sector_label)) %>%
  group_by(title) %>%
  mutate(growth = 100 * (obsValue/obsValue[time == "2019-10-01"]-1)) %>%
  ungroup() %>%
  mutate(growth_label = case_when(growth > 0 ~ paste0("+", round(growth, 1), "%"),
                                   TRUE ~ as.character(paste0(round(growth, 1), "%")))) %>%
  group_by(title) %>%
  mutate(title2 = paste(growth_label[time == max(time)], "-", title))


order = df %>%
  distinct(title, title2) %>%
  pull(title2)

df_plot = df %>%
  mutate(title2 = factor(title2, levels = order)) %>%
  mutate(value = obsValue/1000)

last_date = df_plot %>% pull(time) %>% max()

gg =
ggplot(df_plot, aes(x = time, y = value)) +
  geom_line() +
  geom_point() +
  facet_wrap(~title2,
             scales = "free",
             labeller = label_wrap_gen(50)) +
  ggtitle("Loans vis-a-vis euro area NFC") +
  labs(subtitle = "Total stock of loans in billions euros, all currencies, all maturities\nGrowth from Q4 2019 is displayed by sector",
       caption = sprintf("Last date : %s", last_date)) +
  ggthemes::theme_stata() +
  theme(
    plot.title   = element_text(lineheight = 0.8, face = "bold", hjust = 0.5, size = 18),
    axis.text.x  = element_text(angle = 45, hjust = 1),
    axis.text.y  = element_text(angle = 0, hjust = 1),
    axis.title.x = element_blank(),
    strip.text.x = element_text(size = 9),
    legend.title = element_blank(),
    legend.position = "bottom"
  )

gg  %>%  ggsave(filename = file_graph, width = 17, height = 9)
