
library(rdbnomics)
library(rsdmx)
library(ggthemes)
library(grid)
library(lubridate)
library(eurostat)
library(fredr)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(RColorBrewer)
library(pRev)

date = gsub("-","",Sys.Date())
link_cahierFI_graph = "M:/Usuels.dsc/pRev/FI/cahier_FI/graph"

file_name = paste0("graph_FED_balance_sheet", ".pdf" )
file_graph = file.path(link_cahierFI_graph, file_name)

start_time = as.Date("2008-01-01")

fredr_set_key("1e1376b050a44076281adda2fe2e1a32")

# Assets: Other: Gold Certificate Account: Wednesday Level (WGCAL)
fed_gold = fredr(series_id = "WGCAL", observation_start = start_time)

# Assets: Other: Special Drawing Rights Certificate Account: Wednesday Level (WOSDRL)
fed_draw_right = fredr(series_id = "WOSDRL", observation_start = start_time)

# Assets: Other: Coin: Wednesday Level (WACL)
fed_coin = fredr(series_id = "WACL", observation_start = start_time)

# Assets: Securities Held Outright: U.S. Treasury Securities: Wednesday Level (WSHOTSL)
fed_sec1 = fredr(series_id = "WSHOTSL", observation_start = start_time)
fed_sec1_ = fed_sec1
names(fed_sec1_)[3] = "WSHOTSL"

# Assets: Securities Held Outright: Federal Agency Debt Securities: Wednesday Level (WSHOFADSL)
fed_sec2 = fredr(series_id = "WSHOFADSL", observation_start = start_time)
fed_sec2_ = fed_sec2
names(fed_sec2_)[3] = "WSHOFADSL"

# Assets: Securities Held Outright: Mortgage-Backed Securities: Wednesday Level (WSHOMCB)
fed_sec3 = fredr(series_id = "WSHOMCB", observation_start = start_time)
fed_sec3_ = fed_sec3
names(fed_sec3_)[3] = "WSHOMCB"

# Assets: Unamortized Discounts on Securities Held Outright: Wednesday Level (WUDSHO)
fed_undisc = fredr(series_id = "WUDSHO", observation_start = start_time)

# Assets: Other: Repurchase Agreements: Wednesday Level (WORAL)
fed_repagr = fredr(series_id = "WORAL", observation_start = start_time)
# Assets: Liquidity and Credit Facilities: Loans: Wednesday Level (WLCFLL)
fed_liqcred = fredr(series_id = "WLCFLL", observation_start = start_time)
# Assets: Net Portfolio Holdings of Maiden Lane LLCs: Net Portfolio Holdings of Maiden Lane LLC: Wednesday Level (WAML1L)
fed_portf = fredr(series_id = "WAML1L", observation_start = start_time)
# Assets: Other: Items in Process of Collection (Less Eliminations From Consolidation): Wednesday Level (WPCLC)
fed_itemcol = fredr(series_id = "WPCLC", observation_start = start_time)
# Assets: Other: Bank Premises: Wednesday Level (WABPL)
fed_bkprem = fredr(series_id = "WABPL", observation_start = start_time)
#  Assets: Central Bank Liquidity Swaps: Central Bank Liquidity Swaps: Wednesday Level (SWPT)
fed_swap = fredr(series_id = "SWPT", observation_start = start_time)
# Assets: Other Factors Supplying Reserve Balances: Foreign Currency Denominated Assets: Wednesday Level (WFCDA)
fed_fctsupp = fredr(series_id = "WFCDA", observation_start = start_time)
# Assets: Other: Other Assets: Wednesday Level (WAOAL)
fed_othass = fredr(series_id = "WAOAL", observation_start = start_time)
# Assets: Unamortized Premiums on Securities Held Outright: Wednesday Level
fed_WUPSHO = fredr(series_id = "WUPSHO", observation_start = start_time)

# total assets
fed_total_asset = fredr(series_id = "WALCL", observation_start = start_time)
names(fed_total_asset)[3] = "WALCL"

df_fed = fed_total_asset %>%
  left_join(fed_sec1_,  by = c("date")) %>%
  left_join(fed_sec3_,  by = c("date")) %>%
  mutate(other = WALCL- WSHOTSL -WSHOMCB) %>%
  select(date, other, WSHOMCB, WSHOTSL) %>%
  pivot_longer(-date, names_to = "variable", values_to = "value") %>%
  mutate(label = case_when(variable == "other" ~ "Autre",
                           variable == "WSHOMCB" ~ "Titre adossé à des hypothèques immobilières",
                           variable == "WSHOTSL" ~ "Obligation du Trésor américain",
                           ))


# df_fed = rbind(fed_gold, fed_draw_right, fed_coin, fed_sec1, fed_sec2,
#                fed_sec3, fed_undisc, fed_repagr, fed_liqcred, fed_portf,
#                fed_itemcol, fed_bkprem, fed_swap, fed_fctsupp, fed_othass, fed_WUPSHO)


# df_fed = df_fed %>%
#   mutate(name = case_when(series_id == "WGCAL" ~ "Gold Certificate",
#                           series_id == "WOSDRL" ~ "Special Drawing Rights Certificate",
#                           series_id == "WACL" ~ "Coin",
#                           series_id == "WSHOTSL" ~ "US Treasury Securities",
#                           series_id == "WSHOFADSL" ~ "Federal Agency Debt Securities",
#                           series_id == "WSHOMCB" ~ "Mortgage-Backed Securities",
#                           series_id == "WUDSHO" ~ "Unamortized Discounts on Securities Held Outright",
#                           series_id == "WORAL" ~ "Repurchase Agreements",
#                           series_id == "WLCFLL" ~ "Liquidity and Credit Facilities",
#                           series_id == "WAML1L" ~ "Net Portfolio Holdings of Maiden Lane LLCs",
#                           series_id == "WPCLC" ~ "Items in Process of Collection",
#                           series_id == "WABPL" ~ "Bank Premises",
#                           series_id == "SWPT" ~ "Central Bank Liquidity Swaps",
#                           series_id == "WFCDA" ~ "Other Factors Supplying Reserve Balances",
#                           series_id == "WAOAL" ~ "Other Assets",
#                           series_id == "WUPSHO" ~ "Unamortized Premiums on Securities Held Outright"
#                           )) %>%
  # mutate(name = case_when(
  #                         series_id == "WSHOTSL" ~ "Obligation du Trésor américain",
  #                         series_id == "WSHOMCB" ~ "Titre adossé à des hypothèques immobilières",
  #                         TRUE ~ as.character("Autre")
  # )) %>%
  # group_by(date, name) %>%
  # summarise(value = sum(value, na.rm = TRUE))

df = df_fed
df[,"value"] = df[,"value"] / 1000

caption_order = df %>%
  dplyr::filter(date == max(df$date)) %>%
  arrange(desc(value)) %>%
  pull(label)


df = as.data.frame(df)
df[,"label"] = factor(df[,"label"], levels = caption_order)


subtitle_day = lubridate::day(max(df$date))
subtitle_month = gsub("\\\\.","",
                      lubridate::month(max(df$date), label = TRUE))
subtitle_year = lubridate::year(max(df$date))
xaxis_breaks = seq.Date(from = min(df$date), to = max(df$date), by = "3 months")

last_value = df %>%
  group_by(label) %>%
  dplyr::filter(date == max(date)) %>%
  drop_na() %>%
  pull(value) %>%
  sum()

bs_size = sprintf("Taille du bilan : %s milliards de dollars", last_value)

graph_subtitle = sprintf("Dernier point : %s %s %s, source : FED\n%s",
                         subtitle_day, subtitle_month, subtitle_year, bs_size)

mycolors = c(brewer.pal(7, "Set1"), brewer.pal(8, "Set2"), brewer.pal(8, "Set3"))

df_ = df %>% drop_na()
# df_ = df %>% drop_na() %>% filter(date >= "2010-01-01")

graph_fed_bs =
  ggplot() +
  geom_area(data = df_, aes(x = date, y = value, fill = label)) +
  ggtitle("Bilan de la Réserve Fédérale américaine") +
  scale_fill_manual(values = mycolors) +
  scale_y_continuous(sec.axis = dup_axis()) +
  scale_x_date(breaks = xaxis_breaks, date_labels = "%b %y" ,
               expand = c(0.01,0.01)) +
  labs(y = "en millard de dollars",
       subtitle = graph_subtitle) +
  theme_stata() +
  theme(
    plot.title   = element_text(lineheight = 0.8, face = "bold",
                                hjust = 0.5, size = 18),
    axis.text.x  = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y  = element_text(angle = 0, hjust = 1, size = 12),
    plot.subtitle = element_text(size = 12),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 12),
    legend.text = element_text(size = 10),
    legend.title = element_blank(),
    legend.position = "bottom"
  )

graph_fed_bs + ggsave(filename = file_graph, width = 20, height = 11)




# export_minio_graph(graph_fed_bs, perim = "FI", folder_name = "fed_balance_sheet", update = TRUE)

gdp = fredr(series_id = "GDP", observation_start = start_time) %>%
  dplyr::rename(gdp = value)

gdp_Q4_2019 = gdp %>% filter(date == "2019-10-01") %>% pull(gdp)

df_tot = df %>%
  group_by(date) %>%
  summarise(value = sum(value)) %>%
  mutate(bs_gdp_ratio = 100 * value / (gdp_Q4_2019)) %>%
  mutate(growth = 100 * (value/value[date == "2020-02-26"]-1)) %>%
  mutate(gap = value - value[date == "2020-02-26"])

df %>%
  filter(date == "2021-05-26")

1371.843 + 2474.060 + 2.347
