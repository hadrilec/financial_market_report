
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
library(pRev)
library(ISOcodes)
library(RColorBrewer)

date = gsub("-","",Sys.Date())
link_cahierFI_graph = "M:/Usuels.dsc/pRev/FI/cahier_FI/graph"
link_cahierFI_excel = "M:/Usuels.dsc/pRev/FI/cahier_FI/excel"
start_time = as.Date("2015-07-01")
# start_time_TOTAL = as.Date("2008-01-01")
start_time_TOTAL = start_time
file_name = paste0("graph_US_labor_market_continued_jobless_claims", ".pdf" )
file_graph = file.path(link_cahierFI_graph, file_name)
file_excel = file.path(link_cahierFI_excel, paste0(gsub(".pdf|graph_","",file_name),".csv"))

fredr_set_key("1e1376b050a44076281adda2fe2e1a32")

states = ISO_3166_2 %>% 
  filter(str_detect(Code, "^US-")) %>% 
  mutate(Code_state = gsub("^US-", "", Code)) %>% 
  filter(Type == "State")

states_code = states %>% pull(Code_state)

ini_claims_key_series = paste0(states_code, "CCLAIMS")
ini_claims_label_series = states %>% pull(Name)

list_df = list()

for(series in ini_claims_key_series){
  df = fredr(series_id = series,
             observation_start = start_time)
  df[,"state"] = ini_claims_label_series[which(ini_claims_key_series == series)]
  list_df[[length(list_df)+1]] = df
}

ini_claims_us = bind_rows(list_df)

ini_claims_us_tot = fredr(series_id = "CCSA", observation_start = start_time_TOTAL )

ini_claims_us_tot[,"state"] = "TOTAL"

colors_ = rev(c("black", brewer.pal(7,'Set1'), brewer.pal(8,'Set2'), brewer.pal(11,'Set3'),
            brewer.pal(9, 'Pastel1'), brewer.pal(8, 'Pastel2'),
            brewer.pal(7, 'Dark2')))

state_order = ini_claims_us %>%
  group_by(state) %>% 
  filter(date == max(date)) %>% 
  arrange(desc(value)) %>%
  pull(state) %>% 
  as.character() %>% 
  rev()

ini_claims_us_tot = ini_claims_us_tot %>% mutate(level = "TOTAL - seasonally adjusted") 

ini_claims_us_ = ini_claims_us %>% 
  mutate(level = "State by state - not seasonally adjusted") %>% 
  filter(date >= start_time) %>% 
  bind_rows(ini_claims_us_tot) %>%
  mutate(state = factor(state, levels = c(state_order, "TOTAL"))) %>% 
  mutate(value = value / 1000)

max_date = ini_claims_us_ %>% 
  group_by(state) %>% 
  filter(date == max(date)) %>% 
  pull(date) %>% 
  min()

ini_claims_us_tot2 = ini_claims_us_ %>% 
  filter(state == "TOTAL") %>% 
  filter(date <= max_date) %>% 
  filter(date >= start_time) %>% 
  mutate(level = "State by state - not seasonally adjusted")

ini_claims_us_tot_last_value = ini_claims_us_tot %>% 
  arrange(desc(date)) %>% 
  slice(1:3) %>% 
  mutate(value = value / 1000) %>% 
  mutate(month_ = month(date, label = T)) %>% 
  mutate(day_ = day(date)) %>% 
  mutate(time = paste(day_, month_))

date_done = gsub("CET","",Sys.time())

subtitle = sprintf("en milliers de demandes, Fait le : %s\n ", date_done)

for(i in 1:nrow(ini_claims_us_tot_last_value)){
  subtitle = paste(subtitle, sprintf("%s : %s, ",
                                     ini_claims_us_tot_last_value[i,"time"],
                                     ini_claims_us_tot_last_value[i,"value"]))
}
desc_var = "Initial jobless claims measure emerging unemployment, and it is released after one week,\n but continued claims data measure the number of persons claiming unemployment benefits,\n and it is released one week later than the initial claims, that's the reason initial have a higher impact in the financial markets. "
caption_plot = sprintf("Insured unemployment is the number of people receiving unemployment benefits.\n%s", desc_var)

gg_continued_jobless_claims_us = 
ggplot() +
  geom_area(data = ini_claims_us_, aes(x = date, y = value, fill = state)) +
  geom_point(data = ini_claims_us_tot2, aes(x = date, y = value), size = 1) +
  facet_wrap(~level, scales = "free") +
  scale_fill_manual(values = colors_) +
  scale_x_date(expand = c(0.01, 0.01)) +
  ggthemes::theme_stata() +
  guides(fill = guide_legend(nrow = 5)) +
  ggtitle("Continued jobless claims") +
  labs(caption = caption_plot, subtitle = subtitle) +
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
gg_continued_jobless_claims_us + ggsave(file_graph, width = 15, height = 10)

export_graph(gg_initial_jobless_claims_us, update = TRUE,
             perim = "FI", folder_name = "jobless_claims_continued")
export_graph(gg_initial_jobless_claims_us, update = TRUE,
             perim = "US", folder_name = "jobless_claims_continued")



