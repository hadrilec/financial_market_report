
library(rdbnomics)
library(rsdmx)
library(ggthemes)
library(grid)
library(lubridate)
library(eurostat)
library(fredr)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(RColorBrewer)

date = gsub("-","",Sys.Date())

link_cahierFI_graph = Sys.getenv("HOME")
file_name = paste0("graph_ECB_bs_breakdown_en", ".pdf" )
file_graph = file.path(link_cahierFI_graph, file_name)

start_time = as.Date("2010-01-01")

# source("M:/Usuels.dsc/pRev/FI/dataInsight_query_excel_list.R")

# list_var_DI = c("EAECBILM0014.W", "EAECBILM0007.W", "EAILM024.W", "EAECBBSI3121.M")
# 
# list_var_pRev = c("securities","ltrp", "total_asset_ecb","excess_liqd")
# 
# list_label = c("Securities", "OBligations","Total Assets ECB","Excès de liquidités")

# getDataDI("EAECBILM0014.W", "EAECBILM0007.W", "EAILM024.W")
# getDataDI("EAECBBSI3121.M")

# EAECBBSI3121.M > ECB series key =  BSI.M.U2.N.R.LRE.X.1.A1.3000.Z01.E
# Eurozone Money and Banking Credit Institutions, Reserve Maintenance, 
# Excess Reserves, Maturity Not Applicable, World (All Entities)

# EAILM024.W > ?
# Eurozone Money and Banking Eurosystem, Total Assets/Liabilities Units: Millions of Euro,

# EAECBILM0007.W > ECB series key = ILM.W.U2.C.A052.U2.EUR
# Eurozone Money and Banking Eurosystem, Longer-Term Refinancing Operations Units: Millions of Euro

# EAECBILM0014.W > ECB series key = ILM.W.U2.C.A071.U2.EUR
# Eurozone Money and Banking Eurosystem, Securities Held for Monetary Policy Purposes Units: Millions of Euro


# RA6.M.N.U2.W1.S121.S1.LE.A.FA.R.F11._Z.EUR.XAU.M.N 

path_ECB_FM = "https://sdw-wsrest.ecb.europa.eu/service/data/ILM/"
myUrl <- paste0(path_ECB_FM,"W.U2.C..U2.EUR")
data <- readSDMX(myUrl)
data <- as.data.frame(data)

path_ECB_FM = "https://sdw-wsrest.ecb.europa.eu/service/data/RA6/"
myUrl <- paste0(path_ECB_FM,"M.N.U2.W1.S121.S1.LE.A.FA.R.F11._Z.EUR.XAU.M.N ")
gold <- readSDMX(myUrl)
gold <- as.data.frame(gold)
gold = gold %>%
  mutate(
    fake_date = as.Date(paste0(obsTime, "-01"), "%Y-%m-%d"),
    year_month = paste0(year(fake_date),month(fake_date))) 

data_ = data %>%
  dplyr::filter(stringr::str_detect(BS_ITEM,"A")) %>%
  dplyr::filter(!stringr::str_detect(BS_ITEM,"A070000")) %>%
  dplyr::filter(!stringr::str_detect(BS_ITEM,"A050000")) %>%
  mutate(date = as.Date(str_c(str_replace(obsTime,"W",""), "-1"),
                        format="%Y-%U-%u")) %>%
  mutate(year_ = year(date)) %>%
  mutate(year_month = paste0(year(date),month(date))) %>%
  filter(year_ >= 2008) %>%
  mutate(TITLE = gsub(" - Eurosystem| denominated in euro","",TITLE)) %>%
  drop_na(TITLE)


gold_ = gold %>%
  rename(gold = obsValue) %>%
  select(year_month, gold )

data2_ = data_ %>%
  left_join(gold_) %>%
  select(-obsValue) %>%
  rename(obsValue = gold) %>%
  mutate(TITLE = "Gold", BS_ITEM = "Gold") %>%
  distinct() 

last_gold = data2_ %>%
  drop_na(obsValue) %>%
  filter(date == max(date)) %>%
  distinct() %>%
  pull(obsValue)
  

data2_ = data2_ %>%
  mutate(obsValue = case_when(is.na(obsValue) & TITLE == "Gold" ~ last_gold, TRUE ~ obsValue))

data_ = data_ %>%
  select(BS_ITEM, TITLE, obsValue, date)

data2_ = data2_ %>%
  select(BS_ITEM, TITLE, obsValue, date)

# data_ = data_2
# data_ = rbind(data_, data2_)
# 
# data_ = data_ %>%
#   select(BS_ITEM, TITLE, obsValue, date)

# category = data_ %>%
#   dplyr::group_by(BS_ITEM, TITLE) %>%
#   summarise(c = count())

caption_order = data_ %>%
  filter(date == max(data_$date)) %>%
  arrange(desc(obsValue)) %>%
  pull(TITLE)


data_ = as.data.frame(data_)
data_[,"TITLE"] = factor(data_[,"TITLE"], levels = caption_order)
data_[,"obsValue"] = data_[,"obsValue"] / 1000

mycolors = c(brewer.pal(8, "Set1"), brewer.pal(8, "Set2"),brewer.pal(8, "Set3"))

min_scale = data_ %>%
  group_by(date) %>%
  summarize(bs = sum(obsValue)) %>%
  pull(bs) %>%
  min()

max_scale = data_ %>%
  group_by(date) %>%
  summarize(bs = sum(obsValue)) %>%
  pull(bs) %>%
  max()
  
subtitle_month = gsub("\\\\.","", lubridate::month(max(data_$date), label = TRUE))
subtitle_day = gsub("\\\\.","", lubridate::day(max(data_$date)))
subtitle_year = lubridate::year(max(data_$date)) 
xaxis_breaks = seq.Date(from = min(data_$date), to = max(data_$date), by = "3 months")
graph_subtitle = sprintf("Dernier point : %s %s %s, source : ECB", subtitle_day, subtitle_month, subtitle_year)


yaxis_breaks = seq(from = floor(min(min_scale, na.rm = TRUE) / 1000) * 1000,
                   to = ceiling(max(max_scale, na.rm = TRUE)/ 1000) * 1000, by = 1000)

# data_ =  data_ %>%
#   mutate(TITLE = case_when(BS_ITEM == "Gold" ~ "Gold", TRUE ~ TITLE))

graph_ecb_bs = 
ggplot() +
  geom_area(data = data_, aes(x = date, y = obsValue, fill = TITLE)) +
  ggtitle("Total des actifs liquides de la BCE - Eurosystem") +
  scale_fill_manual(values = mycolors) +
  scale_y_continuous(sec.axis = dup_axis(), breaks = yaxis_breaks) +
  scale_x_date(breaks = xaxis_breaks, date_labels = "%b %y" ) +
  labs(y = "en millard d'euros") +  
  theme_stata() +
  theme(
    plot.title   = element_text(lineheight = 0.8, face = "bold", hjust = 0.5, size = 18),
    axis.text.x  = element_text(angle = 45, hjust = 1),
    axis.text.y  = element_text(angle = 0, hjust = 1),
    axis.title.x = element_blank(),
    # axis.title.y = element_blank(),
    legend.title = element_blank(),
    legend.position = "bottom"
  ) 

graph_ecb_bs %>%  ggsave(filename = file_graph, width = 17, height = 9)
