
library(rdbnomics)
library(rsdmx)
library(ggthemes)
library(grid)
library(lubridate)
library(eurostat)
library(fredr)
library(dplyr)
library(ggplot2)

link_cahierFI_graph = Sys.getenv("HOME")
file_name = paste0("graph_ECB_interest_rates", ".pdf" )
file_graph = file.path(link_cahierFI_graph, file_name)

t_min = as.Date("2011-01-01", "%Y-%m-%d")

path_ECB_FM = "https://sdw-wsrest.ecb.europa.eu/service/data/FM/"
myUrl <- paste0(path_ECB_FM,"D.U2.EUR.4F.KR.MLFR.LEV")
dataset <- readSDMX(myUrl)
MLFR <- as.data.frame(dataset)
MLFR[,"TITLE"] = "ECB Marginal lending facility"

myUrl <- paste0(path_ECB_FM,"D.U2.EUR.4F.KR.DFR.LEV")
dataset <- readSDMX(myUrl)
DFR <- as.data.frame(dataset)
DFR[,"TITLE"] = "ECB Deposit facility"

myUrl <- paste0(path_ECB_FM, "D.U2.EUR.4F.KR.MRR_FR.LEV")
dataset <- readSDMX(myUrl)
MRR_FR <- as.data.frame(dataset)

myUrl <- paste0(path_ECB_FM, "B.U2.EUR.4F.KR.MRR_MBR.LEV")
dataset <- readSDMX(myUrl)
MRR_MBR <- as.data.frame(dataset)

MRR_FR = rbind(MRR_FR, MRR_MBR)

MRR_FR[which(MRR_FR[,'TITLE'] != MRR_FR[1,'TITLE']),'obsValue'] = NA
MRR_FR[which(MRR_FR[,'TITLE'] != MRR_FR[1,'TITLE']),'TITLE'] = MRR_FR[1,'TITLE']

MRR_FR[,'TITLE'] = "ECB Main refinancing operations"
# euribor 3m - monthly 

myUrl <- paste0(path_ECB_FM, "M.U2.EUR.RT.MM.EURIBOR3MD_.HSTA")
dataset <- readSDMX(myUrl)
EURIBOR_3M <- as.data.frame(dataset)
EURIBOR_3M = EURIBOR_3M[,c('obsTime','obsValue','TITLE')]
EURIBOR_3M[,"obsTime"] = paste0(EURIBOR_3M[,"obsTime"],"-01")
EURIBOR_3M[,"TITLE"] = "Euribor 3-month"
# EURIBOR_3M[,"obsTime"] = as.Date(EURIBOR_3M[,"obsTime"],'%Y-%m-%d')

# EURIBOR_3M = EURIBOR_3M[which(EURIBOR_3M[,"obsTime"] >= t_min),]

# EONIA
path_ECB_EONIA = "https://sdw-wsrest.ecb.europa.eu/service/data/EON/"
myUrl <- paste0(path_ECB_EONIA, "D.EONIA_TO.RATE") #TO BE CHECKED
dataset <- readSDMX(myUrl)
EONIA <- as.data.frame(dataset)
EONIA = EONIA[,c('obsTime','obsValue','TITLE')]
EONIA[,"TITLE"] = "EONIA"

df = rbind(MLFR, DFR, MRR_MBR, MRR_FR)
df = df[,c('obsTime','obsValue','TITLE')]
df[,"obsValue"] = as.numeric(df[,"obsValue"])

df = rbind(df, EONIA, EURIBOR_3M)

df[,'obsTime'] = as.Date(df[,'obsTime'],'%Y-%m-%d')
df = df[which(df[,'obsTime'] >= t_min ),]
df = df[order(df[,'obsTime']),]

subtitle_day = lubridate::day(max(df$obsTime))
subtitle_month = gsub("\\\\.","", lubridate::month(max(df$obsTime), label = TRUE))
subtitle_year = lubridate::year(max(df$obsTime)) 
xaxis_breaks = seq.Date(from = min(df$obsTime), to = max(df$obsTime), by = "3 months")
graph_subtitle = sprintf("Dernier point : %s %s %s, source : ECB", subtitle_day, subtitle_month, subtitle_year)


yaxis_breaks = seq(from = floor(min(df$obsValue, na.rm = TRUE) / 0.5) * 0.5,
                   to = ceiling(max(df$obsValue, na.rm = TRUE)/ 0.5) * 0.5, by = 0.5)
# scale_y_continuous(sec.axis = dup_axis(), breaks = yaxis_breaks, labels = function(x) paste0(x, "%")) +

graph_ecb_ir = 
  ggplot(data = df, aes(x = obsTime, y = obsValue, colour = TITLE, linetype = TITLE)) +
  geom_line(size = 1) +
  ggtitle("Taux du marché monétaire européen") +
  labs(subtitle = graph_subtitle) + 
  scale_y_continuous(sec.axis = dup_axis(), breaks = yaxis_breaks, labels = function(x) paste0(x, "%")) +
  scale_x_date(breaks = xaxis_breaks, date_labels = "%b %y" ) +
  theme_stata() +
  theme(
    plot.title   = element_text(lineheight = 0.8, face = "bold", hjust = 0.5, size = 18),
    axis.text.x  = element_text(angle = 45, hjust = 1),
    axis.text.y  = element_text(angle = 0, hjust = 1),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.title = element_blank(),
    legend.position = "bottom"
    # panel.grid.major.y = element_line(colour="black", 
    #                                       size = 0.5,
    #                                       linetype = 1)
  ) 

data_q = df %>% 
  mutate(quarter_ = lubridate::quarter(obsTime, with_year = TRUE)) %>% 
  filter(str_detect(quarter_, "2019")) %>% 
  filter(str_detect(TITLE, "Euribor")) %>% 
  group_by(quarter_) %>% 
  summarise(value = mean(obsValue))


graph_ecb_ir %>%  ggsave(filename = file_graph, width = 12, height = 7)


