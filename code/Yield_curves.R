
library(rdbnomics)
library(rsdmx)
library(ggthemes)
library(grid)
library(lubridate)
library(eurostat)
library(fredr)
library(dplyr)
library(ggplot2)
library(Quandl)
library(zoo)

number_month_backward = 6

link_cahierFI_graph = Sys.getenv("HOME")
file_name = paste0("graph_yield_curves", ".pdf" )
file_graph = file.path(link_cahierFI_graph, file_name)

Quandl.api_key('ZQv7EfJg6xnzC-by17Kn')
us_bond = Quandl("USTREASURY/YIELD")

path_ECB_FM = "https://sdw-wsrest.ecb.europa.eu/service/data/YC/"
myUrl <- paste0(path_ECB_FM,"B.U2.EUR.4F.G_N_C+G_N_A.SV_C_YM.BETA0+BETA1+BETA2+BETA3+TAU1+TAU2?lastNObservations=600")
data <- readSDMX(myUrl)
data <- as.data.frame(data)

dates = intersect(as.character(as.Date(us_bond$Date)), as.character(as.Date(data$obsTime)))

max_date = max(as.Date(dates))
min_date = max_date %m-% months(number_month_backward)
while(!min_date %in% as.Date(dates)){
  min_date = min_date %m-% days(1)
}

data = data %>%
  mutate(date = as.Date(obsTime),
         INSTRUMENT_FM = as.character(INSTRUMENT_FM),
         DATA_TYPE_FM = as.character(DATA_TYPE_FM)) %>%
  filter(date %in% c(min_date, max_date))

maturity = c(0.25, seq(from = 0.5, to = 30, by = 0.5)) 
ycf = function(x,b0,b1,b2,b3,t1,t2){b0+b1*t1*(1-exp(-x/t1))/x+b2*(t1*(1-exp(-x/t1))/x-exp(-x/t1))+b3*(t2*(1-exp(-x/t2))/x-exp(-x/t2))}
df_maturity = data.frame(maturity = maturity)

list_df = list()

for(i in unique(data[,"date"])){
  for(j in unique(data[,"INSTRUMENT_FM"])){
    date_ = 
    b0 = data %>% filter(date == i, INSTRUMENT_FM == j, DATA_TYPE_FM == "BETA0") %>% pull(obsValue)
    b1 = data %>% filter(date == i, INSTRUMENT_FM == j, DATA_TYPE_FM == "BETA1") %>% pull(obsValue)
    b2 = data %>% filter(date == i, INSTRUMENT_FM == j, DATA_TYPE_FM == "BETA2") %>% pull(obsValue)
    b3 = data %>% filter(date == i, INSTRUMENT_FM == j, DATA_TYPE_FM == "BETA3") %>% pull(obsValue)
    t1 = data %>% filter(date == i, INSTRUMENT_FM == j, DATA_TYPE_FM == "TAU1") %>% pull(obsValue)
    t2 = data %>% filter(date == i, INSTRUMENT_FM == j, DATA_TYPE_FM == "TAU2") %>% pull(obsValue)
    if(j == "G_N_C"){
      p = "Zone Euro - Tous les pays"
    }else{
      p = "Zone Euro - Pays AAA" 
    }
    df = data.frame(x = maturity, value = ycf(maturity,b0,b1,b2,b3,t1,t2), label = as.Date(i),  perim = p)
    list_df[[length(list_df)+1]] = df
  }
}
data_f = dplyr::bind_rows(list_df)

data_f$label2 = format(data_f$label,"%d %b %Y")

data_f = data_f %>%
  arrange(desc(label))


us_bond_ = us_bond %>%
  filter(Date %in% c(min_date, max_date))

list_df = list()
for(i in 1:nrow(us_bond_)){
  df = data.frame(maturity = c(0.25, 0.5, 1,2,3,5,7,10,20,30),
                  value = as.numeric(us_bond_[i,4:ncol(us_bond_)]))
  df = merge(df, df_maturity, by = "maturity", all.x = T, all.y = T)
  names(df)[1] = "x"
  df[,"value"] = as.numeric(na.approx(zoo(df[,"value"])))
  df[,"label"] = us_bond_[i,"Date"]
  df[,"perim"] = "Etats-Unis"
  df[,"label2"] = format(df[,"label"],"%d %b %Y")

  list_df[[length(list_df)+1]] = df
}
us_bond_f = bind_rows(list_df)

data_f = rbind(data_f, us_bond_f)

data_time = data_f$x

subtitle_day = gsub("\\\\.","", lubridate::day(max(data_f$label)))

subtitle_month = gsub("\\\\.","", lubridate::month(max(data_f$label), label = TRUE))

subtitle_year = lubridate::year(max(data_f$label)) 

xaxis_breaks = seq(from = floor(min(data_time)), to = ceiling(max(data_time)), by = 2)
yaxis_breaks = seq(from = floor(min(data_f$value)/0.5)*0.5, to = ceiling(max(data_f$value)/0.5)*0.5, by = 0.5)

graph_subtitle = sprintf("Dernier point : %s %s %s, source : BCE, US Treasury", subtitle_day, subtitle_month, subtitle_year)

graph_yc = ggplot(data = data_f, aes(x = x, y = value, colour = perim)) +
  facet_wrap(~label2) +
  geom_line(size = 1) +
  ggtitle("Courbe des taux") +
  labs(subtitle = graph_subtitle, x= "Maturité en années") + 
  scale_y_continuous( sec.axis = dup_axis(),
                      breaks = yaxis_breaks,
                      labels = function(x) paste0(x, "%")) +
  scale_x_continuous(breaks = xaxis_breaks) +
  theme_stata() +
  theme(
    plot.title   = element_text(lineheight = 0.8, face = "bold", hjust = 0.5, size = 18),
    axis.text.x  = element_text(angle = 0),
    axis.text.y  = element_text(angle = 0, hjust = 1),
    # axis.title.x = element_blank(),
    text = element_text(size = 15),
    axis.title.y = element_blank(),
    legend.title = element_blank(),
    legend.position = "bottom"
  ) 

graph_yc %>% ggsave(filename = file_graph, width = 12, height = 7)


