
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

date = gsub("-", "", Sys.Date())
link_cahierFI_graph = Sys.getenv("HOME")
link_cahierFI_excel = Sys.getenv("HOME")
start_time = as.Date("2014-01-01")
file_name = paste0("graph_FED_interest_rates", ".pdf" )
file_graph = file.path(link_cahierFI_graph, file_name)
file_excel = file.path(link_cahierFI_excel, paste0(gsub(".pdf|graph_","",file_name),".csv"))

fredr_set_key("1e1376b050a44076281adda2fe2e1a32")

# Federal Funds Target Range - Upper Limit (DFEDTARU)
us_ir_uptarget = fredr(series_id = "DFEDTARU",
                         observation_start = start_time)

us_ir_lowtarget = fredr(series_id = "DFEDTARL",
                    observation_start = start_time)

us_ir_effective = fredr(series_id = "DFF",
                        observation_start = start_time)

# Interest Rate on Excess Reserves (IOER)
us_ioer = fredr(series_id = "IOER", observation_start = start_time)

# Secured Overnight Financing Rate (SOFR)
us_sofr = fredr(series_id = "SOFR", observation_start = start_time)

# df_us_ir = rbind(us_ir_uptarget, us_ir_lowtarget, us_ir_effective, us_ioer, us_sofr)
df_us_ir = rbind(us_ir_uptarget, us_ir_lowtarget, us_ir_effective)

df_us_ir = df_us_ir %>%
  mutate(name = case_when(series_id == "DFEDTARU" ~ "Taux directeur - borne supérieure",
                          series_id == "DFEDTARL" ~ "Taux directeur - borne inférieure",
                          series_id == "DFF" ~ "Taux effectif",
                          series_id == "SOFR" ~ "Taux interbancaire au jour-le-jour sur le marché interbancaire collatéralisé",
                          series_id == "IOER" ~ "Taux d'intérêt sur les réservers excessives"
                          ))


last_irup = us_ir_uptarget %>%
  arrange(desc(date)) %>%
  slice(1) %>%
  mutate(month = month(date, label = T, abbr = T)) %>%
  mutate(month = gsub("\\\\.","", month)) %>%
  mutate(value = round(value,2)) %>%
  pull(value)

df = df_us_ir

subtitle_day = lubridate::day(max(df$date))
subtitle_month = gsub("\\\\.","",
                      lubridate::month(max(df$date), label = TRUE))
subtitle_year = lubridate::year(max(df$date))  
xaxis_breaks = seq.Date(from = min(df$date),
                        to = max(df$date), by = "3 months")

graph_subtitle_ = sprintf("Taux directeur - borne supérieure: %s%%",last_irup)

graph_subtitle = sprintf("Dernier point : %s %s %s, source : FED \n %s",
                         subtitle_day, subtitle_month, subtitle_year, graph_subtitle_)

yaxis_breaks = seq(from = floor(min(df$value, na.rm = TRUE) / 0.5) * 0.5,
                   to = ceiling(max(df$value, na.rm = TRUE)/ 0.5) * 0.5, by = 0.5)
# scale_y_continuous(sec.axis = dup_axis(), breaks = yaxis_breaks, labels = function(x) paste0(x, "%")) +

caption = "The federal funds rate is the interest rate at which depository institutions trade federal funds (balances held at Federal Reserve Banks) with each other overnight.\nWhen a depository institution has surplus balances in its reserve account, it lends to other banks in need of larger balances. In simpler terms, a bank with excess cash,\nwhich is often referred to as liquidity, will lend to another bank that needs to quickly raise liquidity. The rate that the borrowing institution pays to the lending institution \nis determined between the two banks; the weighted average rate for all of these types of negotiations is called the effective federal funds rate.\nThe effective federal funds rate is essentially determined by the market but is influenced by the Federal Reserve through open market operations to reach the federal funds rate target."

graph_us_ir = ggplot(data = df,
               aes(x = date, y = value, colour = name
                   # , linetype = name
               )) +
  geom_line(size = 1) +
  ggtitle("Politique monétaire américaine ") +
  labs(subtitle = graph_subtitle, caption = caption) + 
  scale_y_continuous(sec.axis = dup_axis(), breaks = yaxis_breaks, labels = function(x) paste0(x, "%")) +
  scale_x_date(breaks = xaxis_breaks, date_labels = "%b %y" ) +
  theme_stata() +
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

graph_us_ir %>%  ggsave(filename = file_graph, width = 12, height = 7)



# 
#  export to csv
# 
link_cahierFI_excel = Sys.getenv("HOME")
file_excel = file.path(link_cahierFI_excel, paste0(gsub(".pdf|graph_","",file_name),".csv"))

df = as.data.frame(df)
var_name = "name"
value_name = "value"
date_name = "date"

list_var = unique(df[,var_name])
df_ = data.frame(unique(df[,date_name]))
names(df_)[1] = date_name

for(var in list_var){
  df2 = df[which(df[,var_name] == var), c(date_name, value_name)]
  df2 = as.data.frame(df2)
  names(df2)[2] = var
  df_ = merge(df_, df2, by = date_name, all.x = T, all.y = T)
}

write.csv(df_, file = file_excel, row.names = FALSE) 
