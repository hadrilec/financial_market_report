
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

date = gsub("-","",Sys.Date())
link_cahierFI_graph = Sys.getenv("HOME")
start_time = as.Date("2018-12-01")
file_name = paste0("graph_US_interbank_rate", ".pdf" )
file_graph = file.path(link_cahierFI_graph, file_name)

fredr_set_key("1e1376b050a44076281adda2fe2e1a32")
# 
# us_ir_uptarget = fredr(series_id = "DFEDTARU",
#                          observation_start = start_time)
# 
# us_ir_lowtarget = fredr(series_id = "DFEDTARL",
#                     observation_start = start_time)
# 
# us_ir_effective = fredr(series_id = "DFF",
#                         observation_start = start_time)

# Interest Rate on Excess Reserves (IOER)
us_ioer = fredr(series_id = "IOER", observation_start = start_time)

# Overnight Bank Funding Rate (OBFR)
us_obfr = fredr(series_id = "OBFR", observation_start = start_time)

# Secured Overnight Financing Rate (SOFR)
us_sofr = fredr(series_id = "SOFR", observation_start = start_time)

# Assets: Other: Repurchase Agreements: Wednesday Level (WORAL)
fed_repagr = fredr(series_id = "WORAL", observation_start = start_time)
fed_repagr$value = fed_repagr$value / 1000

us_treasury_fed_buy = fredr(series_id = "RPONTSYD", observation_start = start_time)
# us_treasury_fed_buy$value = us_treasury_fed_buy$value / 1000

# df_us_ir = rbind(us_ir_uptarget, us_ir_lowtarget, us_ir_effective, us_ioer, us_sofr)
df_us_ir = rbind(us_sofr, us_obfr, fed_repagr
                 # , us_treasury_fed_buy
                 )

df_us_ir = df_us_ir %>%
  mutate(name = case_when(series_id == "DFEDTARU" ~ "Taux directeur - borne supérieure",
                          series_id == "DFEDTARL" ~ "Taux directeur - borne inférieure",
                          series_id == "WORAL" ~ "Volume d'actifs achetés sur le marché interbancaire collatéralisé détenu par la FED",
                          series_id == "RPONTSYD" ~ "Achats au jour le jour de bons du Trésor par la Fed sur le marché collatéralisé interbancaire",
                          series_id == "DFF" ~ "Taux effectif",
                          series_id == "OBFR" ~ "Taux au jour-le-jour sur le marché interbancaire",
                          series_id == "SOFR" ~ "Taux au jour-le-jour sur le marché interbancaire collatéralisé",
                          series_id == "IOER" ~ "Taux d'intérêt sur les réservers excessives"
                          ),
         type = case_when(series_id %in% c("WORAL", "RPONTSYD") ~ "Volume d'actifs (milliards de dollars)",
                          series_id == "OBFR" ~ "Taux sur le marché interbancaire (%)",
                          series_id == "SOFR" ~ "Taux sur le marché interbancaire (%)"))


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

# graph_subtitle_ = sprintf("Taux directeur - borne supérieure: %s%%",last_irup)

graph_subtitle = sprintf("Dernier point : %s %s %s, source : FED",
                         subtitle_day, subtitle_month, subtitle_year)

yaxis_breaks = seq(from = floor(min(df$value, na.rm = TRUE) / 0.5) * 0.5,
                   to = ceiling(max(df$value, na.rm = TRUE)/ 0.5) * 0.5, by = 0.5)
# scale_y_continuous(sec.axis = dup_axis(), breaks = yaxis_breaks, labels = function(x) paste0(x, "%")) +

caption = "The Secured Overnight Financing Rate (SOFR) is a broad measure of the cost of borrowing cash overnight collateralized by Treasury securities
Repurchase agreements reflect some of the Federal Reserve's temporary open market operations. Repurchase agreements are transactions
in which securities are purchased from a primary dealer under an agreement to sell them back to the dealer on a specified date in the future"

df = df %>% drop_na()

graph_us_ir = ggplot(data = df,
               aes(x = date, y = value, colour = name)) +
  geom_line(size = 1) +
  facet_wrap(~type, scales = "free", ncol = 2) +
  ggtitle("Marché interbancaire américain") + 
  labs(subtitle = graph_subtitle, caption = caption) + 
  # scale_y_continuous(sec.axis = dup_axis(), breaks = yaxis_breaks,
  #                    labels = function(x) paste0(x, "%")) +
  scale_x_date(breaks = xaxis_breaks, date_labels = "%b %y" ) +
  theme_stata() +
  guides(colour = guide_legend(nrow = 2, byrow = TRUE)) +
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

graph_us_ir %>% ggsave(filename = file_graph, width = 20, height = 11)

