
library(pRev)

D111NCOPWT.D = getDataDI("D111NCOPWT.D")
D163NCOPBR.D = getDataDI("D163NCOPBR.D")

# D111NCOPWT.D
# UNITED STATES CRUDE OIL PRICES:WTI    
# SPOT MARKET WEST TEXAS INTERMEDIATE CUSH  (1700-1800) CRUDE GRADES  USD PER BARREL SOURCE: DAILY PRESS, IHS ECONOMICS,
# INTERNATIONAL MONETARY FUND (IMF)  PRIOR TO 2006 MARCH 31),  COMMODITY RESEARCH BUREAU (CRB) ( FROM 2006 APRIL 3 TO PRESENT)

# D163NCOPBR.D
# EUROZONE (UK - NORWAY) CRUDE OIL PRICES     (DAILY) SPOT MARKET BRENT BLEND  (NORTH SEA) CRUDE GRADES USD PER BARREL,
# FOB SOURCE: DAILY PRESS, # IHS ECONOMICS,IMF (PROR TO 2005M8), COMMODITY RESEARCH BUREAU (FROM 2005M9 ONWARD)

oil_price_us = D111NCOPWT.D %>% mutate(label = "oil wti texas")

oil_price_eu = D163NCOPBR.D %>% mutate(label = "oil brent")

oil = oil_price_eu %>% 
  bind_rows(oil_price_us) %>% 
  filter(time >= "2018-01-01") %>% 
  drop_na() %>% 
  arrange(time) %>% 
  as.data.frame()

# brent_add = 35
# texas_add = 33
# oil[nrow(oil)+1, ] = c(as.Date("2020-03-09"), brent_9mars, "oil price - Brent")
# oil[nrow(oil)+1, ] = c(as.Date("2020-03-09"), texas_9mars, "oil price - Texas")
# oil[nrow(oil)+1, ] = c("2020-03-10", brent_add, "oil price - Brent")
# oil[nrow(oil)+1, ] = c("2020-03-10", texas_add, "oil price - Texas")

last_values = oil %>% 
  group_by(label) %>% 
  filter(time == max(time))

subtt = sprintf("Dernier point : %s \n%s : %s \n%s : %s", max(last_values$time),
                last_values[1,"label"], last_values[1,"value"],
                last_values[2,"label"], last_values[2,"value"])

oil = oil %>% 
  mutate(value = as.numeric(value))

yaxis_breaks = seq(from = floor(min(as.numeric(oil$value))/5)*5, to = ceiling(max(as.numeric(oil$value))/5)*5, by = 5)
xaxis_breaks = seq.Date(from = min(oil$time), to = max(oil$time), by = "1 month")

ggplot(oil, aes(x = time, y = value, colour = label)) +
  geom_hline(yintercept = 65, linetype = "dashed") +
  geom_line(size = 1) +
  labs(subtitle = subtt) +
  ggthemes::theme_stata() +
  scale_x_date(date_labels = "%b %y", breaks = xaxis_breaks) +
  scale_y_continuous(sec.axis = dup_axis(),
                     breaks = yaxis_breaks,
                     limits = c(0,max(yaxis_breaks))) +
  ggtitle("Cours du pétrole en dollar par baril") +
  theme(
    plot.title   = element_text(lineheight = 0.8, face = "bold", hjust = 0.5, size = 18),
    axis.text.x  = element_text(angle = 45, hjust = 1),
    axis.text.y  = element_text(angle = 0, hjust = 1),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.title = element_blank(),
    legend.position = "bottom"
  ) 
  
