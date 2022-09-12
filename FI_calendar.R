
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
library(tidyr)
library(RColorBrewer)
library(pRev)


# library(timevis)
# library(timelineS)

link_cahierFI_graph = "M:/Usuels.dsc/pRev/FI/cahier_FI/graph"
link_file = file.path(link_cahierFI_graph, 'FI_calendar.pdf')

# Federal Open Market Committee FOMC Meetings
FED_dates = as.Date(c('2020-01-29', '2020-03-18', '2020-04-29', '2020-06-10','2020-07-29' ,'2020-09-16','2020-11-05','2020-12-16'))
FED = data.frame(event = rep('FED', length(FED_dates)), date = FED_dates, stringsAsFactors = FALSE, y = 1.5)

# ECB monetary policy meeting
ECB_dates  = as.Date(c('2020-01-23', '2020-03-12', '2020-04-30', '2020-06-04','2020-07-16' ,'2020-09-10','2020-10-29','2020-12-10'))
ECB = data.frame(event = rep('ECB', length(ECB_dates)), date = ECB_dates, stringsAsFactors = FALSE, y = 1)

#Bank of England Monetary Policy Committee meeting
BOE_dates  = as.Date(c('2020-01-30', '2020-03-26', '2020-05-07', '2020-06-18','2020-08-06' ,'2020-09-17','2020-11-05','2020-12-17'))
BOE = data.frame(event = rep('BOE', length(BOE_dates)), date = BOE_dates, stringsAsFactors = FALSE, y = -1)

# Bank of Japan Moneraty Policy Meeting
BOJ_dates  = as.Date(c('2020-01-20', '2020-03-18', '2020-05-27', '2020-06-15','2020-07-14' ,'2020-09-16','2020-10-28','2020-12-17'))
BOJ = data.frame(event = rep('BOJ', length(BOJ_dates)), date = BOJ_dates, stringsAsFactors = FALSE, y = -1.5)

FI_calendar = bind_rows(FED, ECB, BOE, BOJ)

FI_calendar$event = factor(FI_calendar$event , levels = c('FED', "ECB", "BOE", "BOJ"))
# timevis(FI_calendar)
# timelineS(FI_calendar)

FI_calendar[, "date_label"] = paste(day(FI_calendar[, "date"]),
                              as.character(month(FI_calendar[, "date"], label = TRUE, abbr = FALSE)))
FI_calendar[, "event2"] = paste0(FI_calendar[, "event"], " \n", FI_calendar[, "date_label"])

months_ = seq.Date(from = as.Date('2020-01-01'), to = as.Date("2020-12-01"), by = "1 month")
months_label = data.frame( 
  date = months_,
  label = paste(1, gsub("\\.|\\\\",'',as.character(month(months_, label = TRUE, abbr = TRUE)))))

colors_ = c(brewer.pal(4,'Set1'))

gg_calendar =
ggplot() +
  ggtitle("Calendrier 2020 des décisions de politique monétaire des banques centrales") + 
  geom_hline(yintercept = 0, color = "black", size = 0.3) + 
  theme_classic() + 
  geom_text(data = months_label, aes(y = 0.1, x = date, label = label), angle = 90, size = 4) +
  geom_point(data = months_label, aes(y = 0, x = date), shape = 3) +
  geom_segment(data = FI_calendar, aes(y = y, yend = 0, xend = date, x = date), color = 'black', size = 0.2) + 
  geom_point(data = FI_calendar, aes(y = 0, x = date, colour = event), size = 3, show.legend = FALSE) +
  geom_point(data = FI_calendar, aes(y = y, x = date, colour = event), size = 1, show.legend = FALSE) +
  geom_text(data = FI_calendar, aes(y = y, x = date, colour = event, label = event2),
            size = 5, hjust = 1.1, show.legend = FALSE) +
  scale_colour_manual(values = colors_) +
  theme(axis.line.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank(),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5) 
  ) + 
  ggsave(filename = link_file, width = 15, height = 10)


# pRev::export_graph(gg_calendar, folder_name = "calendar_monetay_policy", perim = "FI")
