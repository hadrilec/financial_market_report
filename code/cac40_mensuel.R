
library(rtsdata)
library(xml2)
library(rvest)
library(lubridate)
library(dplyr)
library(tidyr)
library(stringr)
library(openxlsx)

env_ = new.env()
start_date = as.Date("2000-01-01")
end_date = Sys.Date()

getSymbols('^FCHI', env_, src = 'yahoo', from = start_date, to = end_date, verbose = TRUE)

data = data.frame(time = time(env_[['^FCHI']][,1]),
                  Open = as.numeric(env_[['^FCHI']][,1]),
                  High = as.numeric(env_[['^FCHI']][,2]),
                  Low  = as.numeric(env_[['^FCHI']][,3]),
                  Close = as.numeric(env_[['^FCHI']][,4]),
                  Volume = as.numeric(env_[['^FCHI']][,5]),
                  Adjusted = as.numeric(env_[['^FCHI']][,6]),
                  ticker = '^FCHI')

data_m = data %>% 
  mutate(cac40 = Adjusted) %>% 
  mutate(month_ = paste(year(time), month(time))) %>% 
  group_by(month_) %>% 
  summarise(cac40_m = mean(cac40, na.rm = TRUE))

openxlsx::write.xlsx(data_m, file = file.path(Sys.getenv("USERPROFILE"),"Desktop", "cac40.xlsx"))




