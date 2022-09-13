library(rtsdata)
library(xml2)
library(rvest)
library(lubridate)
library(dplyr)
library(ggplot2)
library(tidyr)
library(RColorBrewer)
library(stringr)


index_code = 'GSPC'
index_code2 = '^GSPC'
index_label = 'SP500'
currency = 'dollars'
company_number_plotted = 40

url_ = 'https://en.wikipedia.org/wiki/List_of_S%26P_500_companies'

table = read_html(url_) %>% html_table(fill=TRUE)
table = table[[2]]

names(table) = tolower(names(table))
table = table[,c(1,2,4)]
names(table) = c('ticker', 'company','sector')
table[,1] = str_replace(table[,1], 'BRK.B|BRKB', 'BRK-B')

if(any(grepl('GOOGL',table[,1]))){
  table = table[-grep('GOOGL', table[,1]),]
}

table[nrow(table)+1,] = c(index_code2,index_label,'')
Symbols = c(table[,1])
