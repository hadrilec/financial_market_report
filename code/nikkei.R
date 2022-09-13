library(rtsdata)
library(xml2)
library(rvest)
library(lubridate)
library(dplyr)
library(ggplot2)
library(tidyr)
library(RColorBrewer)
library(stringr)


index_code = 'N225'
index_code2 = '^N225'
index_label = 'NIKKEI'
currency = 'yen'
company_number_plotted = 40

url_ = 'https://en.wikipedia.org/wiki/Nikkei_225'

# table = read_html(url_) %>%
#   html_nodes('h3')%>% 
#   html_text()
# 
# table = table[str_detect(table, 'edit')]
# table = gsub('\\[edit\\]','',table)

table_ = read_html(url_) %>%
  html_nodes('div') %>% 
  html_text()

table_ = paste(table_[15],table_[16])
test = str_split(table_, 'edit')

list_df = list()
# i = 37
for(i in 1:(length(test[[1]]))){
  
  parenth_loc = str_locate_all(test[[1]][i], '\\)')[[1]]
  last_parenth_loc = parenth_loc[nrow(parenth_loc)]
  
  if(i==1){
    parenth_loc = str_locate_all(test[[1]][i], '\n')[[1]]
    last_parenth_loc = parenth_loc[1]
  }
  
  parenth2_loc = str_locate_all(test[[1]][i], '\\[')[[1]]
  last_parenth2_loc = parenth2_loc[nrow(parenth2_loc)]
  
  list_df[[i]] =''
  
  if(i!=1){
    list_df[[i-1]] = str_count(test[[1]][i], 'TYO')
  }
  
  if(length(last_parenth_loc)>0 & length(last_parenth2_loc)>0 & i != length(test[[1]])){
  
    sect = substr(test[[1]][i],last_parenth_loc+1, last_parenth2_loc-1)
    sect = gsub('\n','',sect)
    # print(sect)
    names(list_df)[i] = sect
    rm(sect)
  }
  
}

# table_ = table_[[15]]

table = read_html(url_) %>%
  html_nodes('ul > li > a') %>% 
  html_text()

table = data.frame(name = table[which(str_detect(table,'TYO'))-1],
                code = table[which(str_detect(table,'TYO'))+1],
                stringsAsFactors = FALSE)
rows = 0
for(i in 1:length(list_df)){
  print(i)
  if(!is.na(as.numeric(list_df[[i]]))){
    rows = (rows+1):(rows+list_df[[i]])
    table[rows, 'sector'] = names(list_df)[i]
    rows = rows[length(rows)]
  }
}

# table = table[,c(2,1,3)]
# names(table) = c('ticker', 'company', 'sector')
table = table[,c(2,1)]
names(table) = c('ticker', 'company')

table[,'ticker'] = paste0(table[,'ticker'], '.T')

table[nrow(table)+1,] = c(index_code2, index_label) #, ''

Symbols = c(table[,1])
