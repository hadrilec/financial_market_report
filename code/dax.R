
index_code = 'GDAXI'
index_code2 = '^GDAXI'
index_label = 'DAX'
currency = 'euros'
company_number_plotted = 30

url_ = 'https://en.wikipedia.org/wiki/DAX'

table = read_html(url_) %>% html_table(fill=TRUE)
table = table[[4]]

names(table) = tolower(names(table))
table = table[,c(4,2,3)]
names(table) = c('ticker', 'company', 'sector')
table[,1] = paste0(table[,1], '.DE')
table[nrow(table)+1,] = c(index_code2,index_label,'')

if(any(grepl('CON.DE',table[,1]))){
  table = table[-grep('CON.DE', table[,1]),]
}

Symbols = c(table[,1])

table[str_detect(table[,'company'], 'Bayer|Merck|Covestro|BASF|Linde'),'sector'] = 'Pharmaceuticals & chemicals'
table[str_detect(table[,'company'], 'Henkel|Merck|Beiersdorf'),'sector'] = 'Consumer goods'
table[str_detect(table[,'company'], 'BMW|Volkswagen|Daimler'),'sector'] = 'Automobile'



