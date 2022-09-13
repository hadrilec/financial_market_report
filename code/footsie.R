
index_code = 'FSTE'
index_code2 = '^FTSE'
index_label = 'FOOTSIE'
currency = 'livres'
company_number_plotted = 40

url_ = 'https://en.wikipedia.org/wiki/FTSE_100_Index'

table = read_html(url_) %>% html_table(fill=TRUE)
table = table[[4]]

names(table) = tolower(names(table))
table = table[,c(2,1,3)]
names(table) = c('ticker', 'company', 'sector')
table[,1] = gsub('\\.$','',table[,1])
table[,1] = paste0(table[,1], '.L')
table[nrow(table)+1,] = c(index_code2,index_label, '')

Symbols = c(table[,1])

table[str_detect(table[,'sector'], 'Insurance'),'sector'] = 'Insurance'
table[str_detect(table[,'sector'], 'Asset Managers'),'sector'] = 'Financial Services'
table[str_detect(table[,'sector'], 'Mining'),'sector'] = 'Mining'
table[str_detect(table[,'company'], 'Associated Brit|Diageo|Coca-'),'sector'] = 'Food & Beverage Producer'
