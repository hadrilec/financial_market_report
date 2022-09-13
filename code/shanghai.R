# 
# index_code = '000016.SS' #SSE 50
index_code = "000001.SS" #SSE index
index_code2 = "000001.SS"
# index_code2 = '000016.SS'
index_label = 'SHANGHAI'
currency = 'yuan'
company_number_plotted = 40

url_ = 'https://en.wikipedia.org/wiki/SSE_50_Index'

table = read_html(url_) %>% html_table(fill = TRUE)
table = table[[3]]
table = table[,c(3,1,2)]
names(table) = c('ticker', 'company', 'sector')
table[,1] = gsub("SSE: ","",paste0(table[,1], '.SS'))
table[nrow(table)+1,] = c(index_code2, index_label,'')

table[str_detect(table[,'company'], 'Tsinghua'),'sector'] = 'Software'

Symbols = c(table[,1])
