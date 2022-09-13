
index_code = 'FCHI'
index_code2 = '^FCHI'
index_label = 'CAC40'
currency = 'euros'
company_number_plotted = 40

url = 'https://en.wikipedia.org/wiki/CAC_40'

table = read_html(url) %>% html_table(fill = TRUE)
table = table[[4]]

names(table) = tolower(names(table))
table = table[,c(4,1,2)]
names(table) = c('ticker', 'company', 'sector')
table[,1] = gsub('\\.$','',table[,1])
table[nrow(table)+1,] = c(index_code2,index_label, '')

env_ = new.env()

Symbols = c(table[,1])

table[which(table[,'company'] == 'AXA'),'sector'] = 'banks'
table[which(table[,'company'] == 'Michelin'),'sector'] = 'automobiles'
table[which(table[,'company'] == 'Dassault Systèmes'),'sector'] = 'IT services'
table[str_detect(table[,'company'], 'Safran|Airbus|Thales'),'sector'] = 'aerospace & defense'
table[str_detect(table[,'company'], 'Herm|LVMH|Kering'),'sector'] = 'luxury goods'
table[str_detect(table[,'company'], 'Total|Engie|Technip|Veolia'),'sector'] = 'energy'
table[str_detect(table[,'company'], 'Carrefour|Danone|Sodexo'),'sector'] = 'food products, retailers & services'
table[str_detect(table[,'company'], 'Unibail'),'sector'] = 'real estate'
table[str_detect(table[,'company'], 'Unibail'),'company'] = 'Unibail'
table[str_detect(table[,'company'], 'Orange|Publicis|Vivendi'),'sector'] = 'telecommunications & media'
table[str_detect(table[,'company'], 'Arcelor|Air Liquide'),'sector'] = 'steel & chemical industry'
table[str_detect(table[,'company'], 'STMicro|Legrand|Schneider'),'sector'] = 'electrical components & semiconductor'
table[str_detect(table[,'company'], 'Bouygues|Vinci|Saint-Gobain'),'sector'] = 'construction'
table[str_detect(table[,'company'], 'Essilor|Sanofi'),'sector'] = 'pharmaceuticals & medical supplies'


