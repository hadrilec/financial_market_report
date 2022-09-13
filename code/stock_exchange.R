

# install.packages('rtsdata')

library(rtsdata)
library(xml2)
library(rvest)
library(lubridate)
library(dplyr)
library(ggplot2)
library(tidyr)
library(RColorBrewer)
library(stringr)


today_date = today()
# today_date = Sys.Date()

year_current = year(today_date)
# start_date = as.Date(paste0(substr(today_date,1,7), '-01')) %m-% months(25)

start_date = as.Date('2017-12-01')

start_date2 = start_date %m+% months(1)

link_graph = "M:/Usuels.dsc/pRev/FI/prod/graph/stock_exchange"
link_code = "M:/Usuels.dsc/pRev/FI/prod/code/stock_exchange"

list_file_index = c('cac40.R', 'footsie.R', 'dax.R',
                    'sp500.R', 'nikkei.R', 'shanghai.R')

list_file_index = c('cac40.R')

ifile = 6

for(ifile in 1:length(list_file_index)){
  print(list_file_index[[ifile]])
  source(file.path(link_code, list_file_index[[ifile]]))

  # download ####

  file_contrib = file.path(link_graph, sprintf('%s_contrib_index.pdf',index_label))
  file_contrib_sector = file.path(link_graph, sprintf('%s_contrib_index_sector.pdf',index_label))
  file_cap_prices = file.path(link_graph, sprintf('%s_cap_prices.pdf',index_label) )
  file_mrkt_share_index = file.path(link_graph, sprintf('%s_mrkt_share_index.pdf',index_label) )
  file_data = file.path(link_graph, sprintf('%s_data.RData',index_label) )

  env_ = new.env()

  # download data
  for(i in 1:length(Symbols)){

    print(Symbols[[i]])

    getSymbols(Symbols[[i]], env_, src = 'yahoo', from = start_date,
               to = today_date, verbose = TRUE)

  }


  data = list()
  list_ticker_ = names(env_)
  if(any(grepl('getSymb', list_ticker_))){
    list_ticker_ = list_ticker_[-grep('getSymb', list_ticker_)]
  }


  for(ticker in list_ticker_){
    df = env_[[ticker]]
    if(length(df)>0){
      data[[length(data)+1]] = data.frame(time = time(env_[[ticker]][,1]),
                                          Open = as.numeric(env_[[ticker]][,1]),
                                          High = as.numeric(env_[[ticker]][,2]),
                                          Low  = as.numeric(env_[[ticker]][,3]),
                                          Close = as.numeric(env_[[ticker]][,4]),
                                          Volume = as.numeric(env_[[ticker]][,5]),
                                          Adjusted = as.numeric(env_[[ticker]][,6]),
                                          ticker = ticker)
    }

  }

  dfIndex = dplyr::bind_rows(data)
  dfIndex = dfIndex %>% left_join(table, by = 'ticker')

  shares_ticker = list()

  for(ticker in list_ticker_){
    print(ticker)
    url = sprintf('https://finance.yahoo.com/quote/%s?p=%s',ticker, ticker)

    df =  try(read_html(url))
    if(class(df) == "try-error"){
      next
    }

    if(!ticker %in% paste0(c('^',''),index_code)){

      df = df %>% html_table(fill=TRUE)

      if(str_detect(df[[2]][1, 2],'T')){
        markt_cap = gsub('T', '', df[[2]][1, 2])
        markt_cap = as.character(markt_cap)
        markt_cap = as.numeric(markt_cap) * 1000
      }else{
        markt_cap = gsub('B', '', df[[2]][1, 2])
        markt_cap = as.character(markt_cap)
        markt_cap = as.numeric(markt_cap)
      }
      price = as.numeric(gsub(',','',df[[1]][1, 2]))

      shares_ticker[[length(shares_ticker) + 1]] =
        data.frame(ticker = ticker,
                   price = price,
                   markt_cap = markt_cap)

    }

  }


  # for(i in 1:length(shares_ticker)){
  #   shares_ticker[[i]]$markt_cap = as.numeric(as.character(shares_ticker[[i]]$markt_cap))
  # }
  shares_ticker_ = shares_ticker
  shares_ticker = bind_rows(shares_ticker)
  # shares_ticker[9,3] = 1205

  shares_ticker = shares_ticker %>%
    mutate(shares = 1000000000 * markt_cap / price) %>%
    select(ticker, shares)

  data_save = list(dfIndex, shares_ticker)

  save(data_save , file = file_data)
  # load(file_data)
  # dfIndex = data_save[[1]]
  # shares_ticker = data_save[[2]]
  # dataframe ####
  dfIndex_2 = dfIndex %>%
    left_join(shares_ticker) %>%
    # mutate(Adjusted = Close) %>%
    mutate(cap = shares * Adjusted) %>%
    mutate(week = week(time)) %>%
    mutate(year = year(time)) %>%
    mutate(month = month(time, label = TRUE)) %>%
    mutate(day_ = as.Date(time)) %>%
    mutate(time = paste0(substr(as.character(time), 1, 7), '-01')) %>%
    mutate(time = as.Date(time))


  dfIndex_ = dfIndex_2 %>%
    group_by(ticker, time, company) %>%
    filter(day_ == max(day_)) %>%
    ungroup() %>%
    group_by(ticker, time, company) %>%
    dplyr::summarise(cap = mean(cap, na.rm = TRUE)) %>%
    ungroup() %>%
    group_by(time) %>%
    mutate(cap_tot = sum(cap, na.rm = TRUE)) %>%
    ungroup() %>%
    group_by(ticker) %>%
    mutate(cap_growth = (cap / dplyr::lag(cap)-1)*100) %>%
    mutate(contrib = cap_growth * dplyr::lag(cap) / dplyr::lag(cap_tot))

  contribs = dfIndex_ %>%
    group_by(time) %>%
    dplyr::summarise(contribs = sum(contrib, na.rm = TRUE)) %>%
    # filter(year(time) == year_current) %>%
    filter(time >= start_date) %>%
    mutate(label = 'Capitalisation des entreprises')

  cap_tot = dfIndex_ %>%
    ungroup() %>%
    distinct(time, cap_tot) %>%
    mutate(cap_tot_growth = (cap_tot / dplyr::lag(cap_tot)-1)*100) %>%
    mutate(cap_tot_growth = case_when(cap_tot_growth > 10 ~ NA_real_,
                                      cap_tot_growth < -10 ~ NA_real_,
                                      TRUE ~ as.numeric(cap_tot_growth)))

  index = data.frame(date = time(env_[[index_code2]]),
                     value = as.numeric(env_[[index_code2]][,6]))

  index = index %>%
    drop_na() %>%
    mutate(month = month(as.Date(date), label = TRUE)) %>%
    mutate(year = year(date)) %>%
    mutate(time = as.Date(paste0(substr(date,1,7), '-01'))) %>%
    # mutate(time = week(as.Date(date))) %>%
    group_by(time) %>%
    filter(date == max(date)) %>%
    dplyr::summarise(value = mean(value)) %>%
    mutate(index_growth = (value / dplyr::lag(value) - 1) * 100) %>%
    mutate(label = sprintf('Indice %s', index_label))

  colors_ = c(brewer.pal(7,'Set1'), brewer.pal(8,'Set2'), brewer.pal(11,'Set3'),
              brewer.pal(9, 'Pastel1'), brewer.pal(8, 'Pastel2'),
              brewer.pal(8, 'Dark2'), brewer.pal(12, 'Paired'))

  while(length(colors_) < length(unique(dfIndex_$company))){
    colors_ = c(colors_, colors_)
  }

  dfIndex_ = dfIndex_ %>%
    mutate(company = case_when(str_detect(company, 'Unibail-Rodamco-Westfield') ~ 'Unibail',
                               TRUE ~ as.character(company))) %>%
    drop_na(contrib)

  contrib_avg = dfIndex_ %>%
    group_by(company) %>%
    dplyr::summarise(contrib_ = mean(abs(contrib), na.rm = TRUE)) %>%
    arrange(desc(contrib_))

  dfIndex_$company = factor(dfIndex_$company, levels = contrib_avg$company)

  contribs_ = contribs %>% dplyr::rename(value = contribs)

  data_point = index %>%
    select(time, index_growth, label) %>%
    dplyr::rename(value = index_growth) %>%
    bind_rows(contribs_) %>%
    filter(time >= start_date2)

  dfIndex_price_100 = dfIndex %>%
    group_by(ticker) %>%
    filter(time >= start_date2) %>%
    mutate(price_100 = 100 * Adjusted/Adjusted[time == min(time)],
           price_growth = round(price_100 - 100)) %>%
    mutate(price_growth = case_when(price_growth >= 0 ~ paste0('+',price_growth),
                                    TRUE ~ as.character(price_growth)))

  dfIndex_growth_arrange = dfIndex_price_100 %>%
    filter(time == max(time)) %>%
    mutate(label = paste0(substr(company,1,12),' ', price_growth, '%')) %>%
    arrange(desc(price_100))

  label_dfIndex = dfIndex_growth_arrange %>%
    pull(company) %>%
    unique()

  label_dfIndex_growth = dfIndex_growth_arrange %>%
    pull(label) %>%
    unique()

  order_ = grepl(index_label, label_dfIndex)

  label_dfIndex = label_dfIndex[c(which(order_), which(!order_))]
  label_dfIndex_growth = label_dfIndex_growth[c(which(order_), which(!order_))]

  label_dfIndex_growth = str_replace(label_dfIndex_growth, "Unibail-Rodamco-Westfield", "Unibail" )
  label_dfIndex_growth = str_replace(label_dfIndex_growth, "Schneider Electric", "Schneider Electr" )
  label_dfIndex_growth = str_replace(label_dfIndex_growth, "STMicroelectronics", "STMicro" )
  label_dfIndex_growth = str_replace(label_dfIndex_growth, "Dassault Systèmes", "Dassault Syst" )

  dfIndex_bis = dfIndex
  dfIndex_bis$company = factor(dfIndex_bis$company,
                               levels = label_dfIndex, labels = label_dfIndex_growth)

  company_colors = data.frame(company = levels(dfIndex_$company),
                              colors = colors_[1:length(unique(dfIndex_$company))])

  dfIndex_market_val = dfIndex_ %>%
    ungroup() %>%
    group_by(ticker) %>%
    filter(time == max(time)) %>%
    ungroup() %>%
    mutate(cap_share = 100 * cap/sum(cap)) %>%
    mutate(time = 1) %>%
    mutate(cap_share_ = round(cap_share,1)) %>%
    arrange(desc(cap_share))

  company_order = dfIndex_market_val %>%
    select(company, ticker) %>%
    left_join(company_colors) %>%
    mutate(colors = as.character(colors)) %>%
    slice(1:company_number_plotted) %>%
    as.data.frame() %>%
    mutate(ticker = as.character(ticker)) %>%
    mutate(company = as.character(company))

  company_order[nrow(company_order)+1,] = c('OTHER','OTHER', brewer.pal(8, 'Pastel2')[8])

  dfIndex_market_val_ = dfIndex_market_val %>%
    select(time, company, cap_share) %>%
    mutate(company = case_when(company %in% company_order$company ~ as.character(company),
                               TRUE ~ 'OTHER')) %>%
    group_by(company) %>%
    filter(company != 'OTHER') %>%
    dplyr::summarise(cap_share = sum(cap_share))

  pct_market_val_select = sum(dfIndex_market_val_$cap_share)

  dfIndex_bis = dfIndex_bis %>%
    filter(time >= start_date2) %>%
    filter(ticker %in% c(company_order$ticker, index_code, index_code2))

  dfIndex_contrib = dfIndex_ %>%
    select(time, company, contrib, ticker) %>%
    mutate(company = case_when(company %in% company_order$company ~ as.character(company),
                               TRUE ~ 'OTHER')) %>%
    ungroup() %>%
    group_by(time, company) %>%
    dplyr::summarise(contrib = sum(contrib))

  dfIndex_contrib$company = factor(dfIndex_contrib$company,
                                   levels = company_order$company)


  dfIndex_market_val$company = factor(dfIndex_market_val$company,
                                      levels = company_order$company)



  cap_tot_dfIndex = dfIndex_market_val %>%
    pull(cap_tot) %>%
    unique() %>%
    as.numeric() %>%
    round() %>%
    max()

  dfIndex_sector = 
    dfIndex_ %>%
    left_join(table) %>% 
    ungroup() %>% 
    filter(time %in% c(as.Date("2017-12-01"), as.Date("2019-12-01"), as.Date("2018-12-01"),
                       as.Date("2020-12-01"), as.Date("2021-04-01"))) %>% 
    select(-c("cap_tot", "cap_growth", "contrib")) %>% 
    group_by(sector, time) %>% 
    mutate(cap_sector = sum(cap)) %>% 
    group_by(sector) %>% 
    mutate(growth_from_dec18 = 100*(cap_sector/cap_sector[time == as.Date("2018-12-01")]-1))
  
  write.csv(dfIndex_sector, file = file.path(Sys.getenv("USERPROFILE"), "Desktop", "data_market_cap_sector.csv"))
  
  dfIndex_market_val = dfIndex_market_val %>% drop_na()

  subtt = sprintf("la capitalisation totale est de %s milliards %s \n les entreprises représentées représentent %s%% du total", round(cap_tot_dfIndex/1000000000), currency, round(pct_market_val_select))

  title_market_val = sprintf("Poids des entreprises du %s dans la capitalisation totale", index_label)

  dfIndex_contrib_sector = data.frame()
  if('sector' %in% names(table)){

    dfIndex_contrib_sector = dfIndex_ %>%
      select(time, company, contrib, ticker, cap) %>%
      group_by(time, company) %>%
      dplyr::summarise(contrib = sum(contrib)) %>%
      left_join(table) %>%
      ungroup() %>%
      group_by(time, sector) %>%
      dplyr::summarise(contrib = sum(contrib))

    contrib_sector_arr = dfIndex_contrib_sector %>%
      ungroup() %>%
      group_by(sector) %>%
      dplyr::summarise(contrib_ = mean(abs(contrib))) %>%
      arrange(desc(contrib_))

    dfIndex_contrib_sector$sector = factor(dfIndex_contrib_sector$sector ,
                                           levels = contrib_sector_arr$sector)
  }


  #
  # plot market valuation share of total ####
  #


  ggplot(data = dfIndex_market_val,
         aes(x = time, y = cap_share, fill = company)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    # geom_text(aes(label = cap_share_), hjust = 0.5,
    #           position = position_dodge(width = 1)) +
    # annotate(company_order$colors) +
    scale_y_continuous(breaks = 0:100, labels = function(x) paste0(x, "%")) +
    scale_fill_manual(values = as.character(company_order$colors)) +
    theme(
      plot.subtitle = element_text(hjust = 0.5),
      plot.title = element_text(hjust = 0.5),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.x = element_blank()
    ) +
    ggtitle(title_market_val) +
    labs(subtitle = subtt) +
    guides(fill = guide_legend(ncol = 2)) +
    ggsave(filename = file_mrkt_share_index, width = 15, height = 10)

  #
  # plot stock prices variation ####
  #
  # dfIndex_bis_company = dfIndex_bis$company
  # dfIndex_bis$company = substr(dfIndex_bis_company,1,17)

  title_facet = sprintf('Evolution du cours de bourse des entreprises du %s entre %s et %s', index_label, start_date2, today_date)
  subtitle = sprintf("Dernier point : %s", today_date)
  ggplot(dfIndex_bis, aes(x = time, y = Adjusted, colour = company)) +
    geom_line(show.legend = FALSE) +
    facet_wrap(~company, scales = 'free_y') +
    theme(
      plot.title = element_text(hjust = 0.5),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1),
      strip.text = element_text(size = 14)
    ) +
    # ggthemes::theme_stata() +
    ggtitle(title_facet) +
    ggsave(filename = file_cap_prices, width = 15, height = 10)



  #
  # plot contribution to index ####
  #

  title = sprintf("%s - variation de la capitalisation des entreprises et de l'indice",index_label)

  xaxis_breaks = seq.Date(from = start_date2,
                          to = max(dfIndex_$time), by = '1 month')

  yaxis_breaks = -50:50

  source_ = "Source : Yahoo Finance"
  hypoth = "Hypothèses de calcul de la capitalisation :"
  hypoth_1 = "- nombre d'actions constant par entreprise sur la période"
  hypoth_2 = "- même les actions non flottantes sont comprises"
  subtitle = sprintf("%s", source_)
  caption = sprintf("%s %s / %s",  hypoth, hypoth_1, hypoth_2)

  ggplot() +
    geom_bar(data = dfIndex_contrib,
             aes(x = time, y = contrib, fill = company),
             stat = 'identity', position = 'stack') +
    geom_point(data = data_point, aes(x = time, y = value, shape = label)) +
    geom_line(data = index, aes(x = time, y = index_growth)) +
    ggthemes::theme_stata() +
    scale_fill_manual(values = company_order$colors) +
    scale_shape_manual(values = c(16, 17)) +
    scale_linetype_manual(values = c('dashed')) +
    scale_y_continuous(labels = function(x) paste0(x, '%'),
                       breaks = yaxis_breaks,
                       sec.axis = dup_axis()) +
    scale_x_date(breaks = xaxis_breaks,
                 limits = c(start_date2 %m-% days(15),
                            max(dfIndex_$time) %m+% days(15)),
                 date_labels =  "%b %Y") +
    ggtitle(title) +
    labs(subtitle = subtitle, caption = caption) +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.y = element_text(angle = 0),
      axis.text.x = element_text(angle = 90),
      legend.text = element_text(size = 9),
      legend.title = element_blank(),
      legend.position = 'right'
    ) + # guides(fill = guide_legend(nrow = 4, byrow = TRUE)) +
    guides(fill = guide_legend(ncol = 2)) +
    ggsave(filename = file_contrib, width = 15, height = 10)





  # plot contribution by sector to index ####
  if('sector' %in% names(dfIndex_contrib_sector)){

    title_sector = sprintf("%s - variation de l'indice et contribution par secteur",index_label)

    ggplot() +
      geom_bar(data = dfIndex_contrib_sector,
               aes(x = time, y = contrib, fill = sector),
               stat = 'identity', position = 'stack') +
      geom_point(data = data_point, aes(x = time, y = value, shape = label)) +
      geom_line(data = index, aes(x = time, y = index_growth)) +
      scale_fill_manual(values = colors_) +
      ggthemes::theme_stata() +
      scale_shape_manual(values = c(16, 17)) +
      scale_linetype_manual(values = c('dashed')) +
      scale_y_continuous(labels = function(x) paste0(x, '%'),
                         breaks = yaxis_breaks,
                         sec.axis = dup_axis()) +
      scale_x_date(breaks = xaxis_breaks,
                   limits = c(start_date2 %m-% days(15),
                              max(dfIndex_$time) %m+% days(15)),
                   date_labels =  "%b %Y") +
      ggtitle(title_sector) +
      labs(subtitle = subtitle, caption = caption) +
      theme(
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_text(angle = 0),
        axis.text.x = element_text(angle = 90),
        legend.text = element_text(size = 9),
        legend.title = element_blank(),
        legend.position = 'right'
      ) +
      guides(fill = guide_legend(ncol = 2)) +
      ggsave(filename = file_contrib_sector, width = 15, height = 10)
  }


}



