
rm(list=ls())

source('code/myFunction.R')

balances <- fread('/FSP/My Documents/Analysis/Treasury/Balances/data/current_balances.txt', sep = ',')
tickers <- fread('data/counterparts_list.csv')

colnames(balances) <- c('dt', 'counterpart', 'acc.type', 'status', 'ccy', 'amnt')
balances <- merge.data.frame(x = balances, y = tickers, by = 'counterpart') %>% as.data.table() 


balances$dt <- balances$dt %>% as.Date()
balances$counterpart <- factor(x = balances$counterpart, levels = balances$counterpart %>% unique(), ordered = T)
balances$ticker <- factor(x = balances$ticker, levels = balances$ticker %>% unique(), ordered = T)
balances$amnt <- balances$amnt %>% gsub(., pattern = ',', replacement = '.') %>% as.numeric()
balances <- balances[order(dt, counterpart)]

dt.offset <- (balances$dt %>% unique() %>% as.Date()) - (balances$dt %>% unique() %>% min() %>% as.Date())

ccy.list <- unique(balances$ccy)
counterpart.list <-  unique(balances$counterpart) %>% levels()
ticker.list <-  unique(balances$ticker) %>% levels()
dates.list <-  unique(balances$dt)
ticker.list <-  unique(balances$ticker) %>% levels()
exclude.list <- c('KBBS', 'PF')

neg.bal <- balances[status=='temporary_balance' & amnt < 0 & dt <= (dt %>% unique %>% extract(7))]
if(nrow(neg.bal)!=0){dcast(data = neg.bal, formula = ccy + dt ~ ticker, fun.aggregate = sum, value.var = 'amnt') %>% print()}

cat('\n')
open.book <- balances[status=='open_bookings' & acc.type=='current' & amnt !=0 & dt <= (dt %>% unique %>% extract(1:2))]
if(abs(open.book$amnt) %>% sum() > 0) { dcast(data = open.book, formula = dt + ccy ~ ticker, fun.aggregate = sum, value.var = 'amnt') %>% print()}




###  BCGE and ZKB CHF and EUR optimization.
if(TRUE){
  tmp <- dcast(data = balances[(ccy=='CHF' | ccy=='EUR') & (ticker=='BCGE' | ticker=='ZKB') & 
                                      status =='temporary_balance' & (acc.type=='current' | acc.type=='margin')  & 
                                      (balances$ticker %in% (ticker.list %wo% exclude.list))], 
                    formula = dt ~ ccy + ticker + acc.type, fun.aggregate = sum, value.var = 'amnt')
  
  fx.eurchf <- bdp(securities = 'EURCHF Curncy', fields = 'PX_LAST')
  
  bcge_zkb <- tmp[, cbind(CHF_BCGE_current, CHF_BCGE_margin,  
                                      BCGE = (CHF_BCGE_current + CHF_BCGE_margin), EUR_BCGE_current, 
                                      CHF_ZKB_current, CHF_ZKB_margin, EUR_ZKB_current,  
                                      ZKB = (CHF_ZKB_current + CHF_ZKB_margin + EUR_ZKB_current * fx.eurchf  %>% as.numeric()))] %>% 
                      data.table()

  bcge_zkb <- bcge_zkb %>% multiply_by(.001) %>% format(.,nsmall = 1, digits = 1) %>% data.table() 
  bcge_zkb <- tmp$dt %>% as.character() %>% cbind(date=., bcge_zkb) %>% extract(1:12) 

  colnames(bcge_zkb) <- c('dt', 'GE_CHF_curr', 'GE_CHF_marg', 'BCGE_CHF','BCGE_EUR', 
                          'ZH_CHF_curr', 'ZH_EUR_marg', 'ZH_EUR_curr', 'ZKB')
  }

### get FX rates
if(FALSE){
  ccy.ticker <- paste(ccy.list[ccy.list != 'CHF' & ccy.list != 'SGR'], 'CHF Curncy', sep = '') 
  
  ccy.mkt <- bdp(securities = ccy.ticker, fields = c('PX_LAST', 'QUOTE_FACTOR')) 
  ccy.pair <- ccy.mkt %>% rownames() %>% substr(start = 1, stop = 3) #%>% c(., 'CHF', 'SGR')
  
  ccy.mkt <- ccy.mkt %>% data.table()
  ccy.mkt <- cbind(as.character(ccy.pair),ccy.mkt)
  ccy.mkt <- rbindlist(list(ccy.mkt, data.frame('CHF', 1, 1), data.frame('SGR', 1, 1))) 
  colnames(ccy.mkt) <- c('ccy', 'price', 'factor')
  ccy.mkt$priceadj <- ccy.mkt[ , (price/factor)]
  balances <- merge.data.frame(x = balances, 
                               y = ccy.mkt[,cbind(ccy = (ccy %>% as.character()), fx = as.numeric(priceadj))] %>% data.table(), 
                               by = 'ccy') %>% data.table()
  balances$amnt.CHF <- balances[,amnt * as.numeric(fx)]
  
  tdy.balances <- balances[dt==dates.list[1] & status=='temporary_balance', sum(amnt.CHF), by=c('ccy', 'acc.type')] %>% data.table()
  colnames(tdy.balances) <- c('ccy', 'acc.type', 'amnt.CHF')
}

  







