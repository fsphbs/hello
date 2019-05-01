
rm(list=ls())

load('data/mt.RDA')

yrs <- c(2017, 2018, 2019)

dt.mt <- NULL
for(i in 1:3){
  dt.raw <- mt[i] %>% as.data.table()
  
  dt.raw$Zeit <- dt.raw$Zeit %>% substr(., start = 1, stop = 10) %>% as.Date(., '%Y.%m.%d')
  
  colnames(dt.raw) <- colnames(dt.raw) %>% gsub(., pattern = '-', replacement = '.')
  
  dt.basic <- dt.raw[,cbind(id = Auftrag, Erfasser, trade.date = Zeit %>% as.character(), 
                            status = Status, ccy = Whrg., amnt = Betrag,  SWIFT = Settle.Plan, 
                            bank.name = Dest.Bank, country = `Dest. Land`, Dispo.Bank)] %>% data.table() 
  
  dt.basic$trade.date <- dt.basic$trade.date %>% as.Date()
  tmp <- dt.basic[year(trade.date)==yrs[i]] %>% as.data.table()
  
  dt.mt <- rbind(dt.mt, tmp)
}


mt.103 <- dt.mt[SWIFT %like% 'SWIFT.103']


save(mt.103, file = 'data/mt_103.Rda')

