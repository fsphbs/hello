
rm(list=ls())

dt.swap <- fread(
  file = 'data/FXswaps_20190103.csv', sep = ',', header = T)

colnames(dt.swap) <- colnames(dt.swap) %>% gsub(pattern = ' ', replacement = '_', .)
colnames(dt.swap) <- c(colnames(dt.swap)[1:11], 
                       'near.buy', 'far.buy', 'near.sell', 'far.sell', 
                       colnames(dt.swap)[16:18], 'Value_date.near', 'Value_date.far')

dt.swap$Order_date <- dt.swap$Order_date %>% as.Date(., format('%d.%m.%Y'))
dt.swap$Trade_date <- dt.swap$Trade_date %>% as.Date(., format('%d.%m.%Y'))
dt.swap$Value_date.near <- dt.swap$Value_date.near %>% as.Date(., format('%d.%m.%Y'))
dt.swap$Value_date.far <- dt.swap$Value_date.far %>% as.Date(., format('%d.%m.%Y'))


dt.swap$near.buy <- dt.swap$near.buy %>% gsub(x = ., pattern = "'", replacement = '' )
dt.swap$near.buy.amnt <- dt.swap$near.buy %>% substring(text = ., first = 5, 
                                                        last = dt.swap$near.buy %>% length()) %>% as.numeric()

dt.swap$near.sell <- dt.swap$near.sell %>% gsub(x = ., pattern = "'", replacement = '' )
dt.swap$near.sell.amnt <- dt.swap$near.sell %>% substring(text = ., first = 5, 
                                                        last = dt.swap$near.sell %>% length()) %>% as.numeric()

dt.swap$far.buy <- dt.swap$far.buy %>% gsub(x = ., pattern = "'", replacement = '' )
dt.swap$far.buy.amnt <- dt.swap$far.buy %>% substring(text = ., first = 5, 
                                                      last = dt.swap$far.buy %>% length()) %>% as.numeric()

dt.swap$far.sell <- dt.swap$far.sell %>% gsub(x = ., pattern = "'", replacement = '' )
dt.swap$far.sell.amnt <- dt.swap$far.sell %>% substring(text = ., first = 5, 
                                                      last = dt.swap$far.sell %>% length()) %>% as.numeric()



dt.swap$base <- dt.swap$near.buy %>% substring(text = ., first = 1, last = 3)
dt.swap$term <- dt.swap$near.sell %>% substring(text = ., first = 1, last = 3)

if(FALSE){
  dt.raw$pair <- dt.raw[,Asset_leg_1] %>% substring(., first = 20, last = 26) %>% 
    gsub(pattern = '/', replacement = '', x = .)
  dt.raw$base <- dt.raw$pair %>% substring(., first = 1, last = 3)
  dt.raw$term <- dt.raw$pair %>% substring(., first = 4, last = 6)
  
  dt.raw$direction <- dt.raw$base == dt.raw$`Bank_buys_(leg_1)` %>% substr(., 1,3)
  dt.raw$direction[TRUE] <- 'b/s'  
  dt.raw$direction[FALSE] <- 's/b'  
  
  dt.raw$nearLeg.buy_Amnt <- dt.raw$`Bank_buys_(leg_1)` %>% 
    substring(text = ., first = 5, last = dt.raw$`Bank_buys_(leg_1)` %>% nchar()) %>% as.numeric()
  dt.raw$nearLeg.sell_Amnt <- dt.raw$Bank_sells_leg_1 %>% 
    substring(text = ., first = 5, last = dt.raw$Bank_sells_leg_1 %>% nchar()) %>% as.numeric()
  
  dt.raw$FarLeg.buy_Amnt <- dt.raw$`Bank_buys_(leg_2)` %>% 
    substring(text = ., first = 5, last = dt.raw$`Bank_buys_(leg_1)` %>% nchar()) %>% as.numeric()
  dt.raw$FarLeg.sell_Amnt <- dt.raw$Bank_sells_leg_2 %>% 
    substring(text = ., first = 5, last = dt.raw$Bank_sells_leg_1 %>% nchar()) %>% as.numeric()
  
  dt <- dt.raw[Workflow_status %>% tolower() =='done' & Value_date_leg_2 >= Sys.Date(), with=T]
}


dt.swap <- dt.swap[, -c(6:11)]



a <- dt.swap[Value_date.far >= as.Date('2018-12-31') & Workflow_status == 'Done', cbind( ccy = base, amnt = near.buy.amnt, 'loan')] %>% data.table()
b <- dt.swap[Value_date.far >= as.Date('2018-12-31') & Workflow_status == 'Done', cbind(ccy = term, amnt = near.sell.amnt, 'deposit')] %>% data.table()
bla <- rbind(a,b)
bla$amnt <- bla$amnt %>% as.numeric()

dcast(data = bla, formula = ccy ~ V3, fun.aggregate = sum, value.var = 'amnt')
