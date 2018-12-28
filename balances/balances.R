
rm(list=ls())

balances <- fread('/FSP/My Documents/Analysis/Treasury/Balances/data/current_balances.txt', sep = ',')
tickers <- fread('data/counterparts_list.csv')

colnames(balances) <- c('dt', 'counterpart', 'acc.type', 'status', 'ccy', 'amnt')
balances <- merge.data.frame(x = balances, y = tickers, by = 'counterpart') %>% as.data.table() 


balances$dt <- balances$dt %>% as.Date()
balances$counterpart <- factor(x = balances$counterpart, levels = balances$counterpart %>% unique(), ordered = T)
balances$ticker <- factor(x = balances$ticker, levels = balances$ticker %>% unique(), ordered = T)
balances$amnt <- balances$amnt %>% gsub(., pattern = ',', replacement = '.') %>% as.numeric()
#balances$amnt <- balances$amnt %>% as.numeric()
balances <- balances[order(dt, counterpart)]

dt.offset <- (balances$dt %>% unique() %>% as.Date()) - (balances$dt %>% unique() %>% min() %>% as.Date())

ccy.list <- unique(balances$ccy)
counterpart.list <-  unique(balances$counterpart) %>% levels()
ticker.list <-  unique(balances$ticker) %>% levels()
dates.list <-  unique(balances$dt)


neg.bal <- balances[status=='temporary_balance' & amnt < 0 & dt <= (dt %>% unique %>% extract(7))]
if(nrow(neg.bal)!=0){dcast(data = neg.bal, formula = ccy + dt ~ ticker, fun.aggregate = sum, value.var = 'amnt') %>% print()}

cat('\n')
open.book <- balances[status=='open_bookings' & acc.type=='current' & amnt !=0 & dt <= (dt %>% unique %>% extract(1))]
if(abs(open.book$amnt) %>% sum() > 0) { dcast(data = open.book, formula = ccy + dt ~ ticker, fun.aggregate = sum, value.var = 'amnt') %>% print()}

dt <- balances[ccy=='GBP' & acc.type=='current' & status =='temporary_balance' & 
                 dt <= balances$dt %>% unique() %>% extract(14)]





