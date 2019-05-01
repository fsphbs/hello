
rm(list=ls())

load(file = 'data/dt.mt.RDA')

###   mt 103 - from 2017 to 2019
mt.103 <- dt.mt[SWIFT %like% 'SWIFT.103']

mt.103$week <- mt.103$trade.date %>% week()
mt.103$month <- format(mt.103$trade.date, '%Y%m')
mt.103$quarter <- quarter(mt.103$trade.date) %>% paste0(year(mt.103$trade.date)-2000,'Q',.)
mt.103$yr <- mt.103$trade.date %>% year()


### by Dispo.bank and by year
rpt.01 <- dcast(data = mt.103, formula = Dispo.Bank ~ yr, fun.aggregate = length, value.var = 'id')
