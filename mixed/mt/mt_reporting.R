
rm(list=ls())

load(file = 'data/dt.mt.RDA')

#######   mt 103 - from 2017 to 2019

mt.103 <- dt.mt[SWIFT %like% 'SWIFT.103']

mt.103$week <- mt.103$trade.date %>% week()
mt.103$month <- format(mt.103$trade.date, '%Y%m')
mt.103$quarter <- quarter(mt.103$trade.date) %>% paste0(year(mt.103$trade.date)-2000,'Q',.)
mt.103$yr <- mt.103$trade.date %>% year()

#######   

mt.103 <- mt.103[trade.date <= as.Date('2019-04-30')] %>% data.table()

###########################################################################################################################

# by RZB (by yr, by quarter and month)

rpt.01 <- dcast(data = mt.103[Dispo.Bank %like% 'RZB'], formula = ccy ~ yr, fun.aggregate = length, value.var = 'id')
rpt.02 <- dcast(data = mt.103[Dispo.Bank %like% 'RZB'], formula = ccy ~ quarter, fun.aggregate = length, value.var = 'id')
rpt.03 <- dcast(data = mt.103[Dispo.Bank %like% 'RZB' & trade.date >= as.Date('2018-01-01') ], 
                formula = month ~ ccy, 
                fun.aggregate = length, value.var = 'id')

# by RZB (by country and yr)

rpt.04 <- dcast(data = mt.103[ccy=='EUR'], formula = country ~ yr, fun.aggregate = length, value.var = 'id')

# by RZB (by country and yr)

rpt.10 <- dcast(data = mt.103, formula =  Dispo.Bank ~ yr, fun.aggregate = length, value.var = 'id')



#write.table(x = rpt.01, 'clipboard', sep = ',', row.names = F)



#ggplot(data = mt.103[ccy=='EUR'], aes(x = week, color = yr %>% as.factor )) + 
#  geom_line(stat = 'count') + theme_minimal()
