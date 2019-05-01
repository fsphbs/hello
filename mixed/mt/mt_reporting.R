
rm(list=ls())

load(file = 'data/mt_103.Rda')

mt.103 <- mt.103[trade.date<as.Date('2019-04-01')]

mt.103$week <- mt.103$trade.date %>% week()
mt.103$month <- format(mt.103$trade.date, '%Y%m')
mt.103$quarter <- quarter(mt.103$trade.date) %>% paste0(year(mt.103$trade.date)-2000,'Q',.)
mt.103$yr <- mt.103$trade.date %>% year()


ggplot(data = mt.103[ccy=='EUR'], aes(x = week, color = yr %>% as.factor )) + 
  geom_line(stat = 'count') + theme_minimal()