

rm(list=ls())

load('data/prezio.Rda')

prezio <- prezio[,-c('Debit_Text_for_advice', 'Credit_Text_for_advice')]
prezio <- prezio[order(Trade_date, decreasing = T)]



####

flow <- prezio[Order_type !=  'FX Swap Leg (Spot)']

flow$eod <- c(1, flow$Trade_date %>% diff.Date() %>% abs())       # identify eod balance
flow$eod <- flow[,eod==0] %>% equals(., FALSE)

flow[eod==TRUE, Balance * .001, Trade_date] %>% plot(., type='b')
abline(h=c(-15000, -10000, 0, 10000), col='grey80', lty=2)



####

p.2019 <- dcast(data = flow[Trade_date > as.Date('2017-12-31') & Trade_date < as.Date('2019-01-01') & 
                              (Order_type == 'FX spot' | Order_type == 'Withdrawal')],
                formula = Trade_date ~ Order_type, 
                fun.aggregate = sum, 
                value.var = 'Quantity') %>% data.table()
colnames(p.2019) <- gsub(x = colnames(p.2019), pattern = ' ', replacement = '_')

p.2019$cFX_spot <- p.2019$FX_spot %>% cumsum()
p.2019$cWithdrawal <- p.2019$Withdrawal %>% cumsum()
p.2019$net.flow <- p.2019[, cFX_spot + cWithdrawal]
