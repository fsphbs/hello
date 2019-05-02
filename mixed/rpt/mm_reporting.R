


rm(list=ls())


load(file = 'data/mm.RDA')

mm$amnt.re <- mm$amnt %>% multiply_by(.001)

mm$month <- format(mm$trade.date, '%Y%m')
mm$quarter <- quarter(mm$trade.date) %>% paste0(year(mm$trade.date)-2000,'Q',.)
mm$yr <- mm$trade.date %>% year()



###  RBI by year

rpt.010 <- dcast(data = mm[counterpart %like% 'RAIF'], formula = ccy ~ yr, fun.aggregate = length, value.var = 'amnt.re')

rpt.011 <- dcast(data = mm[counterpart %like% 'RAIF' & amnt.re > 0], formula = ccy ~ yr, fun.aggregate = length, 
                value.var = 'amnt.re')

rpt.012 <- dcast(data = mm[counterpart %like% 'RAIF' & amnt.re < 0], formula = ccy ~ yr, fun.aggregate = length, 
                value.var = 'amnt.re')



rpt.020 <- dcast(data = mm[counterpart %like% 'RAIF'], formula = ccy ~ yr, fun.aggregate = sum, value.var = 'amnt.re')

rpt.021 <- dcast(data = mm[counterpart %like% 'RAIF' & amnt.re > 0], formula = ccy ~ yr, fun.aggregate = sum, 
                value.var = 'amnt.re')

rpt.022 <- dcast(data = mm[counterpart %like% 'RAIF' & amnt.re < 0], formula = ccy ~ yr, fun.aggregate = sum, 
                value.var = 'amnt.re')




###  ALL by year


rpt.110 <- dcast(data = mm, formula = ccy ~ yr, fun.aggregate = length, value.var = 'amnt.re')

rpt.111 <- dcast(data = mm[ amnt.re > 0], formula = ccy ~ yr, fun.aggregate = length, 
                 value.var = 'amnt.re')

rpt.112 <- dcast(data = mm[ amnt.re < 0], formula = ccy ~ yr, fun.aggregate = length, 
                 value.var = 'amnt.re')



rpt.120 <- dcast(data = mm, formula = ccy ~ yr, fun.aggregate = sum, value.var = 'amnt.re')

rpt.121 <- dcast(data = mm[ amnt.re > 0], formula = ccy ~ yr, fun.aggregate = sum, 
                 value.var = 'amnt.re')

rpt.122 <- dcast(data = mm[ amnt.re < 0], formula = ccy ~ yr, fun.aggregate = sum, 
                 value.var = 'amnt.re')