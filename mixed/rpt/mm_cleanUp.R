

rm(list=ls())


load(file = 'data/mm.RDA')

mm$amnt.re <- mm$amnt %>% multiply_by(.001)

mm$month <- format(mm$trade.date, '%Y%m')
mm$quarter <- quarter(mm$trade.date) %>% paste0(year(mm$trade.date)-2000,'Q',.)
mm$yr <- mm$trade.date %>% year()
