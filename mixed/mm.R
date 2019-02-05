
rm(list=ls())

load('data/mm.Rda')

dat$counterpart <- dat$counterpart %>% as.character()

dat <- dat[ order(trade.date, decreasing = T)]
dat <- dat[ status=='DONE' ]
dat <- dat[ ,-c('post.it', 'des', 'status', 'value.date.2')]

dat$mm.type <- ''
dat$mm.type[dat$qt<0] <- 'deposit'
dat$mm.type[dat$qt>0] <- 'loan'
dat$amnt <- dat$qt * .001
dat$cons <- dat[,amnt * rate * .001]


mm <- dat[asset %like% 'Money Market' & order.type %like% 'Open']
  pos <- mm[,counterpart %like% 'INDUS' | counterpart %like% 'HINDUJA' ] %>% equals(FALSE)
mm <- mm[pos,] 

fidu <- dat[asset %like% 'Fidu' & order.type %like% 'Open']

fn <- function(iDate, mm.inst){
  iDt <- as.Date(iDate)
  res.tmp <- mm.inst[ maturity >= iDt & trade.date <= iDt]
  pos <- res.tmp[,(maturity - iDt) %>% as.numeric() %>% is_weakly_greater_than(30)] 
  res.tmp$mm.type[pos] <- 'loan.LCR'

  res.tmp <- res.tmp[order(maturity), -c('order.type', 'asset')]
    
  return(res.tmp)
}

rcp <- dcast(data = bla, formula = ccy ~ mm.type, fun.aggregate = sum, value.var = 'amnt')

