
library(RColorBrewer)
coeff <- 0.001
n.datapoint <- 12

my.fn <- function(iCcy){
  
  res <- balances[ccy==iCcy & acc.type=='current' & status =='temporary_balance' &
                    dt <= balances$dt %>% unique() %>% extract(n.datapoint)]
  
  nBanks <- res$counterpart %>% unique() %>% length()
  getPalette <- colorRampPalette(brewer.pal(9, "Spectral"))
  
  
  g <- ggplot(data = res, aes(x = dt, y = amnt * coeff, fill=ticker))
  gg <- g + geom_bar(stat = 'identity') + 
    #scale_fill_brewer(palette = getPalette(nBanks)) +
    scale_fill_manual(values = getPalette(nBanks)) +
    labs(title = iCcy, x='', y= paste0(iCcy," '",'000')) +
    scale_y_continuous(labels = scales::comma) +
    theme_minimal()
  
  plot(gg)
  
  return(res)
}


fn.single <- function(iCcy, bank){
  
  vCcy <- c('CHF', 'EUR', 'USD', 'GBP', 'YEN', 'CAD')
  vColor <- c('red2', 'dodgerblue3', 'seagreen', 'blue3', 'grey80', 'red4')
  pos.ccy <- vCcy %in% iCcy
  bar.col <- vColor[pos.ccy]
  
  res.tmp <- balances[ccy==iCcy & acc.type=='current' & status =='temporary_balance' &
                        ticker==bank & dt <= balances$dt %>% unique() %>% extract(n.datapoint)]
  
  res.diff <- res.tmp$amnt %>% diff() %>% c(res.tmp$amnt[1], .)       # calculates the daily delta, 1st pos is curr. cash pos.
  res <- data.frame(dt=res.tmp$dt, flow=res.diff) %>% data.table()
  
  g <- ggplot(data = res, aes(x = dt, y = flow * coeff, fill=bank))
  gg <- g + geom_bar(stat = 'identity', show.legend = FALSE) +
    #geom_text(aes(label = (flow * coeff) %>% trunc()), size = 3, angle = 90, position = position_stack(vjust = 0.5)) +
    scale_fill_manual(values = bar.col) + theme_minimal() + 
    labs(title = bank, x='', y= paste0(iCcy," '",'000')) +
    scale_x_date(breaks = res.tmp$dt, date_labels = '%a  %d.%m') +
    scale_y_continuous(labels = scales::comma) + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1), panel.grid.minor.x = element_blank())
  plot(gg)
  
  return(res)
}


fn.multiple <- function(iCcy, banks.list) {
  
  vCcy <- c('CHF', 'EUR', 'USD', 'GBP', 'JPY', 'CAD')
  vColor <- c('Reds', 'Blues', 'Greens', 'Purples', 'Greys', 'OrRd')
  pos.ccy <- vCcy %in% iCcy
  bar.col <- vColor[pos.ccy]
  
  pos.flag <- balances$ticker %in% banks.list
  tmp.balances <- balances[pos.flag]
  
  pos <- tmp.balances[,ccy == iCcy & acc.type == 'current' & status == 'temporary_balance' &
                        dt <= balances$dt %>% unique() %>% extract(n.datapoint),]
  
  res <- tmp.balances[pos,]
  
  g <- ggplot(data = res, aes(x = dt, y = amnt * coeff, fill = ticker))
  gg <- g + geom_bar(stat = 'identity', position = position_dodge()) +
    scale_fill_brewer(palette = bar.col) + theme_minimal() + 
    labs(title = iCcy, x='', y= paste0(iCcy," '",'000')) +
    scale_x_date(breaks = res$dt, date_labels = '%a  %d.%m') +
    scale_y_continuous(labels = scales::comma) + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1), panel.grid.minor.x = element_blank())
  
  
  plot(gg)
  
  return(res)
}