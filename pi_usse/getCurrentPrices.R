
# get prices
curr.usClose <- NULL
for(i in length(usEquities):1)
{
  raw.data <- fread(paste('data/current/', usEquities[i], '.csv', sep = ''))
  raw.data.xts <- as.xts(x = raw.data$`Adj Close`, order.by = raw.data$Date %>% as.Date())
  curr.usClose <- merge.xts(raw.data.xts, curr.usClose)
}

colnames(curr.usClose) <- usEquityNames

### SPY

curr.usIndex <- fread(paste('data/current/', 'SPY', '.csv', sep = '')) 
curr.usIndex <- as.xts(x = curr.usIndex$`Adj Close`, order.by = curr.usIndex$Date %>% as.Date())

colnames(curr.usIndex) <- 'SPY'

