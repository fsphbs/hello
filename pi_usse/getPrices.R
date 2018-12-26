
# original code
if(FALSE)
{
  # model parameters
  s <- "2003-01-01"
  spyS <- "2004-01-01"
  e <- "2011-01-01"
  q <- "AdjClose"
  
  
  usEquities <- c("XLB", "XLE", "XLF", "XLK", "XLI", "XLP", "XLU", "XLV", "XLY")
  usEquityNames <- c("materials", "energy", "financials", "tech", "industrial", "staples", "utilities", "healthcare", "discretionary")
  
  
  colors <- c('black', 'red', 'blue', 'green', 'orange', 'purple', 'yellow', 'brown', 'pink');
  
  usClose <- as.xts(data.frame(lapply(usEquities, get.hist.quote, start=s, end=e, quote=q)))
  usRets <- xts(data.frame(lapply(log(usClose), diff)), order.by=index(usClose))[2:nrow(usClose)]
  
  colnames(usRets) <- usEquities
  spy <- get.hist.quote("SPY", start=spyS, end=e, quote=q)}


# my data.table
if(FALSE)
{
  xlb <- fread(input = 'data/XLB.csv')
  xle <- fread(input = 'data/XLE.csv')
  xlf <- fread(input = 'data/XLF.csv')
  xlk <- fread(input = 'data/XLK.csv')
  xli <- fread(input = 'data/XLI.csv')
  xlp <- fread(input = 'data/XLP.csv')
  xlu <- fread(input = 'data/XLU.csv')
  xlv <- fread(input = 'data/XLV.csv')
  xly <- fread(input = 'data/XLY.csv')
  
  spy <- fread(input = 'data/SPY.csv')
}


####  US sector ETF + S&P500

if(T)
{
  usEquities <- c("XLB", "XLE", "XLF", "XLK", "XLI", "XLP", "XLU", "XLV", "XLY")
  usEquityNames <- c("materials", "energy", "financials", 
                     "tech", "industrial", "staples", 
                     "utilities", "healthcare", "discretionary")
  
  source('code/getCurrentPrices.R')

  }


# get prices
usClose <- NULL
for(i in length(usEquities):1)
{
  raw.data <- fread(paste('data/', usEquities[i], '.csv', sep = ''))
  raw.data.xts <- as.xts(x = raw.data$`Adj Close`, order.by = raw.data$Date %>% as.Date())
  usClose <- merge.xts(raw.data.xts, usClose)
}

colnames(usClose) <- usEquityNames

### SPY

usIndex <- fread(paste('data/', 'SPY', '.csv', sep = '')) 
usIndex <- as.xts(x = usIndex$`Adj Close`, order.by = usIndex$Date %>% as.Date())

colnames(usIndex) <- 'SPY'


### Integrate hist with current

start.dt <- curr.usClose %>% head(.,1) %>% index()
tmp.usClose <- usClose[paste('/', start.dt, sep = '')] %>% head(-1)

usClose <- rbind(tmp.usClose, curr.usClose)



rm('i', 'raw.data', 'raw.data.xts')