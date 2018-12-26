

if(T)     # config & data
{
  source('code/config.R')
  source('code/myFunctions.R')
  source('code/getPrices.R')
}

usClose <- usClose['1998-12-31/'] # ['2004-12-31/']  # 


### calc returns: ETFs and SPY

rets.cls <- as.xts(data.frame(
  lapply(X = usClose, FUN = CalculateReturns, c(method = 'discrete'))), 
  order.by=index(usClose))[2:nrow(usClose)]

rets.index <- as.xts(data.frame(
  CalculateReturns(prices = usIndex, method = 'discrete')), 
  order.by=index(usIndex))[2:nrow(usIndex)]


# year labels
yr.range <- rets.cls %>% index() %>% year() %>% range()

annualNames <- seq(from = min(yr.range), to = max(yr.range)) %>% as.character() %>% as.array()

annualReturns <- rets.cls     # do.call(rbind, sapply(annualNames, function (yr) { rets.cls[yr] }))        not clear the purpose


# weights
annualWeights <- t(sapply(c(1:length(annualNames)), function(i) { minvar(annualReturns[annualNames[i]]) } ))
colnames(annualWeights) <- usEquities
rownames(annualWeights) <- annualNames

norm.weights <-  ((annualWeights) / (annualWeights %>% abs() %>% rowSums() %>% as.numeric())) 
annualWeights <-  annualWeights # norm.weights #

#annualTradeRets <- matrix(vapply(c(1:(nrow(annualNames)-1)), 
#                                 function (i) { r <- cumsum(annualReturns[annualNames[i+1]] %*% annualWeights[i,]); r[length(r)] }, -100))

dailyPnL <- do.call(rbind, sapply(c(1:(nrow(annualWeights)-1)), 
                                  function (i) { matrix(annualReturns[annualNames[i+1]] %*% annualWeights[i,]) }))

dailyPnL.xts <- xts(x = dailyPnL,order.by = annualReturns[paste(yr.range[1]+1,'/',sep = '')] %>% index())
#dailyPnL.xts %>% add(1) %>% cumprod(.) %>% multiply_by(100) %>%  plot()




