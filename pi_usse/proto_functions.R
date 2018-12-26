


daily.returns <- function(tickers, underlying_name, period){
  
  ########      Get Prices
  ###                       ... from file of yahoo data

  prices <- NULL
  curr.prices <- NULL
  for(i in length(tickers):1)
  {
    raw.data <- fread(paste('data/', tickers[i], '.csv', sep = ''))
    raw.data.xts <- as.xts(x = raw.data$`Adj Close`, order.by = raw.data$Date %>% as.Date())
    prices <- merge.xts(raw.data.xts, prices)
    
    curr.raw.data <- fread(paste('data/current/', tickers[i], '.csv', sep = ''))
    curr.raw.data.xts <- as.xts(x = curr.raw.data$`Adj Close`, order.by = curr.raw.data$Date %>% as.Date())
    curr.prices <- merge.xts(curr.raw.data.xts, curr.prices)
  }
  
  colnames(prices) <- underlying_name
  colnames(curr.prices) <- underlying_name
  
  start.dt <- curr.prices %>% head(.,1) %>% index()
  tmp.prices <- prices[paste('/', start.dt, sep = '')] %>% head(-1)
  
  prices <- rbind(tmp.prices, curr.prices)
  
  
  prices <- prices[period]  #['1998-12-31/']
  
  
  ########      Calculate daily returns
  
  res <- as.xts(x = data.frame(lapply(X = prices, FUN = CalculateReturns, c(method = 'discrete'))),
                order.by=index(prices))[2:nrow(prices)]
  
  return(res)
}


weights <- function(daily_returns, normalized_weights){
  
  normalized_weights <- ifelse(missing(normalized_weights)==T, FALSE, normalized_weights)
  
  
  ### Calculate years range and Assign labels
  
  yr.range <- daily_returns %>% index() %>% year() %>% range()
  yr.label <- seq(from = min(yr.range), to = max(yr.range)) %>% as.character() %>% as.array()
  
  
  ### Calculate weights ws
  
  ws <- t(sapply(c(1:length(yr.label)), function(i) { minvar(daily_returns[yr.label[i]]) } ))
  colnames(ws) <- daily_returns %>% colnames()
  rownames(ws) <- yr.label
  
  
  ws.norm <- ws / (ws %>% abs() %>% rowSums() %>% as.numeric()) 
  if(normalized_weights == TRUE) {ws <- ws.norm}
  
  return(ws)
}         # normalized_weights is boolean


  
daily.returns_Strat <- function(daily_returns, annual_weights){
  
  if(FALSE){
    annualNames <- row.names(annual_weights)
    
    annualTradeRets <- matrix(vapply(c(1:(nrow(annualNames)-1)), 
                                     function (i) { r <- cumsum(daily_returns[annualNames[i+1]] %*% annual_weights[i,]); r[length(r)] }, -100))
    
    dailyPnL <- do.call(rbind, sapply(c(1:(nrow(annual_weights)-1)), 
                                      function (i) { matrix(daily_returns[annualNames[i+1]] %*% annual_weights[i,]) }))
    
    dailyPnL.xts <- xts(x = dailyPnL,
                        order.by = daily_returns[paste(index(daily_returns[1]) %>% year(),'/',sep = '')] %>% index())    
  }

  yr.label <- annual_weights %>% row.names() 
  
  rets <- NULL
  r.cum <- NULL
  r.ref <- 1
  for(i in 1:(nrow(yr.label)-1)){
    
    r.cum <- ((daily_returns[yr.label[i+1]] %>% add(1) %>% apply(., 2, cumprod)) %*% annual_weights[yr.label[i],]) * r.ref 
    
    #r.cum <- r[(1+(3*i)):(3*(1+i)), ] %>% add(1) %>% apply(., 2, cumprod)
    #rets.tmp <- (r.cum %*% w[i+1,]) * r.ref 
    rets <- rbind(rets, r.cum)
    r.ref <- rets %>% tail(1) %>% as.numeric
  }
  
  
  if(F){
    rets.xts <- xts(x = rets %>% c(1,.), order.by = seq(from=as.Date('2017-12-31'), to = as.Date('2018-01-06'), by = 'days'))
    res <- rets.xts %>% CalculateReturns %>% tail(-1)
    
    
    
    res <- do.call(rbind, 
                   sapply(c(1:(nrow(annual_weights)-1)),
                          function (i) { matrix(
                            (daily_returns[yr.label[i+1]] %>%
                               add(1) %>% cumprod()) %*% 
                              annual_weights[i,]) }))
    
    res.xts <- xts(x = res,
                   order.by = daily_returns[paste(index(daily_returns[1]) %>% year() %>% add(1),'/',sep = '')] %>% index()) 
    
    res.fin <- as.xts(x = data.frame(lapply(X = res.xts, FUN = CalculateReturns, c(method = 'discrete'))),
                      order.by=index(res.xts))[2:nrow(res.xts)]
    colnames(res.fin) <- 'rets'
    
  }
  

  res <- xts(x = rets[,1] %>% as.numeric(), order.by = rets %>% rownames() %>% as.Date())
  
  return(res)
}



if(T){
  start.yr <- 2005
  end.yr <- 2018 #start.yr + 3
  start.period <- paste(start.yr, '-12-31/', end.yr, '-12-31', sep = '')
  start.period.index <- paste(start.yr + 1,'-12-31/', end.yr, '-12-31', sep = '')
  
  bla <- daily.returns(tickers = us.sector.tickers, underlying_name = us.sector.labels, period = start.period)
  ws <- weights(daily_returns = bla, normalized_weights = F)
  bla.rets <- daily.returns_Strat(daily_returns = bla, annual_weights = ws)

  bla.gld <- daily.returns(tickers = us.sector.gold.tickers, underlying_name = us.sector.gold.labels, period = start.period)
  ws.gld <- weights(daily_returns = bla.gld, normalized_weights = F)
  bla.gld.rets <- daily.returns_Strat(daily_returns = bla.gld, annual_weights = ws.gld)
  
  bla.st <- daily.returns(tickers = us.sector.short_ir.tickers, underlying_name = us.sector.short_ir.tickers, period = start.period)
  ws.st <- weights(daily_returns = bla.st, normalized_weights = F)
  bla.st.rets <- daily.returns_Strat(daily_returns = bla.st, annual_weights = ws.st)
  
  bla.lt <- daily.returns(tickers = us.sector.long_ir.tickers, underlying_name = us.sector.long_ir.tickers, period = start.period)
  ws.lt <- weights(daily_returns = bla.lt, normalized_weights = F)
  bla.lt.rets <- daily.returns_Strat(daily_returns = bla.lt, annual_weights = ws.lt)
  
  spy <- daily.returns(tickers = 'SPY', underlying_name = 'spy', period = start.period.index)
  spy.rets <- spy %>% add(1) %>% cumprod()
}

