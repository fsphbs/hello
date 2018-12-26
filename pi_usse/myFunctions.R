
# function to generat weights for MVP from a return series
minvar <- function(rets) {
  N <- ncol(rets)
  zeros <- array(0, dim = c(N,1))
  aMat <- t(array(1, dim = c(1,N)))
  res <- solve.QP(cov.shrink(rets), zeros, aMat, bvec=1, meq = 1)
  return (res$solution)
}

# function to pretty print strategy statistics
plSummary <-function(dailyPnL)
{
  cumDailyPnL <- cumprod(1 + dailyPnL) - 1
  cat("Max drawdown:", (maxdrawdown(dailyPnL)$maxdrawdown * 100), "%\n")
  cat("Std dev:", sd(dailyPnL), "\n")
  cat("Sharpe:", sharpe(cumDailyPnL), "\n")
  win <- mean(ifelse(dailyPnL > 0, 1, 0))
  cat("Wins:", (win*100), "%\n")
  cat("Losses:", ((1-win)*100), "%\n")
  cat("Average Win:",(mean(ifelse(dailyPnL > 0, dailyPnL, 0)) * 100), "%\n")
  cat("Average Loss:",(mean(ifelse(dailyPnL < 0, dailyPnL, 0)) * 100), "%\n")
}