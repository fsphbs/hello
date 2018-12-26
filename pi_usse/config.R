
rm(list=ls())

library(xts)
library(tseries)
library(PerformanceAnalytics)

library(tawny)

library(magrittr)
library(data.table)


library(quadprog)



######### ---------------          universe and related names

us.sector.tickers <- c("XLB", "XLE", "XLF", "XLK", "XLI", "XLP", "XLU", "XLV", "XLY")
us.sector.labels <- c("materials", "energy", "financials", "tech", "industrial", 
                      "staples", "utilities", "healthcare", "discretionary")


us.sector.gold.tickers <- c(us.sector.tickers, "GLD")
us.sector.gold.labels <- c(us.sector.labels, 'GOLD')

us.sector.short_ir.tickers <- c(us.sector.tickers, "SHY")
us.sector.short_ir.labels <- c(us.sector.labels, 'S_T')

us.sector.long_ir.tickers <- c(us.sector.tickers, "IEF")
us.sector.long_ir.labels <- c(us.sector.labels, 'L_T')

test.tickers <- c("XLB", "XLE")
test.labels <- c("materials", "energy")