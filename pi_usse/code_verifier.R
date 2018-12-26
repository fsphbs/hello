
rm(list=ls())

r <- cbind(c(0.2, 0.1, 0.05, 0.05, 0.025, 0.0125), 
                c(0.05, 0.025, 0.0125, 0.05, 0.025, 0.0125), 
                c(0.025, 0.0125, 0.00625, 0.025, 0.0125, 0.00625)
                )

w <- cbind(c(0.30, 0.60), 
            c(0.50, 0.25), 
            c(0.20, 0.15))
                
rets <- NULL
r.cum <- NULL
r.ref <- 1
for(i in 0:1){
  r.cum <- r[(1+(3*i)):(3*(1+i)), ] %>% add(1) %>% apply(., 2, cumprod)  #  %>% cumprod() %>% matrix(., ncol=3, byrow=F)
  rets.tmp <- (r.cum %*% w[i+1,]) * r.ref 
  rets <- rbind(rets, rets.tmp)
  r.ref <- rets %>% tail(1) %>% as.numeric
}

rets.xts <- xts(x = rets %>% c(1,.), order.by = seq(from=as.Date('2017-12-31'), to = as.Date('2018-01-06'), by = 'days'))
res <- rets.xts %>% CalculateReturns %>% tail(-1)

