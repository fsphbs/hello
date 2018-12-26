
source('code/myFunctions.R')

usClose <- usClose['1998-12-31/']

# daily returns
usRets <- xts(data.frame(lapply(log(usClose), diff)), order.by=index(usClose))[2:nrow(usClose)]


# annualize trade returns
yr.range <- usRets %>% index() %>% year() %>% range()

annualNames <- seq(from = min(yr.range), to = max(yr.range)) %>% as.character() %>% as.array()
annualReturns <- do.call(rbind, sapply(annualNames, function (yr) { usRets[yr] }))        # actually daily, not annual


# calculate MVP weights
annualWeights <- t(sapply(c(1:length(annualNames)), function(i) { minvar(annualReturns[annualNames[i]]) } ))
colnames(annualWeights) <- usEquities
rownames(annualWeights) <- annualNames


annualTradeRets <- matrix(vapply(c(1:(nrow(annualNames)-1)), 
                                 function (i) { r <- cumsum(annualReturns[annualNames[i+1]] %*% annualWeights[i,]); r[length(r)] }, -100))

dailyPnL <- do.call(rbind, sapply(c(1:(nrow(annualWeights)-1)), 
                                  function (i) { matrix(annualReturns[annualNames[i+1]] %*% annualWeights[i,]) }))


# plot longitudinal evolution of pca component variance
pcaStds <- do.call(cbind, lapply(annualNames, 
                                 function(yr) { sdev <- princomp(covmat=cov.shrink(annualReturns[yr]))$sdev; sdev^2/sum(sdev^2) }))
colnames(pcaStds) <- annualNames