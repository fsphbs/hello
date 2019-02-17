
library(circlize)

rm(list=ls())

if(FALSE){
  
  mat = matrix(1:9, 3)
  rownames(mat) = letters[1:3]
  colnames(mat) = LETTERS[1:3]
  
  df <- data.frame(from = letters[1:3], to = LETTERS[1:3], value = 1:3)
  
}

if(FALSE){
  set.seed(999)
  mat = matrix(sample(18, 18), 3, 6) 
  rownames(mat) = paste0("S", 1:3)
  colnames(mat) = paste0("E", 1:6)
  
  
  df = data.frame(from = rep(rownames(mat), times = ncol(mat)),
                  to = rep(colnames(mat), each = nrow(mat)),
                  value = as.vector(mat),
                  stringsAsFactors = FALSE)
  
  grid.col = c(S1 = "red", S2 = "green", S3 = "blue",
               E1 = "grey", E2 = "grey", E3 = "grey", E4 = "grey", E5 = "grey", E6 = "grey")
}


set.seed(456)

tickers <- c('CBK', 'KBC', 'ZKB')
ccy <- c('EURCHF', 'USDCHF', 'EURUSD')


mat <- matrix(sample(seq(from = 25, to = 250, by = 25), size = 9), nrow = 3, ncol = 3)
mat <- mat / (mat %>% sum())

rownames(mat) <- tickers
colnames(mat) <- ccy 


df.volume = data.frame(from = rep(rownames(mat), times = ncol(mat)),
                to = rep(colnames(mat), each = nrow(mat)),
                value = as.vector(mat),
                stringsAsFactors = FALSE)


dt.volume <- data.frame( bnk=rep(x = tickers, 3), 
                         ccy=rep(x = ccy, 3), 
                         vol=sample(seq(from = 25, to = 250, by = 25), size = 9))

grid.col <- c(EURUSD = 'dodgerblue2', USDCHF = 'forestgreen', EURCHF = 'red3',
              CBK = 'yellow2', KBC = 'lightskyblue', ZKB = 'black')

chordDiagram(t(mat), order = c(ccy, tickers), grid.col = grid.col, scale=T, transparency = 0.5)

if(FALSE){
  svg(filename = 'data/svg_plot.svg')
  chordDiagram(t(mat), order = c(ccy, tickers), grid.col = grid.col, scale=T, transparency = 0.5)
  dev.off()
  }

circos.clear()




