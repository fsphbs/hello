

bp <- fread(file = '/FSP/My Documents/Analysis/CFO/Audit - Counterparts/data/Active_Counterparts.csv', header = T)
rma <- fread(file = '/FSP/My Documents/Analysis/CFO/Audit - Counterparts/data/RMAs.csv', header = T)
cif <- read.csv(file = '/FSP/My Documents/Analysis/CFO/Audit - Counterparts/data/CIF.csv', 
                header = T, stringsAsFactors = F) %>% data.table()
iso <- fread(file = '/FSP/My Documents/Analysis/CFO/Audit - Counterparts/data/country_ISO.csv', header = T)

  colnames(bp) <- colnames(bp) %>% gsub(pattern = ' ', replacement = '_', x = .)
  colnames(rma) <- colnames(rma) %>% gsub(pattern = ' ', replacement = '_', x = .)

###

  load(file = '/FSP/My Documents/Analysis/CFO/Audit - Counterparts/data/swift_code_tbl.Rda')
  swift.tbl <- do.call(rbind, swift_codes.tbl[-length(swift_codes.tbl)]) %>% data.table()
  colnames(swift.tbl) <- swift.tbl[1,] %>% as.character() %>% gsub(pattern = ' ', replacement = '_')
  swift.tbl <- swift.tbl %>% tail(-1)

###

pos.numeric <- bp$Sort_alpha %>% as.numeric() %>% is.na() %>% equals(FALSE)

bp$SWIFT <- ''

bp$SWIFT[nchar(bp$Sort_alpha) == 11] <- bp$Sort_alpha[nchar(bp$Sort_alpha) == 11] %>% substr(x = ., start = 1, stop = 8)
bp$SWIFT[nchar(bp$Sort_alpha) == 8] <- bp$Sort_alpha[nchar(bp$Sort_alpha) == 8] 

bp$SWIFT[bp$SWIFT %>% as.numeric() %>% is.na() %>% equals(FALSE)] <- ''


###     ISO country

#iso.list <- substr(x = rma$Corr_Bic , start = 5, stop = 6) %>% table() %>% data.table
#colnames(iso.list) <- c('iso', 'freq')
#iso.list <- iso.list[order(freq, decreasing = T)]

rma$iso <- substr(x = rma$Corr_Bic , start = 5, stop = 6) 
rma$bank.code <- substr(x = rma$Corr_Bic , start = 1, stop = 4) 


iso.tbl <- iso[,name,`alpha-2`] 
colnames(iso.tbl) <- c('iso', 'official.country')

rma <- merge.data.frame(x = rma, y = iso.tbl, by = 'iso') %>% data.table()

bank.code.list <- rma$bank.code %>% table() %>% data.table
colnames(bank.code.list) <- c('bank.code', 'freq')
bank.code.list <- bank.code.list[order(freq, decreasing = T)]


###     SWIFT code
swift.flag <- FALSE


  swift.tbl <- swift.tbl[,-('COUNTRY_CODE')]
  swift.tbl$SWIFT_CODE <- substr(x = swift.tbl$SWIFT_CODE, start = 1, stop = 8)
  
  pos.head <- (swift.tbl$BANK_NAME == 'BANK NAME') %>% equals(FALSE)
  swift.tbl <- swift.tbl[pos.head,]
  swift.tbl$full.name <- as.character(lapply(strsplit(as.character(swift.tbl$BANK_NAME), split=","), "[", 1))
  swift.tbl <- swift.tbl[,-('BANK_NAME')]
  
if(swift.flag){  
  rma <- merge.data.frame(x = swift.tbl, y = rma, by.x = 'SWIFT_CODE', by.y = 'Corr_Bic' ) %>% data.table()
  res <- merge.data.frame(x = bp, y = rma, by.x = 'SWIFT', by.y = 'Corr_Bic' ) %>% data.table()
}


if(swift.flag==FALSE){res <- merge.data.frame(x = bp, y = rma, by.x = 'Sort_alpha', by.y = 'Corr_Bic' )}
  
  cif$proxy <- ''
  cif$SWIFT <- ''
  for(i in 1:nrow(cif)){
    score <- stringdist(a = cif[i]$name %>% as.character(), b = swift.tbl$full.name, method = 'lv') 
    pos <- score == min(score) 
    cif$proxy[i] <- swift.tbl[pos]$full.name %>% head(1)
    cif$SWIFT[i] <- swift.tbl[pos]$SWIFT_CODE %>% head(1)
  }
  
  
  
