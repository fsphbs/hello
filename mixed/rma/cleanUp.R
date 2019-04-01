

source('code/getData.R')

###   (bp) Avaloq Active list: identify 8char swifts and create a SWIFT column

if(TRUE){
  
  bp$SWIFT <- ''
  bp$SWIFT_4 <- ''
  bp$iso <- ''
  
  pos.numeric <- suppressWarnings(as.numeric(bp$Sort_alpha))  %>% is.na() %>% equals(FALSE)
  
  bp[nchar(bp$Sort_alpha) == 11]$SWIFT <- bp$Sort_alpha[nchar(bp$Sort_alpha) == 11] %>% substr(x = ., start = 1, stop = 8)
  bp[nchar(bp$Sort_alpha) == 8]$SWIFT <- bp$Sort_alpha[nchar(bp$Sort_alpha) == 8] 
  
  bp$SWIFT[pos.numeric] <- ''
  bp[SWIFT != '']$SWIFT_4 <- substr(x = bp[SWIFT != '']$SWIFT, start = 1, stop = 4) 
  
  bp[SWIFT != '']$iso <- substr(x = bp[SWIFT != '']$SWIFT, start = 5, stop = 6) 
  
  bp <- bp[, -c('Product', 'Registered_Ownership', 'Primary_contact_person', 'Primary_Contact_Remark')]
  
  rm('pos.numeric')

}



###   (rma) Avaloq RMA list: delete unused columns and create a SWIFT column

if(TRUE){
  
  rma$SWIFT <- rma$Corr_Bic
  rma$SWIFT_4 <- substr(x = rma$SWIFT, start = 1, stop = 4) 
  
  rma$iso <- substr(x = rma$Corr_Bic , start = 5, stop = 6) 
  rma <- rma[, -c('Pending', 'Valid_From', 'Valid_To', 'History', 'Export_Date')]
  
}


if(TRUE){
  
  cfi$iso <- ''
  cfi[SWIFT != '']$iso <- substr(x = cfi[SWIFT != '']$SWIFT, start = 5, stop = 6) 
  
}





###   (swift) Internet 2017 SWIFT list: create a SWIFT column and a 4 char SWIFT

if(TRUE){
  
  colnames(swift) <- c('BANK_NAME', 'iso', 'SWIFT_11')
  swift$SWIFT_4 <- substr(x = swift$SWIFT_11 , start = 1, stop = 4) 
  swift$SWIFT <- substr(x = swift$SWIFT_11 , start = 1, stop = 8)
  swift$iso <-  substr(x = swift$SWIFT , start = 5, stop = 6) 
  
}



###  matching name algo
if(FALSE){
  cif$BANK_NAME <- ''
  cif$SWIFT <- ''
  for(i in 1:nrow(cif)){
    score <- stringdist(a = cif[i]$name %>% as.character(), b = swift$BANK_NAME, method = 'lv') 
    pos <- score == min(score) 
    cif$BANK_NAME[i] <- swift[pos]$BANK_NAME %>% head(1)
    cif$SWIFT[i] <- swift[pos]$SWIFT %>% head(1)
  }
}

