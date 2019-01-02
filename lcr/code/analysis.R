

fn_check <- function(){
  
  res <- NULL
  res <- cbind('HQLA cat. 1', FALSE, FALSE, FALSE) 
  res <- rbind(res, cbind('HQLA cat. 2a', FALSE, FALSE, ''))
  res <- rbind(res, cbind('HQLA cat. 2b', FALSE, FALSE, ''))
  
  
  colnames(res) <- c('name', 'amnt', 'amnt.weighted', 'ratio')
  
  return(res %>% data.table())
}