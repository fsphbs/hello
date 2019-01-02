
rm(list=ls())

dat <- readxl::read_excel("data/LCR_report.xls", col_names = FALSE) %>% data.table()

input.date <- dat$X__14[3] %>% as.Date(., format='%d.%m.%Y')



pos <- (dat$X__11 %>% as.numeric() %>% is.na()) == FALSE
dat.set.1 <- dat[pos, cbind(
  des.1 = X__2 %>% as.character(),
  des.2 = X__3 %>% as.character(),
  id = X__11,
  amnt = X__12,
  perc = X__21,
  amnt.weighted = X__23)] %>% data.table()

fn_position <- function(x) {
  pos <- dat.set.1$id %in% x
  res <- dat.set.1[pos, ]
  return(res)
}


if(FALSE){
  
  fn_recap <- function(category.name, field.name, x){
    category <- category.name
    amnt <- fn_position(x)$amnt
    amnt.weighted <- fn_position(x)$amnt.weighted
    
    line <- cbind(category, field.name, amnt, amnt.weighted) %>% data.table()
    
    line$dt <- input.date
    
    return(line)
  }
  
  
  recap <- NULL
  
  ###   HQLA components
  recap <- rbind(recap,
                 fn_recap(category.name = 'HQLA', field.name = 'Total Stock of Category 1', x = 12),
                 fn_recap(category.name = 'HQLA', field.name = 'Total Stock of Category 2a', x = 25)) 
  
  
  ###   OUTFLOW components
  recap <- rbind(recap,
                 fn_recap(category.name = 'OUTFLOW', field.name = 'Total Retail Deposits Run-Off', x = 81),
                 fn_recap(category.name = 'OUTFLOW', field.name = 'Total Unsecured Wholesale Funding Run-Off', x = 121),
                 fn_recap(category.name = 'OUTFLOW', field.name = 'Total Secured Wholesale Funding Run-Off', x = 137),
                 fn_recap(category.name = 'OUTFLOW', field.name = 'Total Additional Requirement', x = 538),
                 fn_recap(category.name = 'OUTFLOW', field.name = 'Total Retail, Small Business Customers, Non-Financials and other Clients', x = 166),
                 fn_recap(category.name = 'OUTFLOW', field.name = 'Total Run-Off other Contingent Funding Obligations', x = 181))
  
  ###   INFLOW components
  recap <- rbind(recap,
                 fn_recap(category.name = 'INFLOW', field.name = 'Total Inflow on Reverse Repo and Securities Borrowing Transactions', x = 195),
                 fn_recap(category.name = 'INFLOW', field.name = 'Total other Inflow by Counterparty', x = 205),
                 fn_recap(category.name = 'INFLOW', field.name = 'Total of other Cash Inflows', x = 209))
  
  
  colnames(recap) <- c('category', 'field.name', 'amnt', 'amnt.weighted', 'date' )
  recap$amnt <- recap$amnt %>% as.numeric()
  recap$amnt.weighted <- recap$amnt.weighted %>% as.numeric()
  
  melt.ex <- melt(data = recap, id.vars = c('date', 'category', 'field.name'))
  
  
}

####

fn_organize <- function(x){
  amnt <- fn_position(x)$amnt
  amnt.weighted <- fn_position(x)$amnt.weighted
  
  line <- cbind(x, amnt, amnt.weighted) 

  return(line)
}

fn_vect <- Vectorize(FUN = fn_organize)


dat.fin <- fn_vect(x = dat.set.1$id) %>% t() %>% data.table()
dat.fin$date <- input.date

colnames(dat.fin) <- c('id', 'amnt', 'amnt.weighted', 'date' )

dat.fin$id <- as.numeric(dat.fin$id) 
dat.fin$amnt[dat.fin$id==270] <- dat.fin$amnt %>% tail(., 1) %>% gsub(pattern = '%', replacement = '')
dat.fin$amnt <- as.numeric(dat.fin$amnt) 
dat.fin$amnt.weighted <- as.numeric(dat.fin$amnt.weighted) 


dat.fin <- dat.fin[,c('date', 'id', 'amnt', 'amnt.weighted')]

dat.fin[amnt %>% is.na]$amnt <- 0
dat.fin[amnt.weighted %>% is.na]$amnt.weighted <- 0

#################################################################################


indxes_hqla.cat.1 <- c(1:11)[-2]
indxes_hqla.cat.2a <- c(16:21, 503:506, 22:26, 507)
indxes_hqla.cat.2b <- c(31, 33)

indxes_outflow.retail.deposit <- c(61:71, 73:77, 516, 78, 517, 80)
indxes_outflow.unsec.wholesale.fund <- c(83:91, 271, 94, 518, 95, 519:520, 96, 274, 97, 521, 98, 522:523, 99)
indxes_outflow.secured.funding <- c()
indxes_outflow.additional.req <- c()
indxes_outflow.other.contract.oblig <- c()
indxes_outflow.other.contingent <- c()

indxes_inflow.secured.lending <- c()
indxes_inflow.other.inflows.by <- c()
indxes_inflow.other.cash.inflows <- c()

indxes_collateral.swap <- c()

####

indx_hqla.cat.1 <- 12
indx_hqla.cat.2a <- 25
indx_outflow.retail.deposit <- 81
indx_outflow.unsec.wholesale.fund <- 121
indx_outflow.secured.funding <- 137

indx_LCR.hqla.cat.1 <- 607
indx_LCR.hqla.cat.2 <- 608

indx_LCR.inflow <- 210
indx_LCR.outflow <- 182

#################################################################################


amnt_hqla.cat.1 <- fn_position(indx_hqla.cat.1)$amnt %>% as.numeric()
amnt_hqla.cat.2 <- fn_position(indx_hqla.cat.2a)$amnt %>% as.numeric()

amnt_outflow.retail.deposit <- fn_position(indx_outflow.retail.deposit)$amnt %>% as.numeric()
amnt_outflow.unsec.wholesale.fund <- fn_position(indx_outflow.unsec.wholesale.fund)$amnt %>% as.numeric()
amnt_outflow.secured.funding <- fn_position(indx_outflow.secured.funding)$amnt %>% as.numeric()



amnt.W_hqla.cat.1 <- fn_position(indx_hqla.cat.1)$amnt.weighted %>% as.numeric()
amnt.W_hqla.cat.2 <- fn_position(indx_hqla.cat.2a)$amnt.weighted %>% as.numeric()

amnt.W_outflow.retail.deposit <- fn_position(indx_outflow.retail.deposit)$amnt.weighted %>% as.numeric()
amnt.W_outflow.unsec.wholesale.fund <- fn_position(indx_outflow.unsec.wholesale.fund)$amnt.weighted %>% as.numeric()
amnt.W_outflow.secured.funding <- fn_position(indx_outflow.secured.funding)$amnt.weighted %>% as.numeric()



amnt.LCR_hqla.cat.1 <- fn_position(indx_LCR.hqla.cat.1)$amnt %>% as.numeric()
amnt.LCR_hqla.cat.2 <- fn_position(indx_LCR.hqla.cat.2)$amnt %>% as.numeric()
amnt.LCR_inflow <- fn_position(indx_LCR.inflow)$amnt %>% as.numeric()
amnt.LCR_outflow <- fn_position(indx_LCR.outflow)$amnt %>% as.numeric()

