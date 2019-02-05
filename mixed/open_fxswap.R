

rm(list=ls())

dt.raw <- fread(
  file = '/FSP/My Documents/Analysis/Treasury/Balances/data/FXswaps_20190103.csv', 
  sep = ';', header = T)

colnames(dt.raw)  <- colnames(dt.raw) %>% gsub(pattern = ' ', replacement = '_', .)

dt.raw$Order_date <- dt.raw$Order_date %>% as.Date(., format('%d.%m.%Y'))
dt.raw$Trade_date <- dt.raw$Trade_date %>% as.Date(., format('%d.%m.%Y'))
dt.raw$Value_date_leg_1 <- dt.raw$Value_date_leg_1 %>% as.Date(., format('%d.%m.%Y'))
dt.raw$Value_date_leg_2 <- dt.raw$Value_date_leg_2 %>% as.Date(., format('%d.%m.%Y'))

dt.raw$pair <- dt.raw[,Asset_leg_1] %>% substring(., first = 20, last = 26) %>% 
  gsub(pattern = '/', replacement = '', x = .)
dt.raw$base <- dt.raw$pair %>% substring(., first = 1, last = 3)
dt.raw$term <- dt.raw$pair %>% substring(., first = 4, last = 6)

dt.raw$direction <- dt.raw$base == dt.raw$`Bank_buys_(leg_1)` %>% substr(., 1,3)
dt.raw$direction[TRUE] <- 'b/s'  
dt.raw$direction[FALSE] <- 's/b'  

dt.raw$nearLeg.buy_Amnt <- dt.raw$`Bank_buys_(leg_1)` %>% 
  substring(text = ., first = 5, last = dt.raw$`Bank_buys_(leg_1)` %>% nchar()) %>% as.numeric()
dt.raw$nearLeg.sell_Amnt <- dt.raw$Bank_sells_leg_1 %>% 
  substring(text = ., first = 5, last = dt.raw$Bank_sells_leg_1 %>% nchar()) %>% as.numeric()

dt.raw$FarLeg.buy_Amnt <- dt.raw$`Bank_buys_(leg_2)` %>% 
  substring(text = ., first = 5, last = dt.raw$`Bank_buys_(leg_1)` %>% nchar()) %>% as.numeric()
dt.raw$FarLeg.sell_Amnt <- dt.raw$Bank_sells_leg_2 %>% 
  substring(text = ., first = 5, last = dt.raw$Bank_sells_leg_1 %>% nchar()) %>% as.numeric()

dt <- dt.raw[Workflow_status %>% tolower() =='done' & Value_date_leg_2 >= Sys.Date(), with=T]