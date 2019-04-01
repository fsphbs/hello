

rm(list=ls())

####  Avaloq extract
bp <- fread(file = '/FSP/My Documents/Analysis/CFO/Audit - Counterparts/data/Active_Counterparts.csv', header = T)
colnames(bp) <- colnames(bp) %>% gsub(pattern = ' ', replacement = '_', x = .)

####  Central File list
cfi <- read.csv(file = '/FSP/My Documents/Analysis/CFO/Audit - Counterparts/data/CFI.csv', 
                header = T, stringsAsFactors = F, sep = ';') %>% data.table()

####  MO RMAs list
rma <- fread(file = '/FSP/My Documents/Analysis/CFO/Audit - Counterparts/data/RMAs.csv', header = T)
colnames(rma) <- colnames(rma) %>% gsub(pattern = ' ', replacement = '_', x = .)

####  Trade Finance list
trade.fin <- fread(file = '/FSP/My Documents/Analysis/CFO/Audit - Counterparts/data/Trade_Finance.csv', header = T)

####  Credit Aproved list
credit <- fread(file = '/FSP/My Documents/Analysis/CFO/Audit - Counterparts/data/credit.csv', header = T)


#################################         UTILITIES

####        Utilities  (ISO country code)
iso <- fread(file = '/FSP/My Documents/Analysis/CFO/Audit - Counterparts/data/country_ISO.csv', header = T)


####        Utilities  (Swift Banks List)
load(file = '/FSP/My Documents/Analysis/CFO/Audit - Counterparts/data/swift_code_tbl.Rda')
swift <- do.call(rbind, swift_codes.tbl[-length(swift_codes.tbl)]) %>% data.table()
colnames(swift) <- swift[1,] %>% as.character() %>% gsub(pattern = ' ', replacement = '_')
swift <- swift %>% tail(-1)

rm('swift_codes.tbl')

#https://www.seco.admin.ch/seco/fr/home/Aussenwirtschaftspolitik_Wirtschaftliche_Zusammenarbeit/Wirtschaftsbeziehungen/exportkontrollen-und-sanktionen/sanktionen-embargos/sanktionsmassnahmen.html
seco.sanctioned <- c('IQ', 'MM', 'ZW', 'SS', 'SD', 'CG', 'BI', 'KP', 'LB', 'IR', 
                     'SO', 'GN', 'GW', 'LY', 'SY', 'CF', 'YE', 'BI', 'ML', 'VE')
