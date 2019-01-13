
rm(list=ls())

dat <- readxl::read_excel("data/snbgc_full.en.xlsx", col_names = TRUE, skip = 3) %>% data.table()
colnames(dat) <- colnames(dat) %>% gsub(pattern = ' ', replacement = '_', x = .)

dat$Maturity <- as.Date(x = dat$Maturity) 
dat$yrs <- dat$Maturity %>% difftime(time1 = ., time2 = Sys.Date()) %>% as.numeric() %>%multiply_by(1/365)
dat$cut <- cut(dat$yrs, breaks = 0:100)


###    calc yield on domestic and foreign CHF bonds

chf <- dat[Currency=='CHF']

chf.dom <- fread(input = 'data/bonds_chf_domestic_2019-01-11.csv')
chf.fgn <- fread(input = 'data/bonds_chf_foreign_2019-01-11.csv')

des.dom <- chf.dom[,cbind(ISIN, ClosingPrice)] %>% data.table()
des.fgn <- chf.fgn[,cbind(ISIN, ClosingPrice)] %>% data.table()
des <- rbind(des.dom, des.fgn)
chf <- merge.data.frame(x = chf, y = des, by = 'ISIN') %>% data.table()         # merge w/SNB eligible 
chf$yld <- bond.yields(settle = as.Date('2019-01-15'), mature = chf$Maturity, coupon = chf$`Coupon_in_%` * .01,
                       freq = 1, price = chf$ClosingPrice %>% as.numeric(), comp.freq = 1)


snb.name <- 'Eidgenossenschaft'
pfand.name <- c('Pfandbriefzentr. Schweiz. Kantonalbanken AG', 'Pfandbriefbank schweiz. Hypothekarinstitute AG')

snb <- chf[Issuer==snb.name, yld, yrs] %>% data.table()
pfand <- chf[Issuer==pfand.name[1] | Issuer==pfand.name[2], yld, yrs] %>% data.table()

mod.loess <- loess(data = snb, formula = yld ~ yrs)
new.data <- data.frame(yrs=c(1:5))
y_hat <- predict(object = mod.loess, newdata = new.data)


new.data <- data.frame(yrs=c(1:5))

dat.subset <-chf[yrs < 6 & yld > -0.0125 & HQLA=='L2a']
mod.loess.l2a <- loess(data = dat.subset, formula = yld ~ yrs, span = 0.5)
y_hat.l2a <- predict(object = mod.loess.l2a, newdata = new.data)


dat.subset <-chf[yrs < 6 & yld > -0.0125 & HQLA=='L1']
mod.loess.l1 <- loess(data = dat.subset, formula = yld ~ yrs)
y_hat.l1 <- predict(object = mod.loess.l1, newdata = new.data)






