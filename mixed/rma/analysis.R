

source('./code/cleanUp.R')


#################################                   OUT                     ######################################

### sanctioned in rma

rma.sanctioned <- rma[iso %in% seco.sanctioned]
rma.sanctioned_branches <- rma[!(iso %in% seco.sanctioned) & SWIFT_4 %in% (rma.sanctioned$SWIFT_4 %>% unique())]
  
  

### geografical area

# Northern and Sub-Saharan regions
iso.n.afri <- iso[`sub-region` %like% 'Northern Africa']$`alpha-2` %>% unique()
rma.n.afri <- rma[iso %in% iso.n.afri]

iso.sub.sahara <- iso[`sub-region` %like% 'Sub-Saharan Africa']$`alpha-2` %>% unique()
rma.sub.sahara <- rma[iso %in% iso.sub.sahara]


# Central Asia regions
iso.central.asia <- iso[`sub-region` %like% 'Central']$`alpha-2` %>% unique()
rma.central.asia <- rma[iso %in% iso.central.asia]

# Eastern Asia regions
iso.eastern.asia <- iso[`sub-region` %like% 'Eastern Asia']$`alpha-2` %>% unique()
rma.eastern.asia <- rma[iso %in% iso.eastern.asia]

# Western Asia regions
iso.western.asia <- iso[`sub-region` %like% 'Western Asia']$`alpha-2` %>% unique()
rma.western.asia <- rma[iso %in% iso.western.asia]




# Polynesia regions  NONE
iso.polyn <- iso[`sub-region` %like% 'Polyn']$`alpha-2` %>% unique()
rma.polyn <- rma[iso %in% iso.polyn]

# Micronesia regions  NONE
iso.micron <- iso[`sub-region` %like% 'Micro']$`alpha-2` %>% unique()
rma.micron <- rma[iso %in% iso.micron]

# Melanesia regions  NONE
iso.melan <- iso[`sub-region` %like% 'Melan']$`alpha-2` %>% unique()
rma.melan <- rma[iso %in% iso.melan]




#################################                   IN                     ######################################




