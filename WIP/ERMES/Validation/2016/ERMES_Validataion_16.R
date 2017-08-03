library (plyr)
library (dplyr)
library (tidyverse)


load("/home/lb/projects/ermes/datasets/rs_products/Phenology/Validation/2016/IT/IT_val_2k.RData")
load("/home/lb/projects/ermes/datasets/rs_products/Phenology/Validation/2016/ES/es_val_2k.RData")
load("/home/lb/projects/ermes/datasets/rs_products/Phenology/Validation/2016/GR/GR_val_2k.RData")

load("/home/lb/projects/ermes/datasets/rs_products/Phenology/Validation/2016/GR/gr_val_or.RData")
load("/home/lb/projects/ermes/datasets/rs_products/Phenology/Validation/2016/ES/ES_val_or.RData")
load("/home/lb/projects/ermes/datasets/rs_products/Phenology/Validation/2016/IT/IT_val_or.RData")

load("/home/lb/projects/ermes/datasets/rs_products/Phenology/Validation/2016/GR/GR_val_250.RData")
load("/home/lb/projects/ermes/datasets/rs_products/Phenology/Validation/2016/ES/ES_val_250.RData")
load("/home/lb/projects/ermes/datasets/rs_products/Phenology/Validation/2016/IT/IT_val_250.RData")


es_or$country         = "ES"
it_or$country         = "IT"
gr_or$country         = "GR"
es_250$country        = "ES"
it_250$country        = "IT"
gr_250$country        = "GR"
es_2k$country         = "ES"
it_2k$country         = "IT"
gr_2k$country         = "GR"

es_or_melt$country    = "ES"
it_or_melt$country    = "IT"
gr_or_melt$country    = "GR"
es_250_melt$country   = "ES"
it_250_melt$country   = "IT"
gr_250_melt$country   = "GR"
es_2k_melt$country    = "ES"
it_2k_melt$country    = "IT"
gr_2k_melt$country    = "GR"

es_or_melt$sowing_met = "Water"
gr_or_melt$sowing_met = "Water"

data_or = rbind(es_or, it_or, gr_or)
data_or$Sow_MOD[which(data_or$Sow_MOD == 0)] = NA
data_or$diff[which(data_or$Sow_MOD == 0)] = NA

it_or_melt$value[which(it_or_melt$variable == "diff" & it_or_melt$value < -50)] = NA
it_or_melt$value[which(it_or_melt$variable == "Sow_MOD" & it_or_melt$value == 0)] = NA
it_or_melt$value[which(it_or_melt$variable == "diff" & it_or_melt$value == 0)] = NA

data_or_melt = rbind(es_or_melt, it_or_melt, gr_or_melt) %>% as_tibble()

es_250  <- es_250 %>% add_column(FieldDry = NA, FieldWat = es_250$FieldTot, countdry = NA, countwat = es_250$counttot, diffwat = es_250$difftot, 
                                 diffdry = NA) %>% select(id, z, Mod, FieldTot, FieldDry, FieldWat, counttot, countdry, countwat, difftot, diffwat, diffdry, 
                                                          Year, country) %>% as_tibble()

gr_250  <- gr_250 %>% add_column(FieldDry = NA, FieldWat = gr_250$FieldTot, countdry = NA, countwat = gr_250$counttot, diffwat = gr_250$difftot, 
                                 diffdry = NA) %>% select(id, z, Mod, FieldTot, FieldDry, FieldWat, counttot, countdry, countwat, difftot, diffwat, diffdry, 
                                                          Year, country) %>% as_tibble()

it_250 = as_tibble(it_250) %>% filter(Mod != 0)

data_250 = rbind(es_250, it_250, gr_250)

it_250_melt$value[which(it_250_melt$variable == "diff" & it_250_melt$value == 0)] = NA
it_250_melt$value[which(it_250_melt$variable == "Mod" & it_250_melt$value == 0)] = NA

es_250_melt = with(es_250_melt, data.frame(id = id, z = z, counttot = counttot, countdry = NA, countwat = counttot, Year = Year, 
                                           variable = variable, value = value, country = country))

gr_250_melt = with(gr_250_melt, data.frame(id = id, z = z, counttot = counttot, countdry = NA, countwat = counttot, Year = Year, 
                                           variable = variable, value = value, country = country))
# es_250_melt$value[which(es_250_melt$variable == 'Mod')] = es_250_melt$value[which(es_250_melt$variable == 'Mod')]-8

data_250_melt = rbind(es_250_melt, it_250_melt, gr_250_melt) %>% as_tibble()


it_2k_melt = with(it_2k_melt, data.frame(cell_id = cell_id, z = z, Rice_fc = Rice_fc, counttot = counttot, countdry = NA, 
                                         countwat = counttot, Year = Year, variable = variable, value = value, country = country)) %>% as_tibble()

es_2k_melt = with(es_2k_melt, data.frame(cell_id = cell_id, z = z, Rice_fc = Rice_fc, counttot = counttot, countdry = NA, 
                                         countwat = counttot, Year = Year, variable = variable, value = value, country = country)) %>% as_tibble()

gr_2k_melt = with(gr_2k_melt, data.frame(cell_id = cell_id, z = z, Rice_fc = Rice_fc, counttot = counttot, countdry = NA, 
                                         countwat = counttot, Year = Year, variable = variable, value = value, country = country)) %>% as_tibble()

it_2k = as_tibble(it_2k)
es_2k = as_tibble(es_2k)
gr_2k = as_tibble(gr_2k)

es_2k   <- es_2k %>% add_column(FieldDry = rep(NA, 40), countdry = NA, diffdry = NA) %>% select(one_of(names(it_2k))) %>% as_tibble()

gr_2k   <- gr_2k %>% add_column(FieldDry = rep(NA, 10), countdry = NA, diffdry = NA) %>% select(one_of(names(it_2k))) %>% as_tibble()

data_2k = rbind(es_2k, it_2k, gr_2k)
data_2k_melt = rbind(es_2k_melt, it_2k_melt, gr_2k_melt)




# Analysis on or data ----
data_or_sub = droplevels(subset(data_or_melt, area > 0))
stats_or_all = ddply(data_or_sub, .(sowing_met, variable, year), summarize, count = length(value), avg = mean(value, na.rm = T), 
                     stdev = sd(value, na.rm = T), mae = mean(abs(value), na.rm = T))
stats_or = ddply(data_or_sub, .(sowing_met, variable, country), summarize, count = length(value), avg = mean(value, na.rm = T), 
                 stdev = sd(value, na.rm = T), mae = mean(abs(value), na.rm = T))
stats_country_or = ddply(data_or_sub, .(sowing_met, variable, country, year), summarize, count = length(value), avg = mean(value, 
                                                                                                                           na.rm = T), stdev = sd(value, na.rm = T), mae = mean(abs(value), na.rm = T))
stats_country_or_area = ddply(data_or_sub, .(sowing_met, variable, country, year), summarize, count = length(area), avg = mean(area, 
                                                                                                                               na.rm = T), tot = sum(area), stdev = sd(area, na.rm = T), mae = mean(abs(value), na.rm = T))

data_or_sub$gridder = factor(paste(data_or_sub$country, data_or_sub$sowing_met))
## Recoding data_or_sub$gridder into data_or_sub$gridder_rec
data_or_sub$gridder_rec                                    <- as.character(data_or_sub$gridder)
data_or_sub$gridder_rec[data_or_sub$gridder == "ES Water"] <- "Spain"
data_or_sub$gridder_rec[data_or_sub$gridder == "GR Water"] <- "Greece"
data_or_sub$gridder_rec[data_or_sub$gridder == "IT Dry"]   <- "Italy - Dry Seeding"
data_or_sub$gridder_rec[data_or_sub$gridder == "IT Water"] <- "Italy - Water Seeding"
data_or_sub$gridder_rec <- factor(data_or_sub$gridder_rec)
## Reordering data_or_sub$gridder_rec into data_or_sub$gridder_rec_rec
data_or_sub$gridder_rec_rec <- factor(data_or_sub$gridder_rec, levels=c("Spain", "Greece", "Italy - Water Seeding", "Italy - Dry Seeding"))

## Recoding data_or_sub$variable into data_or_sub$variable_rec
data_or_sub$Legend <- as.character(data_or_sub$variable)
data_or_sub$Legend[data_or_sub$variable == "sowing_doy"] <- "Field"
data_or_sub$Legend[data_or_sub$variable == "Sow_MOD"] <- "MODIS"

data_or_sub <- droplevels(subset(data_or_sub, variable != "diff"))
data_or_sub <- droplevels(subset(data_or_sub, is.na(value) == FALSE))

breaks = seq(min(data_or_sub$value, na.rm = T) - 8, max(data_or_sub$value, na.rm = T) + 8 , by = 8)
data_or_sub$value_cut = cut(data_or_sub$value, breaks = breaks)
data_cut <- data_or_sub %>% 
  group_by(gridder_rec_rec, Legend,value_cut) %>%
  summarise(n = n()) %>%
  mutate(Frequency = n / sum(n))
data_cut$Legend = as.factor(data_cut$Legend)

dat2 <- with(data_cut, expand.grid(Legend = levels(Legend), gridder_rec_rec = levels(gridder_rec_rec),
                                   value_cut = levels(value_cut)))
data_cut <- merge(data_cut, dat2, all.y = TRUE)
data_cut$Frequency[is.na(data_cut$Frequency)] <- 0
data_cut$Date = rep(doytodate(seq(84,180,by = 8),2014),8)
p = ggplot(data=data_cut, 
           aes(x = Date, y = Frequency*100, color = Legend, group = Legend), 
           drop = FALSE) + geom_line() + 
  scale_x_date(date_breaks = "1 month", date_labels = "%b ") +
  facet_wrap(~gridder_rec_rec) +  
  ylab("Frequency [%]") + lb_theme_bw() +
  scale_colour_manual(values = c('darkgreen','red')) +
  theme(legend.background = element_rect(colour = "black", size = 0.4, linetype = "solid"))
p

data_cut <- data_or_sub %>% 
  group_by(gridder_rec_rec, year, Legend,value_cut) %>%
  summarise(n = n()) %>%
  mutate(Frequency = n / sum(n))
data_cut$Legend = as.factor(data_cut$Legend)
data_cut$year = as.factor(data_cut$year)

dat2 <- with(data_cut, expand.grid(year = levels(year),Legend = levels(Legend), gridder_rec_rec = levels(gridder_rec_rec),
                                   value_cut = levels(value_cut)))
data_cut <- merge(data_cut, dat2, all.y = TRUE)
data_cut$Frequency[is.na(data_cut$Frequency)] <- 0
data_cut$Date = rep(doytodate(seq(84,180,by = 8),2014),8)
p = ggplot(data=data_cut, 
           aes(x = Date, y = Frequency*100, color = Legend, group = Legend), 
           drop = FALSE) + geom_line() + 
  scale_x_date(date_breaks = "1 month", date_labels = "%b ") +
  facet_grid(year~gridder_rec_rec) +  
  ylab("Frequency [%]") + lb_theme_bw() +
  scale_colour_manual(values = c('darkgreen','red')) +
  theme(legend.background = element_rect(colour = "black", size = 0.4, linetype = "solid"))
p


# p = ggplot(droplevels(subset(data_or_sub, variable %in% c("sowing_doy", "Sow_MOD"))), aes(x = sowing_met, y = value, color = variable))
# p = p + geom_boxplot() + facet_wrap(~year) + theme_bw()
# p
# 
# p = ggplot(droplevels(subset(data_or_sub, variable %in% c("sowing_doy", "Sow_MOD"))), aes(x = sowing_met, y = value, color = variable))
# p = p + geom_boxplot(outlier.colour = "transparent") + facet_grid(country ~ year) + theme_bw() + coord_cartesian(ylim = c(80, 
#   170))
# p
# 
# p = ggplot(droplevels(subset(stats_country_or, variable %in% c("sowing_doy", "Sow_MOD"))), aes(x = sowing_met, y = avg, color = variable, 
#   fill = variable))
# p = p + geom_errorbar(aes(ymin = avg - stdev, ymax = avg + stdev), width = 0.1) + geom_point() + facet_grid(country ~ year) + 
#   theme_bw() + coord_cartesian(ylim = c(80, 170))
# p
# 
# p = ggplot(droplevels(subset(data_or_sub, variable != "diff")), aes(x = value, fill = variable))
# # p =p+ stat_bin(binwidth = 8, aes(y=..count../sum(..count..)))+facet_grid(sowing_met~variable)+theme_bw() p =
# # p+geom_histogram(binwidth = 7)+facet_grid(sowing_met~variable)+theme_bw()
# p = p + geom_freqpoly(binwidth = 7) + facet_grid(sowing_met ~ country) + theme_bw()
# # p = p+geom_boxplot(adjust = 1.4)+facet_grid(variable~sowing_met)+theme_bw()+geom_jitter()
# p = p + geom_density(alpha = 0.2, adjust = 1) + facet_grid(sowing_met ~ country) + theme_bw()
# 
# p


# Analysis on 2k data ---

data_250_sub = droplevels(subset(data_250_melt, countwat > 50))
data_250_sub = droplevels(subset(data_250_melt, countwat > 0))
stats_250_all = ddply(data_250_sub, .(variable), summarize, count = length(value), avg = mean(value, na.rm = T), stdev = sd(value, 
                                                                                                                            na.rm = T), mae = mean(abs(value), na.rm = T))
stats_250 = ddply(data_250_sub, .(variable, country), summarize, count = length(value), avg = mean(value, na.rm = T), stdev = sd(value, 
                                                                                                                                 na.rm = T), mae = mean(abs(value), na.rm = T))
stats_country_250 = ddply(data_250_sub, .(variable, country, Year), summarize, count = length(value), avg = mean(value, na.rm = T), 
                          stdev = sd(value, na.rm = T), mae = mean(abs(value), na.rm = T))

p = ggplot(droplevels(subset(data_250_sub, variable %in% c("Mod", "FieldWat", "FieldTot"))), aes(x = variable, y = value, 
                                                                                                 color = variable))
p = p + geom_boxplot() + facet_wrap(~Year) + theme_bw()
p

p = ggplot(droplevels(subset(data_250_sub, variable %in% c("Mod", "FieldWat", "FieldDry", "FieldTot"))), aes(x = variable, 
                                                                                                             y = value, color = variable))
p = p + geom_boxplot(outlier.colour = "transparent") + facet_grid(country ~ Year) + theme_bw() + coord_cartesian(ylim = c(80, 
                                                                                                                          170))
p
# p = ggplot(droplevels(subset(stats_country_250, variable %in% c('Mod','FieldWat','FieldTot')), aes(x = variable ,y =
# avg, color = variable, fill = variable )) p = p + geom_err250bar(aes(ymin=avg-stdev, ymax=avg+stdev),
# width=.1)+geom_point()+facet_grid(country~Year)+theme_bw()+coord_cartesian(ylim = c(80,170) p

p = ggplot(droplevels(subset(data_250_sub, variable %in% c("Mod", "FieldWat", "FieldTot"))), aes(x = value, color = variable))
# p =p+ stat_bin(binwidth = 8, aes(y=..count../sum(..count..)))+facet_grid(sowing_met~variable)+theme_bw() p =
# p+geom_histogram(binwidth = 7)+facet_grid(sowing_met~variable)+theme_bw()
p = p + geom_freqpoly(aes(y = 100 * ..count../sapply(PANEL, FUN = function(x) sum(count[PANEL == x]))), binwidth = 8) + facet_grid(Year ~ 
                                                                                                                                     country) + theme_bw()
# p = p+geom_boxplot(adjust = 1.4)+facet_grid(variable~sowing_met)+theme_bw()+geom_jitter()
p = p + geom_density(alpha = 0.2, adjust = 1) + facet_grid(Year ~ country) + theme_bw()

p + ylab("Frequency [%}") + xlab("DOY")

a = subset(data_250_sub, (variable == "Mod" & ((country == "IT" & countwat > 100) | (country != "IT" & counttot > 100))))$value
b = subset(data_250_sub, (variable == "FieldTot" & ((country == "IT" & countwat > 100) | (country != "IT" & counttot > 100))))$value
qplot(a, b) + xlim(80, 170) + ylim(80, 170) + geom_hex(binwidth = c(8, 8)) + scale_fill_continuous(high = "black", low = "white") + 
  theme_bw()

# Analysis on 2k data ---

data_2k_sub = droplevels(subset(data_2k_melt)), counttot > 100))
stats_2k_all = ddply(data_2k_sub, .(variable,Year), summarize, count = length(value), avg = mean(value, na.rm = T), stdev = sd(value, 
                                                                                                                               na.rm = T), mae = mean(abs(value), na.rm = T))
stats_2k = ddply(data_2k_sub, .(variable, country), summarize, count = length(value), avg = mean(value, na.rm = T), stdev = sd(value, 
                                                                                                                               na.rm = T), mae = mean(abs(value), na.rm = T))
stats_country_2k = ddply(data_2k_sub, .(variable, country, Year), summarize, count = length(value), avg = mean(value, na.rm = T), 
                         stdev = sd(value, na.rm = T), mae = mean(abs(value), na.rm = T))
data_2k_sub = droplevels(subset(data_2k_sub, variable %in% c("Mod", "FieldWat", "FieldDry")))
                                
data_2k_sub$gridder = factor(paste(data_2k_sub$country, data_2k_sub$variable))
data_2k_sub$variable = as.character(data_2k_sub$variable)
## Recoding data_2k_sub$gridder into data_2k_sub$gridder_rec
data_2k_sub$gridder_rec <- as.character(data_2k_sub$gridder)
data_2k_sub$variable[data_2k_sub$gridder == "ES FieldWat"] <- "Field"
data_2k_sub$gridder_rec[data_2k_sub$gridder == "ES FieldWat"] <- "Spain"
data_2k_sub$variable[data_2k_sub$gridder == "GR FieldWat"] <- "Field"
data_2k_sub$gridder_rec[data_2k_sub$gridder == "GR FieldWat"] <- "Greece"
data_2k_sub$variable[data_2k_sub$gridder == "IT FieldDry"] <- "Field"
data_2k_sub$gridder_rec[data_2k_sub$gridder == "IT FieldDry"] <- "Italy - Dry Seeding"
data_2k_sub$variable[data_2k_sub$gridder == "IT FieldWat"] <- "Field"
data_2k_sub$gridder_rec[data_2k_sub$gridder == "IT FieldWat"] <- "Italy - Water Seeding"
data_2k_sub$variable[data_2k_sub$gridder == "IT Mod"] <- "MODIS"
data_2k_sub$gridder_rec[data_2k_sub$gridder == "IT Mod"] <- "Italy - Water Seeding"
data_2k_sub$variable[data_2k_sub$gridder == "ES Mod"] <- "MODIS"
data_2k_sub$gridder_rec[data_2k_sub$gridder == "ES Mod"] <- "Spain"
data_2k_sub$variable[data_2k_sub$gridder == "GR Mod"] <- "MODIS"
data_2k_sub$gridder_rec[data_2k_sub$gridder == "GR Mod"] <- "Greece"

## Reordering data_2k_sub$gridder_rec into data_2k_sub$gridder_rec_rec
data_2k_sub$gridder_rec_rec <- factor(data_2k_sub$gridder_rec, levels=c("Spain", "Greece", "Italy - Water Seeding", "Italy - Dry Seeding"))

## Recoding data_2k_sub$variable into data_2k_sub$variable_rec
data_2k_sub$Legend <- data_2k_sub$variable
data_2k_sub$Legend[data_2k_sub$variable == "sowing_doy"] <- "Field"
data_2k_sub$Legend[data_2k_sub$variable == "Sow_MOD"] <- "MODIS"

data_2k_sub <- droplevels(subset(data_2k_sub, variable != "diff"))
data_2k_sub <- droplevels(subset(data_2k_sub, is.na(value) == FALSE))

breaks = seq(min(data_2k_sub$value, na.rm = T) - 16, max(data_2k_sub$value, na.rm = T) + 16 , by = 8)
temp = subset(data_2k_sub, Legend == "MODIS" & gridder_rec_rec == "Italy - Water Seeding")
temp$gridder_rec_rec = "Italy - Dry Seeding"
data_2k_sub = rbind(data_2k_sub, temp)
data_2k_sub$value_cut = cut(data_2k_sub$value, breaks = breaks)

data_cut <- data_2k_sub %>% 
  group_by(gridder_rec_rec, Legend,value_cut) %>%
  summarise(n = n()) %>%
  mutate(Frequency = n / sum(n))
data_cut$Legend = as.factor(data_cut$Legend)


dat2 <- with(data_cut, expand.grid(Legend = levels(Legend), gridder_rec_rec = levels(gridder_rec_rec),
                                   value_cut = levels(value_cut)))
data_cut <- merge(data_cut, dat2, all.y = TRUE)
data_cut$Frequency[is.na(data_cut$Frequency)] <- 0
data_cut$Date = rep(doytodate(seq(93-8,150+8,by = 8),2014),8)


p = ggplot(data=data_cut, 
           aes(x = Date, y = Frequency*100, color = Legend, group = Legend), 
           drop = FALSE) + geom_line() + 
  scale_x_date(date_breaks = "1 month", date_labels = "%b ") +
  facet_wrap(~gridder_rec_rec) +  
  ylab("Frequency [%]") + lb_theme_bw() +
  scale_colour_manual(values = c('darkgreen','red')) +
  theme(legend.background = element_rect(colour = "black", size = 0.4, linetype = "solid"))
p

data_cut <- data_or_sub %>% 
  group_by(gridder_rec_rec, year, Legend,value_cut) %>%
  summarise(n = n()) %>%
  mutate(Frequency = n / sum(n))
data_cut$Legend = as.factor(data_cut$Legend)
data_cut$year = as.factor(data_cut$year)

dat2 <- with(data_cut, expand.grid(year = levels(year),Legend = levels(Legend), gridder_rec_rec = levels(gridder_rec_rec),
                                   value_cut = levels(value_cut)))
data_cut <- merge(data_cut, dat2, all.y = TRUE)
data_cut$Frequency[is.na(data_cut$Frequency)] <- 0
data_cut$Date = rep(doytodate(seq(84,180,by = 8),2014),8)
p = ggplot(data=data_cut, 
           aes(x = Date, y = Frequency*100, color = Legend, group = Legend), 
           drop = FALSE) + geom_line() + 
  scale_x_date(date_breaks = "1 month", date_labels = "%b ") +
  facet_grid(year~gridder_rec_rec) +  
  ylab("Frequency [%]") + lb_theme_bw() +
  scale_colour_manual(values = c('darkgreen','red')) +
  theme(legend.background = element_rect(colour = "black", size = 0.4, linetype = "solid"))
p




p = ggplot(droplevels(subset(data_2k_sub, variable %in% c("Mod", "FieldWat", "FieldTot"))), aes(x = variable, y = value, 
                                                                                                color = variable))
p = p + geom_boxplot() + facet_wrap(~Year) + theme_bw()
p

p = ggplot(droplevels(subset(data_2k_sub, variable %in% c("Mod", "FieldWat", "FieldDry", "FieldTot"))), aes(x = variable, 
                                                                                                            y = value, color = variable))
p = p + geom_boxplot(outlier.colour = "transparent") + facet_grid(country ~ Year) + theme_bw() + coord_cartesian(ylim = c(80, 
                                                                                                                          170))
p
# p = ggplot(droplevels(subset(stats_country_2k, variable %in% c('Mod','FieldWat','FieldTot')), aes(x = variable ,y =
# avg, color = variable, fill = variable )) p = p + geom_err2kbar(aes(ymin=avg-stdev, ymax=avg+stdev),
# width=.1)+geom_point()+facet_grid(country~Year)+theme_bw()+coord_cartesian(ylim = c(80,170) p

p = ggplot(droplevels(subset(data_2k_sub, variable %in% c("Mod", "FieldWat", "FieldTot"))), aes(x = value, color = variable))
# p =p+ stat_bin(binwidth = 8, aes(y=..count../sum(..count..)))+facet_grid(sowing_met~variable)+theme_bw() p =
# p+geom_histogram(binwidth = 7)+facet_grid(sowing_met~variable)+theme_bw()
p = p + geom_freqpoly(binwidth = 8) + facet_grid(Year ~ country) + theme_bw()
# p = p+geom_boxplot(adjust = 1.4)+facet_grid(variable~sowing_met)+theme_bw()+geom_jitter()
p = p + geom_density(alpha = 0.2, adjust = 1) + facet_grid(Year ~ country) + theme_bw()

p

a = subset(data_2k_sub, (variable == "Mod" & ((country == "IT" & countwat > 100) | (country != "IT" & counttot > 100))))$value
b = subset(data_2k_sub, (variable == "FieldTot" & ((country == "IT" & countwat > 100) | (country != "IT" & counttot > 100))))$value
qplot(a, b) + xlim(80, 170) + ylim(80, 170) + geom_hex(binwidth = c(8, 8)) + scale_fill_continuous(high = "black", low = "white") + 
  theme_bw()


