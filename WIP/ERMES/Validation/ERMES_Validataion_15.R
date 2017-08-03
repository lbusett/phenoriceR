
load('d:/temp/phenorice/processing/Validation_Ermes/GR/GR_val_2k.RData')
load('d:/temp/phenorice/processing/Validation_Ermes/ES/ES_val_2k.RData')
load('d:/temp/phenorice/processing/Validation_Ermes/IT/IT_val_2k.RData')

load('d:/temp/phenorice/processing/Validation_Ermes/GR/GR_val_or.RData')
load('d:/temp/phenorice/processing/Validation_Ermes/ES/ES_val_or.RData')
load('d:/temp/phenorice/processing/Validation_Ermes/IT/IT_val_or.RData')

load('d:/temp/phenorice/processing/Validation_Ermes/GR/GR_val_250.RData')
load('d:/temp/phenorice/processing/Validation_Ermes/ES/ES_val_250.RData')
load('d:/temp/phenorice/processing/Validation_Ermes/IT/IT_val_250.RData')


es_or$country = 'ES'    ;    it_or$country = 'IT'    ;   gr_or$country = 'GR'
es_250$country = 'ES'   ;    it_250$country = 'IT'   ;   gr_250$country = 'GR'
es_2k$country = 'ES'    ;    it_2k$country = 'IT'    ;   gr_2k$country = 'GR'

es_or_melt$country = 'ES'    ;    it_or_melt$country = 'IT'    ;   gr_or_melt$country = 'GR'
es_250_melt$country = 'ES'   ;    it_250_melt$country = 'IT'   ;   gr_250_melt$country = 'GR'
es_2k_melt$country = 'ES'    ;    it_2k_melt$country = 'IT'    ;   gr_2k_melt$country = 'GR'

data_or = rbind(es_or,it_or,gr_or)
data_or_melt = rbind(es_or_melt,it_or_melt,gr_or_melt)

data_250 = rbind(es_250,it_250,gr_250)

es_250_melt = with(es_250_melt, data.frame (id = id , z = z, counttot = counttot, countdry = NA,
                                              countwat = counttot, Year = Year, variable = variable, value = value, country = country))

gr_250_melt = with(gr_250_melt, data.frame (id = id , z = z, counttot = counttot, countdry = NA,
                                            countwat = counttot, Year = Year, variable = variable, value = value, country = country))
# es_250_melt$value[which(es_250_melt$variable == 'Mod')] = es_250_melt$value[which(es_250_melt$variable == 'Mod')]-8

data_250_melt = rbind(es_250_melt,it_250_melt,gr_250_melt)


it_2k_melt = with(it_2k_melt, data.frame (cell_id = cell_id , z = z, Rice_fc = Rice_fc, counttot = counttot, countdry = NA,
                                         countwat = counttot, Year = Year, variable = variable, value = value, country = country))

es_2k_melt = with(es_2k_melt, data.frame (cell_id = cell_id , z = z, Rice_fc = Rice_fc, counttot = counttot, countdry = NA,
                                            countwat = counttot, Year = Year, variable = variable, value = value, country = country))

gr_2k_melt = with(gr_2k_melt, data.frame (cell_id = cell_id , z = z, Rice_fc = Rice_fc, counttot = counttot, countdry = NA,
                                            countwat = counttot, Year = Year, variable = variable, value = value, country = country))

data_2k = rbind(es_2k,it_2k,gr_2k)
data_2k_melt = rbind(es_2k_melt,it_2k_melt,gr_2k_melt)


# Analysis on or data ----
data_or_sub = droplevels(subset(data_or_melt, area > 1))
stats_or_all = ddply(data_or_sub, .(sowing_met,variable) ,summarize, count = length(value),avg = mean(value, na.rm = T), stdev = sd(value, na.rm = T), mae = mean(abs(value),na.rm = T))
stats_or = ddply(data_or_sub, .(sowing_met,variable,country) ,summarize, count = length(value),avg = mean(value, na.rm = T), stdev = sd(value, na.rm = T), mae = mean(abs(value),na.rm = T))
stats_country_or = ddply(data_or_sub, .(sowing_met,variable,country,year) ,summarize, count = length(value),avg = mean(value, na.rm = T), stdev = sd(value, na.rm = T), mae = mean(abs(value),na.rm = T))

p = ggplot(droplevels(subset(data_or_sub, variable %in% c('sowing_doy','Sow_MOD') )), aes(x = sowing_met ,y = value, color = variable ))
p = p + geom_boxplot()+facet_wrap(~year)+theme_bw()
p

p = ggplot(droplevels(subset(data_or_sub, variable %in% c('sowing_doy','Sow_MOD') )), aes(x = sowing_met ,y = value, color = variable ))
p = p + geom_boxplot(outlier.colour = 'transparent')+facet_grid(country~year)+theme_bw()+coord_cartesian(ylim = c(80,170))
p

p = ggplot(droplevels(subset(stats_country_or, variable %in% c('sowing_doy','Sow_MOD') )), aes(x = sowing_met ,y = avg, color = variable, fill = variable ))
p = p + geom_errorbar(aes(ymin=avg-stdev, ymax=avg+stdev), width=.1)+geom_point()+facet_grid(country~year)+theme_bw()+coord_cartesian(ylim = c(80,170))
p

p = ggplot(droplevels(subset(data_or_sub, variable !='diff')), aes(x = value, fill = variable ))
# p =p+ stat_bin(binwidth = 8, aes(y=..count../sum(..count..)))+facet_grid(sowing_met~variable)+theme_bw()
# p = p+geom_histogram(binwidth = 7)+facet_grid(sowing_met~variable)+theme_bw()
p = p + geom_freqpoly(binwidth = 7)+facet_grid(sowing_met~country)+theme_bw()
#p = p+geom_boxplot(adjust = 1.4)+facet_grid(variable~sowing_met)+theme_bw()+geom_jitter()
p = p+geom_density(alpha = 0.2, adjust = 1.1)+facet_grid(sowing_met~country)+theme_bw()

p

b = (subset(data_or_sub, variable =='Sow_MOD' & sowing_met!= 'Dry')$value)
a = (subset(data_or_sub, variable =='sowing_doy' & sowing_met!= 'Dry')$value)
qplot(a,b)+xlim(80,170)+ylim(80,170)+geom_bin2d(binwidth = c(8,8))+scale_fill_continuous(high = 'black', low = 'white')+theme_bw()

# Analysis on 2k data ---

data_250_sub = droplevels(subset(data_250_melt, countwat > 50))
stats_250_all = ddply(data_250_sub, .(variable) ,summarize, count = length(value),avg = mean(value, na.rm = T), stdev = sd(value, na.rm = T), mae = mean(abs(value),na.rm = T))
stats_250 = ddply(data_250_sub, .(variable,country) ,summarize, count = length(value),avg = mean(value, na.rm = T), stdev = sd(value, na.rm = T), mae = mean(abs(value),na.rm = T))
stats_country_250 = ddply(data_250_sub, .(variable,country,Year) ,summarize, count = length(value),avg = mean(value, na.rm = T), stdev = sd(value, na.rm = T), mae = mean(abs(value),na.rm = T))

p = ggplot(droplevels(subset(data_250_sub, variable %in% c('Mod','FieldWat','FieldTot') )), aes(x = variable ,y = value, color = variable ))
p = p + geom_boxplot()+facet_wrap(~Year)+theme_bw()
p

p = ggplot(droplevels(subset(data_250_sub, variable %in% c('Mod','FieldWat','FieldDry','FieldTot') )), aes(x = variable ,y = value, color = variable ))
p = p + geom_boxplot(outlier.colour = 'transparent')+facet_grid(country~Year)+theme_bw()+coord_cartesian(ylim = c(80,170))
p
#
# p = ggplot(droplevels(subset(stats_country_250, variable %in% c('Mod','FieldWat','FieldTot')), aes(x = variable ,y = avg, color = variable, fill = variable ))
# p = p + geom_err250bar(aes(ymin=avg-stdev, ymax=avg+stdev), width=.1)+geom_point()+facet_grid(country~Year)+theme_bw()+coord_cartesian(ylim = c(80,170)
# p

p = ggplot(droplevels(subset(data_250_sub, variable %in% c('Mod','FieldWat','FieldTot'))), aes(x = value, color = variable ))
# p =p+ stat_bin(binwidth = 8, aes(y=..count../sum(..count..)))+facet_grid(sowing_met~variable)+theme_bw()
# p = p+geom_histogram(binwidth = 7)+facet_grid(sowing_met~variable)+theme_bw()
p = p + geom_freqpoly(binwidth = 8)+facet_grid(Year~country)+theme_bw()
#p = p+geom_boxplot(adjust = 1.4)+facet_grid(variable~sowing_met)+theme_bw()+geom_jitter()
p = p+geom_density(alpha = 0.2, adjust = 1)+facet_grid(Year~country)+theme_bw()

p

a = subset(data_250_sub, ( variable =='Mod' & ((country =='IT' & countwat > 100 ) | (country !='IT' & counttot > 100 ) )))$value
b = subset(data_250_sub, ( variable =='FieldTot' & ((country =='IT' & countwat > 100 ) | (country !='IT'  & counttot > 100) )))$value
qplot(a,b)+xlim(80,170)+ylim(80,170)+geom_hex(binwidth = c(8,8))+scale_fill_continuous(high = 'black', low = 'white')+theme_bw()

# Analysis on 2k data ---

data_2k_sub = droplevels(subset(data_2k_melt, counttot > 100))
stats_2k_all = ddply(data_2k_sub, .(variable) ,summarize, count = length(value),avg = mean(value, na.rm = T), stdev = sd(value, na.rm = T), mae = mean(abs(value),na.rm = T))
stats_2k = ddply(data_2k_sub, .(variable,country) ,summarize, count = length(value),avg = mean(value, na.rm = T), stdev = sd(value, na.rm = T), mae = mean(abs(value),na.rm = T))
stats_country_2k = ddply(data_2k_sub, .(variable,country,Year) ,summarize, count = length(value),avg = mean(value, na.rm = T), stdev = sd(value, na.rm = T), mae = mean(abs(value),na.rm = T))

p = ggplot(droplevels(subset(data_2k_sub, variable %in% c('Mod','FieldWat','FieldTot') )), aes(x = variable ,y = value, color = variable ))
p = p + geom_boxplot()+facet_wrap(~Year)+theme_bw()
p

p = ggplot(droplevels(subset(data_2k_sub, variable %in% c('Mod','FieldWat','FieldDry','FieldTot') )), aes(x = variable ,y = value, color = variable ))
p = p + geom_boxplot(outlier.colour = 'transparent')+facet_grid(country~Year)+theme_bw()+coord_cartesian(ylim = c(80,170))
p
#
# p = ggplot(droplevels(subset(stats_country_2k, variable %in% c('Mod','FieldWat','FieldTot')), aes(x = variable ,y = avg, color = variable, fill = variable ))
# p = p + geom_err2kbar(aes(ymin=avg-stdev, ymax=avg+stdev), width=.1)+geom_point()+facet_grid(country~Year)+theme_bw()+coord_cartesian(ylim = c(80,170)
# p

p = ggplot(droplevels(subset(data_2k_sub, variable %in% c('Mod','FieldWat','FieldTot'))), aes(x = value, color = variable ))
# p =p+ stat_bin(binwidth = 8, aes(y=..count../sum(..count..)))+facet_grid(sowing_met~variable)+theme_bw()
# p = p+geom_histogram(binwidth = 7)+facet_grid(sowing_met~variable)+theme_bw()
p = p + geom_freqpoly(binwidth = 8)+facet_grid(Year~country)+theme_bw()
#p = p+geom_boxplot(adjust = 1.4)+facet_grid(variable~sowing_met)+theme_bw()+geom_jitter()
p = p+geom_density(alpha = 0.2, adjust = 1)+facet_grid(Year~country)+theme_bw()

p

a = subset(data_2k_sub, ( variable =='Mod' & ((country =='IT' & countwat > 100 ) | (country !='IT' & counttot > 100 ) )))$value
b = subset(data_2k_sub, ( variable =='FieldTot' & ((country =='IT' & countwat > 100 ) | (country !='IT'  & counttot > 100) )))$value
qplot(a,b)+xlim(80,170)+ylim(80,170)+geom_hex(binwidth = c(8,8))+scale_fill_continuous(high = 'black', low = 'white')+theme_bw()


