
###############################################################################
library(plyr)
library(ggplot2)
library(reshape2)
library(tools)
library(rgdal)
library(raster)
library(data.table)

es_shape_file_15= 'D:/Temp/PhenoRice/Processing/Validation_Ermes/Input/ES_Static_info_2015.shp'
es_raster_file_15 = 'Y:/ermes/datasets/rs_products/Phenology/ES/Outputs/v4.0/2015/raster/old_min_identification_Full_Out_2015.dat'
es_2km_tif_15 = 'Y:/ermes/datasets/rs_products/Phenology/ES/Outputs/v4.0/ERMES_Grid/TIFFS/2015/MinDoys/es_Phenology_MinDoys_2015_004.tif'

es_shape_file_14 = 'D:/Temp/PhenoRice/Processing/Validation_Ermes/Input/ES_Static_info_2014.shp'
es_raster_file_14 = 'Y:/ermes/datasets/rs_products/Phenology/ES/Outputs/v1.0/2014/raster/old_min_identification_Full_Out_2014.dat'
es_2km_tif_14 = 'Y:/ermes/datasets/rs_products/Phenology/ES/Outputs/v1.0/ERMES_Grid/TIFFS/2014/MinDoys/es_Phenology_MinDoys_2014_001.tif'

# es_grid = 'Y:/ermes/datasets/ERMES_Folder_Structure/ES/Regional/es_Reference_Grid/es_ERMES_Regional_Grid.shp'

es_shape_15 = readOGR (dirname(es_shape_file_15), file_path_sans_ext(basename(es_shape_file_15)))
es_rast_15 = raster(es_raster_file_15, band = 2)
raster::values(es_rast_15)[which(raster::values(es_rast_15) > 180)] =NA
raster::values(es_rast_15)[which(raster::values(es_rast_15) == 0)] =NA

es_2km_15 = raster(es_2km_tif_15)
# es_grid = readOGR(dirname(es_grid), file_path_sans_ext(basename(es_grid)))
NAvalue(es_rast_15) = 0
es_ras_fields_15 = extract(es_rast_15,es_shape_15,weights=TRUE,fun = mean, small = T,  na.rm = T, df = T)
save(es_ras_fields_15, file = 'd:/temp/phenorice/processing/Validation_Ermes/es_15.RData')

# ras_fields_stdev = extract(in_rast,in_shape,fun = sd, na.rm = T, df = T)
es_ras_fields_15$int_id = es_shape_15@data$int_id
joined_15_es = join(es_shape_15@data, es_ras_fields_15, type = 'left')
names(joined_15_es)[17] = 'Sow_MOD'
joined_15_es$Sow_MOD[which(joined_15_es$Sow_MOD == 0)] = NA
joined_15_es$diff = joined_15_es$Sow_MOD - joined_15_es$sowing_doy

sub_15_es = droplevels(subset(joined_15_es, crop_type =='Rice' & is.na(sowing_doy) ==FALSE))
joinedmelt_15_es = melt(sub_15_es, measure.vars = c("sowing_doy","Sow_MOD","diff"))

p = ggplot(joinedmelt_15_es, aes(x = sowing_met ,y = value, fill = variable ))
p = p + geom_boxplot()
p

es_shape_14 = readOGR (dirname(es_shape_file_14), file_path_sans_ext(basename(es_shape_file_14)))
es_rast_14 = raster(es_raster_file_14, band = 2)
raster::values(es_rast_14)[which(raster::values(es_rast_14) > 180)] =NA
raster::values(es_rast_14)[which(raster::values(es_rast_14) == 0)] =NA
es_2km_14 = raster(es_2km_tif_14)
NAvalue(es_rast_14) = 0

es_ras_fields_14 = extract(es_rast_14,es_shape_14,weights=TRUE,fun = mean,na.rm = T, df = T)
save(es_ras_fields_14, file = 'd:/temp/phenorice/processing/Validation_Ermes/es_14.RData')
# ras_fields_stdev_14 = extract(es_rast,es_shape_14,fun = sd, na.rm = T, df = T)
es_ras_fields_14$int_id = es_shape_14@data$int_id

es_joined_14 = join(es_shape_14@data, es_ras_fields_14, type = 'left')
names(es_joined_14)[17] = 'Sow_MOD'
es_joined_14$Sow_MOD[which(es_joined_14$Sow_MOD == 0)] = NA
es_joined_14$diff = es_joined_14$Sow_MOD - es_joined_14$sowing_doy

sub_14_es = droplevels(subset(es_joined_14, crop_type =='Rice' & is.na(sowing_doy) ==FALSE & Sow_MOD !=0))
# levels(sub_14$sowing_met) = c("Dry",   "Water","Unknown")
# sub_14$sowing_met[which(is.na(sub_14$sowing_met))] = 'Unknown'
# #sub_14$sowing_met[which(sub_14$sowing_met == 'Unknown' )] = 'Water'
# sub_14 = droplevels(subset(sub_14, sowing_met !='Unknown'))
# sub_14$sowing_met[which(sub_14$sowing_met == 'Unknown' & sub_14$sowing_doy < 120)] = 'Dry'
# sub_14$sowing_met[which(sub_14$sowing_met == 'Unknown' & sub_14$sowing_doy >= 120)] = 'Water'
joinedmelt_14_es = melt(sub_14_es, measure.vars = c("sowing_doy","Sow_MOD","diff"))

p = ggplot(joinedmelt_14_es, aes(x = sowing_met ,y = value, fill = variable ))
p = p + geom_boxplot()
p

sub_15_es$year = 2015
sub_14_es$year = 2014
sub_tot_es = rbind(sub_15_es ,sub_14_es)
joinedmelt_15_es$year = 2015
joinedmelt_14_es$year = 2014
joinedmelt_tot_es = rbind(joinedmelt_15_es, joinedmelt_14_es)

es_or = sub_tot_es
es_or_melt  = joinedmelt_tot_es
save(es_or_melt,es_or, file = 'd:/temp/phenorice/processing/Validation_Ermes/ES/ES_val_or.RData')

std <- function(x) sd(x)/sqrt(length(x))
stats = ddply(subset(es_or_melt, area > 0), .(sowing_met,variable,year) ,summarize, count = length(value), avg = mean(value, na.rm = T), stdev = sd(value, na.rm = T), mae = mean(abs(value),na.rm = T),sderr = std (value))

stats = ddply(es_or_melt, .(sowing_met,variable) ,summarize, count = length(value),avg = mean(value, na.rm = T), stdev = sd(value, na.rm = T), mae = mean(abs(value),na.rm = T),sderr = std (value))

stats_es = ddply(joinedmelt_tot_es, .(sowing_met,variable,year) ,summarize, avg = mean(value, na.rm = T), stdev = sd(value, na.rm = T), mae = mean(abs(value),na.rm = T))

stats_es = ddply(joinedmelt_tot_es, .(sowing_met,variable) ,summarize, avg = mean(value, na.rm = T), stdev = sd(value, na.rm = T), mae = mean(abs(value),na.rm = T))

p = ggplot(droplevels(subset(es_or_melt, variable !='diff')), aes(x = sowing_met ,y = value, fill = variable ))
p = p + geom_boxplot(outlier.colour = 'transparent')+ylim(80,160)+geom_jitter()+facet_wrap(~year)

p = ggplot(droplevels(subset(es_or_melt, variable !='diff')), aes(x = value, fill = variable ))
p = p+geom_histogram(binwidth = 1, fill = 'transparent')+facet_grid(sowing_met~variable)+theme_bw()
p = p + geom_freqpoly(binwidth = 8)+xlim(80,160)#+facet_grid(~sowing_met)+theme_bw()+xlim(80)
#p = p+geom_boxplot(adjust = 1.4)+facet_grid(variable~sowing_met)+theme_bw()+geom_jitter()
p = p+geom_density(alpha = 0.2, adjust = 2)+facet_grid(~sowing_met)+theme_bw()

p

