
###############################################################################
library(plyr)
library(ggplot2)
library(reshape2)
library(tools)
library(rgdal)
library(raster)
library(data.table)

gr_shape_file_15= 'D:/Temp/PhenoRice/Processing/Validation_Ermes/Input/GR_Static_info_2015.shp'
gr_raster_file_15 = 'Y:/ermes/datasets/rs_products/Phenology/GR/Outputs/v4.0/2015/raster/old_min_identification_Full_Out_2015.dat'
gr_2km_tif_15 = 'Y:/ermes/datasets/rs_products/Phenology/GR/Outputs/v4.0/ERMES_Grid/TIFFS/2015/MinDoys/gr_Phenology_MinDoys_2015_004.tif'

gr_shape_file_14 = 'D:/Temp/PhenoRice/Processing/Validation_Ermes/Input/GR_Static_info_2014.shp'
gr_raster_file_14 = 'Y:/ermes/datasets/rs_products/Phenology/GR/Outputs/v1.0/2014/raster/old_min_identification_Full_Out_2014.dat'
gr_2km_tif_14 = 'Y:/ermes/datasets/rs_products/Phenology/GR/Outputs/v1.0/ERMES_Grid/TIFFS/2014/MinDoys/gr_Phenology_MinDoys_2014_001.tif'

# in_grid = 'Y:/ermes/datasets/ERMES_Folder_Structure/GR/Regional/es_Reference_Grid/gr_ERMES_Regional_Grid.shp'

gr_shape_15 = readOGR (dirname(gr_shape_file_15), file_path_sans_ext(basename(gr_shape_file_15)))
gr_rast_15 = raster(gr_raster_file_15, band = 2)
gr_2km_15 = raster(gr_2km_tif_15)
raster::values(gr_rast_15)[which(raster::values(gr_rast_15) > 180)] =NA
raster::values(gr_rast_15)[which(raster::values(gr_rast_15) <= 97)] =NA

# gr_grid = readOGR(dirname(gr_grid), file_path_sans_ext(basename(gr_grid)))
NAvalue(gr_rast_15) = 0
gr_ras_fields_15 = extract(gr_rast_15,gr_shape_15,weights=TRUE,fun = mean, small = T,  na.rm = T, df = T)
save(gr_ras_fields_15, file = 'd:/temp/phenorice/processing/Validation_Ermes/gr_15.RData')

# ras_fields_stdev = extract(in_rast,in_shape,fun = sd, na.rm = T, df = T)
gr_ras_fields_15$int_id = gr_shape_15@data$int_id
joined_15_gr = join(gr_shape_15@data, gr_ras_fields_15, type = 'left')
names(joined_15_gr)[17] = 'Sow_MOD'
joined_15_gr$Sow_MOD[which(joined_15_gr$Sow_MOD == 0)] = NA
joined_15_gr$diff = joined_15_gr$Sow_MOD - joined_15_gr$sowing_doy

sub_15_gr = droplevels(subset(joined_15_gr, crop_type =='Rice' & is.na(sowing_doy) ==FALSE))
# levels(sub_15_gr$sowing_met) = c("Dry","Water","Unknown")
# sub_15_gr$sowing_met[which(is.na(sub_15_gr$sowing_met))] = 'Unknown'
# sub_15_gr = droplevels(subset(sub_15_gr, sowing_met !='Unknown'))
# sub_15_gr$sowing_met[which(sub_15_gr$sowing_met == 'Unknown' & sub_gr$sowing_doy < 120)] = 'Dry'
# sub_15_gr$sowing_met[which(sub_15_gr$sowing_met == 'Unknown' & sub_gr$sowing_doy >= 120)] = 'Water'
joinedmelt_15_gr = melt(sub_15_gr, measure.vars = c("sowing_doy","Sow_MOD","diff"))

p = ggplot(joinedmelt_15_gr, aes(x = sowing_met ,y = value, fill = variable ))
p = p + geom_boxplot()
p

gr_shape_14 = readOGR (dirname(gr_shape_file_14), file_path_sans_ext(basename(gr_shape_file_14)))
gr_rast_14 = raster(gr_raster_file_14, band = 2)
raster::values(gr_rast_14)[which(raster::values(gr_rast_14) > 180)] =NA
raster::values(gr_rast_14)[which(raster::values(gr_rast_14) <= 97)] =NA
gr_2km_14 = raster(gr_2km_tif_14)
NAvalue(gr_rast_14) = 0

gr_ras_fields_14 = extract(gr_rast_14,gr_shape_14,weights=TRUE,fun = mean,na.rm = T, df = T)
save(gr_ras_fields_14, file = 'd:/temp/phenorice/processing/Validation_Ermes/gr_14.RData')
# ras_fields_stdev_14 = extract(gr_rast,gr_shape_14,fun = sd, na.rm = T, df = T)
gr_ras_fields_14$int_id = gr_shape_14@data$int_id

gr_joined_14 = join(gr_shape_14@data, gr_ras_fields_14, type = 'left')
names(gr_joined_14)[17] = 'Sow_MOD'
gr_joined_14$Sow_MOD[which(gr_joined_14$Sow_MOD == 0)] = NA
gr_joined_14$diff = gr_joined_14$Sow_MOD - gr_joined_14$sowing_doy

sub_14_gr = droplevels(subset(gr_joined_14, crop_type =='Rice' & is.na(sowing_doy) ==FALSE & Sow_MOD !=0))
# levels(sub_14$sowing_met) = c("Dry",   "Water","Unknown")
# sub_14$sowing_met[which(is.na(sub_14$sowing_met))] = 'Unknown'
# #sub_14$sowing_met[which(sub_14$sowing_met == 'Unknown' )] = 'Water'
# sub_14 = droplevels(subset(sub_14, sowing_met !='Unknown'))
# sub_14$sowing_met[which(sub_14$sowing_met == 'Unknown' & sub_14$sowing_doy < 120)] = 'Dry'
# sub_14$sowing_met[which(sub_14$sowing_met == 'Unknown' & sub_14$sowing_doy >= 120)] = 'Water'
joinedmelt_14_gr = melt(sub_14_gr, measure.vars = c("sowing_doy","Sow_MOD","diff"))

p = ggplot(joinedmelt_14_gr, aes(x = sowing_met ,y = value, fill = variable ))
p = p + geom_boxplot()
p

sub_15_gr$year = 2015
sub_14_gr$year = 2014
sub_tot_gr = rbind(sub_15_gr ,sub_14_gr)
joinedmelt_15_gr$year = 2015
joinedmelt_14_gr$year = 2014
joinedmelt_tot_gr = rbind(joinedmelt_15_gr, joinedmelt_14_gr)

gr_or = sub_tot_gr
gr_or_melt  = joinedmelt_tot_gr
save(gr_or_melt,gr_or, file = 'd:/temp/phenorice/processing/Validation_Ermes/GR/GR_val_or.RData')

std <- function(x) sd(x)/sqrt(length(x))
stats = ddply(subset(gr_or_melt, area > 1), .(sowing_met,variable,year) ,summarize, count = length(value), avg = mean(value, na.rm = T), stdev = sd(value, na.rm = T), mae = mean(abs(value),na.rm = T),sderr = std (value))

stats_gr = ddply(joinedmelt_tot_gr, .(sowing_met,variable,year) ,summarize, avg = mean(value, na.rm = T), stdev = sd(value, na.rm = T), mae = mean(abs(value),na.rm = T))

stats_gr = ddply(joinedmelt_tot_gr, .(sowing_met,variable) ,summarize, avg = mean(value, na.rm = T), stdev = sd(value, na.rm = T), mae = mean(abs(value),na.rm = T))

p = ggplot(droplevels(subset(joinedmelt_tot_gr, variable !='diff')), aes(x = sowing_met ,y = value, fill = variable ))
p = p + geom_boxplot(outlier.colour = 'transparent')+geom_jitter()+facet_wrap(~year)

p = ggplot(droplevels(subset(gr_or_melt, variable !='diff')), aes(x = value, fill = variable ))
p = p+geom_histogram(binwidth = 1, fill = 'transparent')+facet_grid(sowing_met~variable)+theme_bw()
p = p + geom_freqpoly(binwidth = 8)+facet_grid(~sowing_met)+theme_bw()
#p = p+geom_boxplot(adjust = 1.4)+facet_grid(variable~sowing_met)+theme_bw()+geom_jitter()
p = p+geom_density(alpha = 0.2, adjust = 2)+facet_grid(~sowing_met)+theme_bw()

p
