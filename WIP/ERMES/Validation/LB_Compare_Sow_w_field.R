
###############################################################################
library(plyr)
library(ggplot2)
library(reshape2)
library(tools)
library(rgdal)
library(raster)
library(data.table)
library(gdalUtils)

it_shape_file_16 = "/media/projects/ermes/datasets/Field_data/2016/Italy/Static_info/IT_Field_data_static_2016_20161219.shp"
it_raster_file_16 = '/media/projects/ermes/datasets/rs_products/Phenology/IT/2016/v1.0/Outputs/2016/Phenorice_IT_2016.dat'
it_2km_tif_16 = '/media/projects/ermes/datasets/rs_products/Phenology/IT/2016/v1.0/Outputs/ERMES_Grid/TIFFS/2016/MinDoys/IT_Phenology_MinDoys_2016_203.tif'

it_shape_file_15 = '/media/projects/ermes/datasets/Field_data/2015/Italy/IT_Static_info/IT_Field_data_static_2015_20161222.shp'
it_raster_file_15 = '/media/projects/ermes/datasets/rs_products/Phenology/IT/2016/v1.0/Outputs/2015/Phenorice_IT_2015.dat'
it_2km_tif_15 = '/media/projects/ermes/datasets/rs_products/Phenology/IT/2016/v1.0/Outputs/ERMES_Grid/TIFFS/2015/MinDoys/IT_Phenology_MinDoys_2015.tif'

it_shape_file_14 = '/media/projects/ermes/datasets/Field_data/2014/Italy/IT Static info/IT_Field_data_static_2014_20161222.shp'
it_raster_file_14 = '/media/projects/ermes/datasets/rs_products/Phenology/IT/2016/v1.0/Outputs/2014/Phenorice_IT_2014.dat'
it_2km_tif_14 = '/media/projects/ermes/datasets/rs_products/Phenology/IT/2016/v1.0/Outputs/ERMES_Grid/TIFFS/2014/MinDoys/IT_Phenology_MinDoys_2014.tif'

out_folder_gen = "/media/projects/ermes/datasets/rs_products/Phenology/Validation/2016"
dir.create(out_folder_gen, recursive = TRUE)
out_folder = file.path(out_folder_gen, "ES")
dir.create(out_folder, recursive = TRUE)
Accessory_folder = file.path(out_folder_gen, 'Accessory')
dir.create(Accessory_folder, recursive = TRUE)

it_grid = '/media/projects/ermes/datasets/rs_products/Phenology/Validation/2016/IT/Accessory/IT_ERMES_Regional_Grid.shp'


it_shape_16 = readOGR (dirname(it_shape_file_16), file_path_sans_ext(basename(it_shape_file_16)))
it_shape_16@data$user_id = NULL
it_rast_16 = raster(it_raster_file_16, band = 2)
it_2km_16 = raster(it_2km_tif_16)
it_grid_sp = readOGR(dirname(it_grid), file_path_sans_ext(basename(it_grid)))

raster::values(it_rast_16)[which(raster::values(it_rast_16) > 180)] =NA
raster::values(it_rast_16)[which(raster::values(it_rast_16) == 0)] =NA

it_ras_fields_16 = extract(it_rast_16,it_shape_16,weights=TRUE,fun = mean, small = T,  na.rm = T, df = T)

save(it_ras_fields_16, file = '/media/projects/ermes/datasets/rs_products/Phenology/Validation/2016/IT/IT_16.RData')
#load('d:/temp/phenorice/processing/Validation_Ermes/IT_16.RData')
# ras_fields_stdev = extract(in_rast,in_shape,fun = sd, na.rm = T, df = T)
it_ras_fields_16$parcel_id = it_shape_16@data$parcel_id
joined_16 = join(it_shape_16@data, it_ras_fields_16, type = 'left')
names(joined_16)[17] = 'Sow_MOD'
joined_16$sowing_doy = as.numeric(as.character(joined_16$sowing_doy))
joined_16$Sow_MOD[which(joined_16$Sow_MOD == 0)] = NA
joined_16$diff = joined_16$Sow_MOD - joined_16$sowing_doy

sub_16 = droplevels(subset(joined_16, crop_type =='Rice' & is.na(sowing_doy) == FALSE ))
levels(sub_16$sowing_met) = c("Dry","Water","Unknown")
sub_16$sowing_met[which(is.na(sub_16$sowing_met))] = 'Unknown'
sub_16 = droplevels(subset(sub_16, sowing_met !='Unknown'))
# sub_16$sowing_met[which(sub_16$sowing_met == 'Unknown' & sub$sowing_doy < 120)] = 'Dry'
# sub_16$sowing_met[which(sub_16$sowing_met == 'Unknown' & sub$sowing_doy >= 120)] = 'Water'
joinedmelt_16 = melt(sub_16, measure.vars = c("sowing_doy","Sow_MOD","diff"))




it_shape_15 = readOGR (dirname(it_shape_file_15), file_path_sans_ext(basename(it_shape_file_15)))
it_rast_15 = raster(it_raster_file_15, band = 2)
it_2km_15 = raster(it_2km_tif_15)
it_grid_sp = readOGR(dirname(it_grid), file_path_sans_ext(basename(it_grid)))

raster::values(it_rast_15)[which(raster::values(it_rast_15) > 180)] =NA
raster::values(it_rast_15)[which(raster::values(it_rast_15) == 0)] =NA

it_ras_fields_15 = raster::extract(it_rast_15,it_shape_15,weights=TRUE,fun = mean, small = T,  na.rm = T, df = T)

save(it_ras_fields_15, file = '/media/projects/ermes/datasets/rs_products/Phenology/Validation/2016/IT/IT_15.RData')
#load('d:/temp/phenorice/processing/Validation_Ermes/IT_15.RData')
# ras_fields_stdev = extract(in_rast,in_shape,fun = sd, na.rm = T, df = T)
it_ras_fields_15$parcel_id = it_shape_15@data$parcel_id
joined_15 = join(it_shape_15@data, it_ras_fields_15, type = 'left')
names(joined_15)[17] = 'Sow_MOD'
joined_15$sowing_doy = as.numeric(as.character(joined_15$sowing_doy))
joined_15$Sow_MOD[which(joined_15$Sow_MOD == 0)] = NA
joined_15$diff = joined_15$Sow_MOD - joined_15$sowing_doy

sub_15 = droplevels(subset(joined_15, crop_type =='Rice' & is.na(sowing_doy) == FALSE ))
levels(sub_15$sowing_met) = c("Dry","Water","Unknown")
sub_15$sowing_met[which(is.na(sub_15$sowing_met))] = 'Unknown'
sub_15 = droplevels(subset(sub_15, sowing_met !='Unknown'))
# sub_15$sowing_met[which(sub_15$sowing_met == 'Unknown' & sub$sowing_doy < 120)] = 'Dry'
# sub_15$sowing_met[which(sub_15$sowing_met == 'Unknown' & sub$sowing_doy >= 120)] = 'Water'
joinedmelt_15 = melt(sub_15, measure.vars = c("sowing_doy","Sow_MOD","diff"))


it_shape_14 = readOGR (dirname(it_shape_file_14), file_path_sans_ext(basename(it_shape_file_14)))
it_rast_14 = raster(it_raster_file_14, band = 2)
it_2km_14 = raster(it_2km_tif_14)
raster::values(it_rast_14)[which(raster::values(it_rast_14) > 180)] =NA
raster::values(it_rast_14)[which(raster::values(it_rast_14) == 0)] =NA

it_ras_fields_14 = extract(it_rast_14,it_shape_14,weights=TRUE, fun = mean,na.rm = T, df = T)
save(it_ras_fields_14, file = '/media/projects/ermes/datasets/rs_products/Phenology/Validation/2016/IT/IT_14.RData')
# ras_fields_stdev_14 = extract(it_rast,it_shape_14,fun = sd, na.rm = T, df = T)
load('/media/projects/ermes/datasets/rs_products/Phenology/Validation/2016/IT/IT_14.RData')
it_ras_fields_14$parcel_id = (it_shape_14@data$parcel_id)

it_joined_14 = join(it_shape_14@data, it_ras_fields_14, type = 'left')
names(it_joined_14)[17] = 'Sow_MOD'
it_joined_14$Sow_MOD[which(it_joined_14$Sow_MOD == 0)] = NA
it_joined_14$sowing_doy = as.numeric(as.character(it_joined_14$sowing_doy))
it_joined_14$diff = it_joined_14$Sow_MOD - it_joined_14$sowing_doy


sub_14 = droplevels(subset(it_joined_14, crop_type == 'Rice' & is.na(sowing_doy) == FALSE ))
levels(sub_14$sowing_met) = c("Dry",   "Water","Unknown")
sub_14$sowing_met[which(is.na(sub_14$sowing_met))] = 'Unknown'
#sub_14$sowing_met[which(sub_14$sowing_met == 'Unknown' )] = 'Water'
sub_14 = droplevels(subset(sub_14, sowing_met !='Unknown'))
#  sub_14$sowing_met[which(sub_14$sowing_met == 'Unknown' & sub_14$sowing_doy < 120)] = 'Dry'
#  sub_14$sowing_met[which(sub_14$sowing_met == 'Unknown' & sub_14$sowing_doy >= 120)] = 'Water'
joinedmelt_14 = melt(sub_14, measure.vars = c("sowing_doy","Sow_MOD","diff"))

#2016

it_shape_16 = readOGR (dirname(it_shape_file_16), file_path_sans_ext(basename(it_shape_file_16)))
it_rast_16 = raster(it_raster_file_16, band = 2)
it_2km_16 = raster(it_2km_tif_16)
raster::values(it_rast_16)[which(raster::values(it_rast_16) > 180)] =NA
raster::values(it_rast_16)[which(raster::values(it_rast_16) == 0)] =NA

it_ras_fields_16 = raster::extract(it_rast_16,it_shape_16,weights=TRUE,fun = mean,na.rm = T, df = T)
save(it_ras_fields_16, file = 'Z:/Validation/2016/IT/IT_16.RData')
# ras_fields_stdev_16 = extract(it_rast,it_shape_16,fun = sd, na.rm = T, df = T)
load('Z:/Validation/2016/IT/IT_16.RData')
it_ras_fields_16$parcel_id = it_shape_16@data$parcel_id

it_joined_16 = join(it_shape_16@data, it_ras_fields_16, type = 'left')
it_joined_16$user_id = NULL
names(it_joined_16)[17] = 'Sow_MOD'
it_joined_16$Sow_MOD[which(it_joined_16$Sow_MOD == 0)] = NA
it_joined_16$sowing_doy = as.numeric(as.character(it_joined_16$sowing_doy))
it_joined_16$diff = it_joined_16$Sow_MOD - it_joined_16$sowing_doy

sub_16 = droplevels(subset(it_joined_16, crop_type =='Rice' & is.na(sowing_doy) == FALSE ))
levels(sub_16$sowing_met) = c("Dry",   "Water","Unknown")
sub_16$sowing_met[which(is.na(sub_16$sowing_met))] = 'Unknown'
#sub_16$sowing_met[which(sub_16$sowing_met == 'Unknown' )] = 'Water'
sub_16 = droplevels(subset(sub_16, sowing_met !='Unknown'))
#  sub_16$sowing_met[which(sub_16$sowing_met == 'Unknown' & sub_16$sowing_doy < 120)] = 'Dry'
#  sub_16$sowing_met[which(sub_16$sowing_met == 'Unknown' & sub_16$sowing_doy >= 120)] = 'Water'
joinedmelt_16 = melt(sub_16, measure.vars = c("sowing_doy","Sow_MOD","diff"))


p = ggplot(joinedmelt_14, aes(x = sowing_met ,y = value, fill = variable ))
p = p + geom_boxplot()
p

sub_16$year = 2016
sub_15$year = 2015
sub_14$year = 2014
sub_tot = rbind(sub_16, sub_15, sub_14)
sub_tot = droplevels(subset(sub_tot, !is.na(diff)))
joinedmelt_16$year = 2016
joinedmelt_15$year = 2015
joinedmelt_14$year = 2014
joinedmelt_tot = rbind(joinedmelt_16,joinedmelt_15, joinedmelt_14)
joinedmelt_tot = melt(sub_tot, measure.vars = c("sowing_doy","Sow_MOD","diff"))

it_or = sub_tot
it_or_melt  = joinedmelt_tot
save(it_or_melt,it_or, file = '/media/projects/ermes/datasets/rs_products/Phenology/Validation/2016/IT/IT_val_or.RData')

#END -----



std <- function(x) sd(x)/sqrt(length(x))
stats = ddply(subset(it_or_melt, area > 2), .(sowing_met,variable,year) ,summarize, count = length(value), avg = mean(value, na.rm = T), stdev = sd(value, na.rm = T), mae = mean(abs(value),na.rm = T),sderr = std (value))

stats = ddply(it_or_melt, .(sowing_met,variable) ,summarize, count = length(value),avg = mean(value, na.rm = T), stdev = sd(value, na.rm = T), mae = mean(abs(value),na.rm = T),sderr = std (value))

p = ggplot(droplevels(subset(joinedmelt_tot, variable !='diff'& area > 1)), aes(x = sowing_met ,y = value, fill = variable ))
p = p + geom_violin()+geom_jitter()+facet_wrap(~year)
p

p = ggplot(droplevels(subset(it_or_melt, variable !='diff'& area > 1)), aes(x = value, fill = variable ))
p =p+ stat_bin(binwidth = 8, aes(y=..count../sum(..count..)))+facet_grid(sowing_met~variable)+theme_bw()
p = p+geom_histogram(binwidth = 7)+facet_grid(sowing_met~variable)+theme_bw()
p = p + geom_freqpoly(binwidth = 7)+facet_grid(~sowing_met)+theme_bw()
#p = p+geom_boxplot(adjust = 1.4)+facet_grid(variable~sowing_met)+theme_bw()+geom_jitter()
p = p+geom_density(alpha = 0.2, adjust = 1.1)+facet_grid(~sowing_met)+theme_bw()

p
#
# sub = droplevels(subset(joined, crop_type =='Rice' ))
# joinedmelt = melt(sub, measure.vars = c("sowing_doy","Sow_MOD"))
# levels(joinedmelt$variable)
#
# sub = droplevels(subset(joinedmelt, crop_type =='Rice' & sowing_met == 'Water' & value > 115))
#
# p = ggplot(joinedmelt, aes(x = sowing_met ,y = value, fill = variable ))
# p = p + geom_violin()
# qplot(joinedmelt$sowing_met, joinedmelt$value, geom = 'boxplot',  = 'variable' )
# x11()
#
#
# twok_fields = extract(in_2km,in_shape,weights=TRUE,fun = mean, na.rm = T, df = T)
# 2k_fields_stdev = extract(in_rast,in_shape,fun = sd, na.rm = T, df = T)
# twok_fields$int_id = in_shape@data$int_id
#
# joined2k = join(in_shape@data, twok_fields, type = 'left')
# names(joined2k)[17] = 'Sow_MOD'
# joined2k$Sow_MOD[which(joined$Sow_MOD == 0)] = NA
#
# sub = droplevels(subset(joined2k, crop_type =='Rice'))
# levels(sub$sowing_met) = c("Dry",   "Water","Unknown")
# sub$sowing_met[which(is.na(sub$sowing_met))] = 'Unknown'
# sub = droplevels(subset(sub, sowing_met != 'Dry' & sowing_doy >105))
# joinedmelt = melt(sub, measure.vars = c("sowing_doy","Sow_MOD"))
# qplot(joinedmelt$variable, joinedmelt$value, geom = 'boxplot')
#
