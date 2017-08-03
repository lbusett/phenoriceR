
###############################################################################
library(plyr)
library(ggplot2)
library(reshape2)
library(tools)
library(rgdal)
library(raster)
library(data.table)
library(gdalUtils)

gr_shape_file_16 = "/media/projects/ermes/datasets/Field_data/2016/Greece/Static_info/GR_Field_data_static_2016_20161205.shp"
gr_raster_file_16 = '/media/projects/ermes/datasets/rs_products/Phenology/GR/2016/v1.0/Outputs/2016/Phenorice_GR_2016.dat'
gr_2km_tif_16 = '/media/projects/ermes/datasets/rs_products/Phenology/GR/2016/v1.0/Outputs/ERMES_Grid/TIFFS/2016/MinDoys/GR_Phenology_MinDoys_2016_203.tif'

gr_shape_file_15 = '/media/projects/ermes/datasets/Field_data/2015/Greece/GR_Static_info/GR_Field_data_static_2015_20161222.shp'
gr_raster_file_15 = '/media/projects/ermes/datasets/rs_products/Phenology/GR/2016/v1.0/Outputs/2015/Phenorice_GR_2015.dat'
gr_2km_tif_15 = '/media/projects/ermes/datasets/rs_products/Phenology/GR/2016/v1.0/Outputs/ERMES_Grid/TIFFS/2015/MinDoys/GR_Phenology_MinDoys_2015.tif'

gr_shape_file_14 = '/media/projects/ermes/datasets/Field_data/2014/Greece/GR Static info/GR_Field_data_static_2014_20161222.shp'
gr_raster_file_14 = '/media/projects/ermes/datasets/rs_products/Phenology/GR/2016/v1.0/Outputs/2014/Phenorice_GR_2014.dat'
gr_2km_tif_14 = '/media/projects/ermes/datasets/rs_products/Phenology/GR/2016/v1.0/Outputs/ERMES_Grid/TIFFS/2014/MinDoys/GR_Phenology_MinDoys_2014.tif'

out_folder_gen = "/media/projects/ermes/datasets/rs_products/Phenology/Validation/2016"
dir.create(out_folder_gen, recursive = TRUE)
out_folder = file.path(out_folder_gen, "GR")
dir.create(out_folder, recursive = TRUE)
Accessory_folder = file.path(out_folder_gen, 'Accessory')
dir.create(Accessory_folder, recursive = TRUE)

gr_grid = '/media/projects/ermes/datasets/rs_products/Phenology/Validation/2016/GR/Accessory/GR_ERMES_Regional_Grid.shp'


gr_shape_16 = readOGR (dirname(gr_shape_file_16), file_path_sans_ext(basename(gr_shape_file_16)))
gr_shape_16@data$user_id = NULL
gr_rast_16 = raster(gr_raster_file_16, band = 2)
gr_2km_16 = raster(gr_2km_tif_16)
gr_grid_sp = readOGR(dirname(gr_grid), file_path_sans_ext(basename(gr_grid)))

raster::values(gr_rast_16)[which(raster::values(gr_rast_16) > 180)] =NA
raster::values(gr_rast_16)[which(raster::values(gr_rast_16) == 0)] =NA

gr_ras_fields_16 = extract(gr_rast_16,gr_shape_16,weights=TRUE,fun = mean, small = T,  na.rm = T, df = T)

save(gr_ras_fields_16, file = '/media/projects/ermes/datasets/rs_products/Phenology/Validation/2016/GR/gr_16.RData')
#load('d:/temp/phenorice/processing/Validation_ERMES/gr_16.RData')
# ras_fields_stdev = extract(in_rast,in_shape,fun = sd, na.rm = T, df = T)
gr_ras_fields_16$parcel_id = gr_shape_16@data$parcel_id
joined_16 = join(gr_shape_16@data, gr_ras_fields_16, type = 'left')
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




gr_shape_15 = readOGR (dirname(gr_shape_file_15), file_path_sans_ext(basename(gr_shape_file_15)))
gr_rast_15 = raster(gr_raster_file_15, band = 2)
gr_2km_15 = raster(gr_2km_tif_15)
gr_grid_sp = readOGR(dirname(gr_grid), file_path_sans_ext(basename(gr_grid)))

raster::values(gr_rast_15)[which(raster::values(gr_rast_15) > 180)] =NA
raster::values(gr_rast_15)[which(raster::values(gr_rast_15) == 0)] =NA

gr_ras_fields_15 = extract(gr_rast_15,gr_shape_15,weights=TRUE,fun = mean, small = T,  na.rm = T, df = T)

save(gr_ras_fields_15, file = '/media/projects/ermes/datasets/rs_products/Phenology/Validation/2016/GR/gr_15.RData')
#load('d:/temp/phenorice/processing/Validation_ERMES/gr_15.RData')
# ras_fields_stdev = extract(in_rast,in_shape,fun = sd, na.rm = T, df = T)
gr_ras_fields_15$parcel_id = gr_shape_15@data$parcel_id
joined_15 = join(gr_shape_15@data, gr_ras_fields_15, type = 'left')
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


gr_shape_14 = readOGR (dirname(gr_shape_file_14), file_path_sans_ext(basename(gr_shape_file_14)))
gr_rast_14 = raster(gr_raster_file_14, band = 2)
gr_2km_14 = raster(gr_2km_tif_14)
raster::values(gr_rast_14)[which(raster::values(gr_rast_14) > 180)] =NA
raster::values(gr_rast_14)[which(raster::values(gr_rast_14) == 0)] =NA

gr_ras_fields_14 = extract(gr_rast_14,gr_shape_14,weights=TRUE, fun = mean,na.rm = T, df = T)
save(gr_ras_fields_14, file = '/media/projects/ermes/datasets/rs_products/Phenology/Validation/2016/GR/gr_14.RData')
# ras_fields_stdev_14 = extract(gr_rast,gr_shape_14,fun = sd, na.rm = T, df = T)
load('/media/projects/ermes/datasets/rs_products/Phenology/Validation/2016/GR/gr_14.RData')
gr_ras_fields_14$parcel_id = (gr_shape_14@data$parcel_id)

gr_joined_14 = join(gr_shape_14@data, gr_ras_fields_14, type = 'left')
names(gr_joined_14)[17] = 'Sow_MOD'
gr_joined_14$Sow_MOD[which(gr_joined_14$Sow_MOD == 0)] = NA
gr_joined_14$sowing_doy = as.numeric(as.character(gr_joined_14$sowing_doy))
gr_joined_14$diff = gr_joined_14$Sow_MOD - gr_joined_14$sowing_doy

sub_14 = droplevels(subset(gr_joined_14, crop_type =='Rice' & is.na(sowing_doy) ==FALSE & Sow_MOD !=0 ))
levels(sub_14$sowing_met) = c("Dry",   "Water","Unknown")
sub_14$sowing_met[which(is.na(sub_14$sowing_met))] = 'Unknown'
#sub_14$sowing_met[which(sub_14$sowing_met == 'Unknown' )] = 'Water'
sub_14 = droplevels(subset(sub_14, sowing_met !='Unknown'))
#  sub_14$sowing_met[which(sub_14$sowing_met == 'Unknown' & sub_14$sowing_doy < 120)] = 'Dry'
#  sub_14$sowing_met[which(sub_14$sowing_met == 'Unknown' & sub_14$sowing_doy >= 120)] = 'Water'
joinedmelt_14 = melt(sub_14, measure.vars = c("sowing_doy","Sow_MOD","diff"))

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

gr_or = sub_tot
gr_or_melt  = joinedmelt_tot
save(gr_or_melt,gr_or, file = '/media/projects/ermes/datasets/rs_products/Phenology/Validation/2016/GR/gr_val_or.RData')

#END -----



std <- function(x) sd(x)/sqrt(length(x))
stats = ddply(subset(gr_or_melt, area > 2), .(sowing_met,variable,year) ,summarize, count = length(value), avg = mean(value, na.rm = T), stdev = sd(value, na.rm = T), mae = mean(abs(value),na.rm = T),sderr = std (value))

stats = ddply(gr_or_melt, .(sowing_met,variable) ,summarize, count = length(value),avg = mean(value, na.rm = T), stdev = sd(value, na.rm = T), mae = mean(abs(value),na.rm = T),sderr = std (value))

p = ggplot(droplevels(subset(joinedmelt_tot, variable !='diff'& area > 1)), aes(x = sowing_met ,y = value, fill = variable ))
p = p + geom_violin()+geom_jitter()+facet_wrap(~year)
p

p = ggplot(droplevels(subset(gr_or_melt, variable !='diff'& area > 1)), aes(x = value, fill = variable ))
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
