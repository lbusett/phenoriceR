library(plyr)
library(ggplot2)
library(reshape2)
library(tools)
library(rgdal)
library(raster)
library(data.table)
library(gdalUtils)

es_shape_file_15 = 'D:/Temp/PhenoRice/Processing/Validation_Ermes/Input/ES_Static_info_2015.shp'
es_raster_file_15 = 'Y:/ermes/datasets/rs_products/Phenology/ES/Outputs/v4.0/2015/raster/old_min_identification_Full_Out_2015.dat'
es_2km_tif_15 = 'Y:/ermes/datasets/rs_products/Phenology/ES/Outputs/v4.0/ERMES_Grid/TIFFS/2015/MinDoys/es_Phenology_MinDoys_2015_004.tif'

es_shape_file_14 = 'D:/Temp/PhenoRice/Processing/Validation_Ermes/Input/ES_Static_info_2014.shp'
es_raster_file_14 = 'Y:/ermes/datasets/rs_products/Phenology/ES/Outputs/v1.0/2014/raster/old_min_identification_Full_Out_2014.dat'
es_2km_tif_14 = 'Y:/ermes/datasets/rs_products/Phenology/ES/Outputs/v1.0/ERMES_Grid/TIFFS/2014/MinDoys/es_Phenology_MinDoys_2014_001.tif'

es_grid_reduced = 'D:/Temp/PhenoRice/Processing/Validation_Ermes/ES/Accessory/ES_ERMES_Regional_Grid.shp'

out_folder = "D:/Temp/PhenoRice/Processing/Validation_Ermes/ES"
Accessory_folder = file.path(out_folder, 'Accessory')

#Inizio calcoli -----

#2014
name_shape_14_sinu = file.path(Accessory_folder, 'Static_info_2014_sinu.shp')
name_rast_14_sinu = file.path(Accessory_folder, 'Static_info_2014_sinu.tiff')

name_shape_15_sinu = file.path(Accessory_folder, 'Static_info_2015_sinu.shp')
name_rast_15_sinu = file.path(Accessory_folder, 'Static_info_2015_sinu.tiff')

es_grid_reduced = file.path(out_folder,'Accessory','ES_ERMES_Regional_Grid.shp')
rast_grid  = file.path(out_folder,'Accessory','es_ERMES_Regional_Grid.tif')
es_grid_sinu = file.path(out_folder,'Accessory','grid_2k_sinu.shp')

es_shape_14 = readOGR (dirname(es_shape_file_14), file_path_sans_ext(basename(es_shape_file_14)))
es_rast_14 = raster(es_raster_file_14, band = 2)
es_2km_14 = raster(es_2km_tif_14)
# es_grid_sp = readOGR(dirname(es_grid_reduced), file_path_sans_ext(basename(es_grid_reduced)))
raster::values(es_rast_14)[which(raster::values(es_rast_14) > 180)] =NA
raster::values(es_rast_14)[which(raster::values(es_rast_14) == 0)] =NA

es_shape_14@data$sowing_met = "Water"


es_grid_2k = readOGR(dsn = dirname(es_grid_reduced),layer = basename(file_path_sans_ext(es_grid_reduced)))
es_grid_25_sinu = spTransform(es_grid_2k,CRS(proj4string(es_rast_14)))
writeOGR(es_grid_25_sinu, dsn = Accessory_folder,layer  ='grid_2k_sinu', driver = 'ESRI Shapefile',overwrite_layer = T)

crop_rast_14 = crop(es_rast_14, extent(es_grid_25_sinu))

es_shape_14 = es_shape_14[!is.na(es_shape_14@data$crop_type == 'Rice'),]
es_shape_14 = es_shape_14[es_shape_14@data$crop_type == 'Rice' &
                            es_shape_14@data$sowing_met != 'Unknown' &
                            !is.na(es_shape_14@data$sowing_doy),]

shape_14_sinu = spTransform(es_shape_14,CRS(proj4string(crop_rast_14)))

writeOGR(shape_14_sinu, dsn = Accessory_folder,layer  = basename(file_path_sans_ext(name_shape_14_sinu)), driver = 'ESRI Shapefile',overwrite_layer = T )

raster_obs_14 = gdal_rasterize(name_shape_14_sinu, name_rast_14_sinu,a = "sowing_doy", output_Raster = T, ot = 'UInt32',tr = c(15,15), tap = T, of = 'GTiff')

NAvalue(raster_obs_14) = 0

pro_142k = ZonalPipe(es_grid_sinu,crop_rast_14,rast_grid,'pippo',
                     'int_id', stat="mean", cut = T)

proall_142k = ZonalPipe(es_grid_sinu,raster_obs_14,rast_grid, 'pippo',
                        'int_id', stat="mean", cut = F)

proallcount_142k = ZonalPipe(es_grid_sinu,raster_obs_14,rast_grid, 'pippo',
                             'int_id', stat="mean", cut = F)


#2015

es_shape_15 = readOGR (dirname(es_shape_file_15), file_path_sans_ext(basename(es_shape_file_15)))
es_rast_15 = raster(es_raster_file_15, band = 2)
es_2km_15 = raster(es_2km_tif_15)
raster::values(es_rast_15)[which(raster::values(es_rast_15) > 180)] =NA
raster::values(es_rast_15)[which(raster::values(es_rast_15) == 0)] =NA

es_shape_15@data$sowing_met = "Water"
crop_rast_15 = crop(es_rast_15, extent(es_grid_25_sinu))

es_shape_15 = es_shape_15[!is.na(es_shape_15@data$crop_type == 'Rice'),]
es_shape_15 = es_shape_15[es_shape_15@data$crop_type == 'Rice' &
                            es_shape_15@data$sowing_met != 'Unknown' &
                            !is.na(es_shape_15@data$sowing_doy),]

shape_15_sinu = spTransform(es_shape_15,CRS(proj4string(crop_rast_15)))

writeOGR(shape_15_sinu, dsn = Accessory_folder,layer  = basename(file_path_sans_ext(name_shape_15_sinu)), driver = 'ESRI Shapefile',overwrite_layer = T )

raster_obs_15 = gdal_rasterize(name_shape_15_sinu, name_rast_15_sinu,a = "sowing_doy", output_Raster = T, ot = 'UInt32',tr = c(15,15), tap = T, of = 'GTiff')
NAvalue(raster_obs_15) = 0

pro_152k = ZonalPipe(es_grid_sinu,crop_rast_15,rast_grid,'pippo',
                     'int_id', stat="mean", cut = T)

proall_152k = ZonalPipe(es_grid_sinu,raster_obs_15,rast_grid, 'pippo',
                        'int_id', stat="mean", cut = F)

proallcount_152k = ZonalPipe(es_grid_sinu,raster_obs_15,rast_grid, 'pippo',
                             'int_id', stat="countna", cut = F)

#Fine calcoli - inizio analisi e plotting ----

protot_142k = join(pro_142k@data, proall_142k@data, by = names(pro_142k@data)[1:10], type = 'left')
protot_142k = join(protot_142k, proallcount_142k@data, by = names(pro_142k@data)[1:10], type = 'left')
names(protot_142k) = c(names(pro_142k@data)[1:10], 'Mod' , 'FieldTot','counttot')
protot_142k_tot = droplevels(subset(protot_142k, !is.nan(Mod) & counttot > 0 ))
profull_142k = protot_142k_tot
profull_142k$difftot = profull_142k$Mod - profull_142k$FieldTot
protot_melt_142k = melt(profull_142k, measure.vars = c('Mod' , 'FieldTot',  'difftot'))



p = ggplot(droplevels(subset(protot_melt_142k, variable !='difftot')), aes(x = value, color = variable ))
# p =p+ stat_bin(binwidth = 8, aes(y=..count../sum(..count..)))+facet_grid(~variable)+theme_bw()
# p = p+geom_histogram(binwidth = 8)+facet_grid(~variable)+theme_bw()
p = p + geom_freqpoly(binwidth = 8) #+facet_grid(~variable)+theme_bw()
#p = p+geom_boxplot(adjust = 1.4)+facet_grid(variable~sowing_met)+theme_bw()+geom_jitter()
p = p+geom_density(alpha = 0.2, adjust = 1) #+facet_grid(~variable)+theme_bw()

p

p = ggplot(droplevels(subset(protot_melt_142k, variable !='difftot')), aes(x = variable, y = value ))
p = p + geom_violin()+theme_bw()   +ylim(50,200)
p

stats_14_2k = ddply(protot_melt_142k, .(variable) ,summarize, count = length(value),avg = mean(value, na.rm = T), stdev = sd(value, na.rm = T), mae = mean(abs(value),na.rm = T))

protot_152k = join(pro_152k@data, proall_152k@data, by = names(pro_152k@data)[1:10], type = 'left')
protot_152k = join(protot_152k, proallcount_152k@data, by = names(pro_152k@data)[1:10], type = 'left')
names(protot_152k) = c(names(pro_152k@data)[1:10], 'Mod' , 'FieldTot','counttot')
protot_152k_tot = droplevels(subset(protot_152k, !is.nan(Mod) & counttot >0 ))

profull_152k = protot_152k_tot
profull_152k$difftot = profull_152k$Mod - profull_152k$FieldTot
protot_melt_152k = melt(profull_152k, measure.vars = c('Mod' , 'FieldTot',  'difftot'))



p = ggplot(droplevels(subset(protot_melt_152k, variable !='difftot')), aes(x = value, color = variable ))
# p =p+ stat_bin(binwidth = 8, aes(y=..count../sum(..count..)))+facet_grid(~variable)+theme_bw()
# p = p+geom_histogram(binwidth = 8)+facet_grid(~variable)+theme_bw()
p = p + geom_freqpoly(binwidth = 8) #+facet_grid(~variable)+theme_bw()
#p = p+geom_boxplot(adjust = 1.4)+facet_grid(variable~sowing_met)+theme_bw()+geom_jitter()
p = p+geom_density(alpha = 0.2, adjust = 1) #+facet_grid(~variable)+theme_bw()

p

p = ggplot(droplevels(subset(protot_melt_152k, variable !='difftot'& variable !='diffwat'& variable !='diffdry')), aes(x = variable, y = value ))
p = p + geom_violin()+theme_bw()   +ylim(50,200)
p

stats_15 = ddply(protot_melt_152k, .(variable) ,summarize, count = length(value),avg = mean(value, na.rm = T), stdev = sd(value, na.rm = T), mae = mean(abs(value),na.rm = T))

protot_melt_14_152k = rbind ( protot_melt_142k,protot_melt_152k)
stats_14_152k = ddply(protot_melt_14_152k, .(variable) ,summarize, count = length(value),avg = mean(value, na.rm = T), stdev = sd(value, na.rm = T), mae = mean(abs(value),na.rm = T))

profull_142k$Year = 2014
profull_152k$Year = 2015

profull_14_152k = rbind(profull_142k,profull_152k)
protot_melt_14_152k = melt(profull_14_152k, measure.vars = c('Mod' , 'FieldTot', 'difftot'))
stats_14_152k = ddply(protot_melt_14_152k, .(variable) ,summarize, count = length(value),avg = mean(value, na.rm = T), stdev = sd(value, na.rm = T), mae = mean(abs(value),na.rm = T))


es_2k = profull_14_152k
es_2k_melt  = protot_melt_14_152k
stats_14_15 = ddply(es_2k_melt, .(variable,Year) ,summarize, count = length(value),avg = mean(value, na.rm = T), stdev = sd(value, na.rm = T), mae = mean(abs(value),na.rm = T))
save(es_2k,es_2k_melt, file = 'd:/temp/phenorice/processing/Validation_Ermes/ES/ES_val_2k.RData')



p = ggplot(droplevels(subset(protot_melt_14_152k, variable !='difftot')), aes(x = variable, y = value ))
p = p + geom_boxplot()+theme_bw()   +ylim(50,200)
p

p = ggplot(droplevels(subset(protot_melt_14_152k, variable !='difftot')), aes(x = value, color = variable ))
p =p+ stat_bin(binwidth = 8, aes(y=..count../sum(..count..)))+facet_grid(~variable)+theme_bw()
p = p+geom_histogram(binwidth =8)+facet_grid(~variable)+theme_bw()
p = p + geom_freqpoly(binwidth = 8) #+facet_grid(~variable)+theme_bw()
#p = p+geom_boxplot(adjust = 1.4)+facet_grid(variable~sowing_met)+theme_bw()+geom_jitter()
p = p+geom_density(alpha = 0.2, adjust = 1) #+facet_grid(~variable)+theme_bw()

p

