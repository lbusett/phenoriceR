library(plyr)
library(ggplot2)
library(reshape2)
library(tools)
library(rgdal)
library(raster)
library(data.table)
library(gdalUtils)

gr_shape_file_15 = 'D:/Temp/PhenoRice/Processing/Validation_Ermes/Input/GR_Static_info_2015.shp'
gr_raster_file_15 = 'Y:/ermes/datasets/rs_products/Phenology/GR/Outputs/v4.0/2015/raster/old_min_identification_Full_Out_2015.dat'
gr_2km_tif_15 = 'Y:/ermes/datasets/rs_products/Phenology/GR/Outputs/v4.0/ERMES_Grid/TIFFS/2015/MinDoys/GR_Phenology_MinDoys_2015_004.tif'

gr_shape_file_14 = 'D:/Temp/PhenoRice/Processing/Validation_Ermes/Input/GR_Static_info_2014.shp'
gr_raster_file_14 = 'Y:/ermes/datasets/rs_products/Phenology/GR/Outputs/v1.0/2014/raster/old_min_identification_Full_Out_2014.dat'
gr_2km_tif_14 = 'Y:/ermes/datasets/rs_products/Phenology/GR/Outputs/v1.0/ERMES_Grid/TIFFS/2014/MinDoys/GR_Phenology_MinDoys_2014_001.tif'

gr_grid_reduced = 'D:/Temp/PhenoRice/Processing/Validation_Ermes/ES/Accessory/GR_ERMES_Regional_Grid.shp'

out_folder = "D:/Temp/PhenoRice/Processing/Validation_Ermes/GR"
Accessory_folder = file.path(out_folder, 'Accessory')

#Inizio calcoli -----

#2014
name_shape_14_sinu = file.path(Accessory_folder, 'Static_info_2014_sinu.shp')
name_shape_14_sinu_water = file.path(Accessory_folder, 'Static_info_2014_sinu_water.shp')
name_shape_14_sinu_dry =    file.path(Accessory_folder, 'Static_info_2014_sinu_dry.shp')

name_rast_14_sinu = file.path(Accessory_folder, 'Static_info_2014_sinu.tiff')
name_rast_14_sinu_water = file.path(Accessory_folder, 'Static_info_2014_sinu_water.tiff')
name_rast_14_sinu_dry =    file.path(Accessory_folder,'Static_info_2014_sinu_dry.tiff')

name_shape_15_sinu = file.path(Accessory_folder, 'Static_info_2015_sinu.shp')
name_shape_15_sinu_water = file.path(Accessory_folder, 'Static_info_2015_sinu_water.shp')
name_shape_15_sinu_dry =    file.path(Accessory_folder, 'Static_info_2015_sinu_dry.shp')

name_rast_15_sinu = file.path(Accessory_folder, 'Static_info_2015_sinu.tiff')
name_rast_15_sinu_water = file.path(Accessory_folder, 'Static_info_2015_sinu_water.tiff')
name_rast_15_sinu_dry =    file.path(Accessory_folder,'Static_info_2015_sinu_dry.tiff')

gr_grid_reduced = file.path(out_folder,'Accessory','GR_ERMES_Regional_Grid.shp')
rast_grid  = file.path(out_folder,'Accessory','GR_ERMES_Regional_Grid.tif')
gr_grid_sinu = file.path(out_folder,'Accessory','grid_2k_sinu.shp')

gr_shape_14 = readOGR (dirname(gr_shape_file_14), file_path_sans_ext(basename(gr_shape_file_14)))
gr_rast_14 = raster(gr_raster_file_14, band = 2)
gr_2km_14 = raster(gr_2km_tif_14)
raster::values(gr_rast_14)[which(raster::values(gr_rast_14) > 180)] =NA
raster::values(gr_rast_14)[which(raster::values(gr_rast_14) == 0)] =NA

gr_grid_2k = readOGR(dsn = dirname(gr_grid_reduced),layer = basename(file_path_sans_ext(gr_grid_reduced)))
gr_grid_25_sinu = spTransform(gr_grid_2k,CRS(proj4string(gr_rast_14)))
writeOGR(gr_grid_25_sinu, dsn = Accessory_folder,layer  ='grid_2k_sinu', driver = 'ESRI Shapefile',overwrite_layer = T)

gr_shape_14@data$sowing_met = "Water"
crop_rast_14 = crop(gr_rast_14, extent(gr_grid_25_sinu))

gr_shape_14 = gr_shape_14[!is.na(gr_shape_14@data$crop_type == 'Rice'),]
gr_shape_14 = gr_shape_14[gr_shape_14@data$crop_type == 'Rice' &
                            gr_shape_14@data$sowing_met != 'Unknown' &
                            !is.na(gr_shape_14@data$sowing_doy),]

shape_14_sinu = spTransform(gr_shape_14,CRS(proj4string(gr_rast_14)))
writeOGR(shape_14_sinu, dsn = Accessory_folder,layer  = basename(file_path_sans_ext(name_shape_14_sinu)), driver = 'ESRI Shapefile',overwrite_layer = T )

raster_obs_14 = gdal_rasterize(name_shape_14_sinu, name_rast_14_sinu,a = "sowing_doy", output_Raster = T, ot = 'UInt32',tr = c(15,15), tap = T, of = 'GTiff')
NAvalue(raster_obs_14) = 0

pro_142k = ZonalPipe(gr_grid_sinu,crop_rast_14,rast_grid,'pippo',
                     'int_id', stat="mean", cut = T)

proall_142k = ZonalPipe(gr_grid_sinu,raster_obs_14,rast_grid, 'pippo',
                        'int_id', stat="mean", cut = F)

proallcount_142k = ZonalPipe(gr_grid_sinu,raster_obs_14,rast_grid, 'pippo',
                             'int_id', stat="countna", cut = F)


#2015

gr_shape_15 = readOGR (dirname(gr_shape_file_15), file_path_sans_ext(basename(gr_shape_file_15)))
gr_rast_15 = raster(gr_raster_file_15, band = 2)
gr_2km_15 = raster(gr_2km_tif_15)
raster::values(gr_rast_15)[which(raster::values(gr_rast_15) > 180)] =NA
raster::values(gr_rast_15)[which(raster::values(gr_rast_15) == 0)] =NA

# spTransform(gr_shape_15,CRS(proj4string(crop_rast_15)))
#

gr_shape_15@data$sowing_met = "Water"
crop_rast_15 = crop(gr_rast_15, extent(gr_grid_25_sinu))
# gr_shape_15@data$sowing_met[which(is.na(gr_shape_15@data$sowing_met))] = 'Unknown'
# gr_shape_15@data$sowing_met[which(gr_shape_15@data$sowing_met == 'Unknown' & gr_shape_15@data$sowing_doy< 120)] = 'Dry'
# gr_shape_15@data$sowing_met[which(gr_shape_15@data$sowing_met == 'Unknown' & gr_shape_15@data$sowing_doy >= 120)] = 'Water'

gr_shape_15 = gr_shape_15[!is.na(gr_shape_15@data$crop_type == 'Rice'),]
gr_shape_15 = gr_shape_15[gr_shape_15@data$crop_type == 'Rice' &
                            gr_shape_15@data$sowing_met != 'Unknown' &
                            !is.na(gr_shape_15@data$sowing_doy),]

shape_15_sinu = spTransform(gr_shape_15,CRS(proj4string(crop_rast_15)))
# shape_15_water = spTransform(gr_shape_15[gr_shape_15@data$sowing_met == 'Water' ,],CRS(proj4string(crop_rast_15)))
# shape_15_dry = spTransform(gr_shape_15[gr_shape_15@data$sowing_met == 'Dry' ,],CRS(proj4string(crop_rast_15)))

writeOGR(shape_15_sinu, dsn = Accessory_folder,layer  = basename(file_path_sans_ext(name_shape_15_sinu)), driver = 'ESRI Shapefile',overwrite_layer = T )
# writeOGR(shape_15_water, dsn = Accessory_folder,layer = basename(file_path_sans_ext(name_shape_15_sinu_water)), driver = 'ESRI Shapefile',overwrite_layer = T )
# writeOGR(shape_15_dry, dsn = Accessory_folder  ,layer = basename(file_path_sans_ext(name_shape_15_sinu_dry)), driver = 'ESRI Shapefile',overwrite_layer = T )

raster_obs_15 = gdal_rasterize(name_shape_15_sinu, name_rast_15_sinu,a = "sowing_doy", output_Raster = T, ot = 'UInt32',tr = c(15,15), tap = T, of = 'GTiff')
# raster_obs_wat_15 = gdal_rasterize(name_shape_15_sinu_water, name_rast_15_sinu_water,a = "sowing_doy", output_Raster = T, ot = 'UInt32',tr = c(15,15), tap = T, of = 'GTiff')
# raster_obs_15_dry = gdal_rasterize(name_shape_15_sinu_dry , name_rast_15_sinu_dry,a = "sowing_doy", output_Raster = T, ot = 'UInt32',tr = c(15,15), tap = T, of = 'GTiff')

NAvalue(raster_obs_15) = 0
# NAvalue(raster_obs_wat_15) = 0
# NAvalue(raster_obs_15_dry) = 0
# prova_250 = extract(raster_obs_15,it_grid_250, fun = mean,  na.rm = T, df = T)
# prova_wat_250 = extract(raster_obs_wat_15,it_grid_250, fun = mean,  na.rm = T, df = T)

#
# prova_250_15$mod     = val250_15
# prova_wat_250_15$mod = val250_15
# comp_all_15 = droplevels(subset(prova_250, !is.na(IT_shape_raster_15)))
# comp_wat_15 = droplevels(subset(prova_wat_250, !is.na(IT_shape_raster_water_15)))
# provacomp_melt_15 = melt(comp_all, id.vars = 'ID')
# provacomp_melt_wat_15 = melt(comp_wat, id.vars = 'ID')

pro_152k = ZonalPipe(gr_grid_sinu,crop_rast_15,rast_grid,'pippo',
                     'int_id', stat="mean", cut = T)

proall_152k = ZonalPipe(gr_grid_sinu,raster_obs_15,rast_grid, 'pippo',
                        'int_id', stat="mean", cut = F)

proallcount_152k = ZonalPipe(gr_grid_sinu,raster_obs_15,rast_grid, 'pippo',
                             'int_id', stat="countna", cut = F)

#Fine calcoli - inizio analisi e plotting ----

protot_142k = join(pro_142k@data, proall_142k@data, by = names(pro_142k@data)[1:10], type = 'left')
# protot_142k = join(protot_142k, prodry_142k@data, by = names(pro_142k@data)[1:10], type = 'left')
# protot_142k = join(protot_142k, prowat_142k@data, by = names(pro_142k@data)[1:10], type = 'left')
protot_142k = join(protot_142k, proallcount_142k@data, by = names(pro_142k@data)[1:10], type = 'left')
# protot_142k = join(protot_142k, prodry_142k_count@data, by = names(pro_142k@data)[1:10], type = 'left')
# protot_142k = join(protot_142k, prowatcount_142k@data, by = names(pro_142k@data)[1:10], type = 'left')
names(protot_142k) = c(names(pro_142k@data)[1:10], 'Mod' , 'FieldTot','counttot')
protot_142k_tot = droplevels(subset(protot_142k, !is.nan(Mod) & counttot > 0 ))
# protot_14_dry = droplevels(subset(protot_14, !is.nan(Mod) & countdry > 125 ))
# protot_14_wat = droplevels(subset(protot_14, !is.nan(Mod) & countwat > 125))

# profull_14= rbind(protot_14_tot,protot_14_dry,protot_14_wat)
profull_142k = protot_142k_tot
# profull_14$FieldDry [which(profull_14$countdry < 0.5*profull_14$countwat)] = NA
# profull_14$FieldWat [which(profull_14$countwat < 0.5*profull_14$countdry)] = NA
#protot_14 = droplevels(subset(protot_14,  counttot > 50 ))
profull_142k$difftot = profull_142k$Mod - profull_142k$FieldTot
# profull_142k$diffwat = profull_142k$Mod - profull_142k$FieldWat
# profull_142k$diffdry = profull_142k$Mod - profull_142k$FieldDry

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
# protot_152k = join(protot_152k, prodry_152k@data, by = names(pro_152k@data)[1:10], type = 'left')
# protot_152k = join(protot_152k, prowat_152k@data, by = names(pro_152k@data)[1:10], type = 'left')
protot_152k = join(protot_152k, proallcount_152k@data, by = names(pro_152k@data)[1:10], type = 'left')
# protot_152k = join(protot_152k, prodry_152k_count@data, by = names(pro_152k@data)[1:10], type = 'left')
# protot_152k = join(protot_152k, prowatcount_152k@data, by = names(pro_152k@data)[1:10], type = 'left')
names(protot_152k) = c(names(pro_152k@data)[1:10], 'Mod' , 'FieldTot','counttot')
protot_152k_tot = droplevels(subset(protot_152k, !is.nan(Mod) & counttot >0 ))
# protot_15_dry = droplevels(subset(protot_15, !is.nan(Mod) & countdry > 125 ))
# protot_15_wat = droplevels(subset(protot_15, !is.nan(Mod) & countwat > 125))

# profull_15= rbind(protot_15_tot,protot_15_dry,protot_15_wat)
profull_152k = protot_152k_tot
# profull_15$FieldDry [which(profull_15$countdry < 0.5*profull_15$countwat)] = NA
# profull_15$FieldWat [which(profull_15$countwat < 0.5*profull_15$countdry)] = NA
#protot_15 = droplevels(subset(protot_15,  counttot > 50 ))
profull_152k$difftot = profull_152k$Mod - profull_152k$FieldTot
# profull_152k$diffwat = profull_152k$Mod - profull_152k$FieldWat
# profull_152k$diffdry = profull_152k$Mod - profull_152k$FieldDry

protot_melt_152k = melt(profull_152k, measure.vars = c('Mod' , 'FieldTot',  'difftot'))

profull_142k$Year = 2014
profull_152k$Year = 2015

profull_14_152k = rbind(profull_142k,profull_152k)
protot_melt_14_152k = melt(profull_14_152k, measure.vars = c('Mod' , 'FieldTot', 'difftot'))
stats_14_152k = ddply(protot_melt_14_152k, .(variable) ,summarize, count = length(value),avg = mean(value, na.rm = T), stdev = sd(value, na.rm = T), mae = mean(abs(value),na.rm = T))



gr_2k = profull_14_152k
gr_2k_melt  = protot_melt_14_152k
stats_14_15 = ddply(gr_2k_melt, .(variable,Year) ,summarize, count = length(value),avg = mean(value, na.rm = T), stdev = sd(value, na.rm = T), mae = mean(abs(value),na.rm = T))
save(gr_2k,gr_2k_melt, file = 'd:/temp/phenorice/processing/Validation_Ermes/GR/GR_val_2k.RData')



p = ggplot(droplevels(subset(gr_2k_melt, variable !='difftot')), aes(x = value, color = variable ))
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

