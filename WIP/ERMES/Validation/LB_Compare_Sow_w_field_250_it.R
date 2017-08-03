library(plyr)
library(ggplot2)
library(reshape2)
library(tools)
library(rgdal)
library(raster)
library(data.table)
library(gdalUtils)

it_shape_file_15 = 'D:/Temp/PhenoRice/Processing/Validation_Ermes/Input/IT_Static_info_2015.shp'
it_raster_file_15 = 'Y:/ermes/datasets/rs_products/Phenology/IT/Outputs/v4.0/2015/raster/old_min_identification_Full_Out_2015.dat'
it_2km_tif_15 = 'Y:/ermes/datasets/rs_products/Phenology/IT/Outputs/v4.0/ERMES_Grid/TIFFS/2015/MinDoys/IT_Phenology_MinDoys_2015_004.tif'

it_shape_file_14 = 'D:/Temp/PhenoRice/Processing/Validation_Ermes/Input/IT_Static_info_2014.shp'
it_raster_file_14 = 'Y:/ermes/datasets/rs_products/Phenology/IT/Outputs/v1.0/2014/raster/old_min_identification_Full_Out_2014.dat'
it_2km_tif_14 = 'Y:/ermes/datasets/rs_products/Phenology/IT/Outputs/v1.0/ERMES_Grid/TIFFS/2014/MinDoys/IT_Phenology_MinDoys_2014_001.tif'

it_grid_reduced = 'D:/Temp/PhenoRice/Processing/Validation_Ermes/IT/Accessory/Grid_MODIS_250_reduced_red.shp'

out_folder = "D:/Temp/PhenoRice/Processing/Validation_Ermes/IT"
Accessory_folder = file.path(out_folder, 'Accessory')

#Inizio calcoli -----

#2014
name_shape_14_sinu = file.path(Accessory_folder, 'Static_info_2014_sinu.shp')
name_shape_14_sinu_water = file.path(Accessory_folder, 'Static_info_2014_sinu_water.shp')
name_shape_14_sinu_dry =    file.path(Accessory_folder, 'Static_info_2014_sinu_dry.shp')

name_rast_14_sinu = file.path(Accessory_folder, 'Static_info_2014_sinu.tiff')
name_rast_14_sinu_water = file.path(Accessory_folder, 'Static_info_2014_sinu_water.tiff')
name_rast_14_sinu_dry =    file.path(Accessory_folder,'Static_info_2014_sinu_dry.tiff')

it_grid_reduced = file.path(out_folder,'Accessory','Grid_MODIS_250_reduced_red.shp')
rast_grid  = file.path(out_folder,'Accessory','Grid_MODIS_250_reduced.tif')


it_shape_14 = readOGR (dirname(it_shape_file_14), file_path_sans_ext(basename(it_shape_file_14)))
it_rast_14 = raster(it_raster_file_14, band = 2)
it_2km_14 = raster(it_2km_tif_14)
# it_grid_sp = readOGR(dirname(it_grid_reduced), file_path_sans_ext(basename(it_grid_reduced)))
raster::values(it_rast_14)[which(raster::values(it_rast_14) > 180)] =NA
raster::values(it_rast_14)[which(raster::values(it_rast_14) == 0)] =NA


it_grid_250 = readOGR(dsn = dirname(it_grid_reduced),layer = basename(file_path_sans_ext(it_grid_reduced)))
it_shape_14 = readOGR (dirname(it_shape_file_14), basename(file_path_sans_ext((it_shape_file_14))))
levels(it_shape_14@data$sowing_met) = c("Dry","Water","Unknown")
crop_rast_14 = crop(it_rast_14, extent(it_grid_250))
it_shape_14@data$sowing_met[which(is.na(it_shape_14@data$sowing_met))] = 'Unknown'
# it_shape_14@data$sowing_met[which(it_shape_14@data$sowing_met == 'Unknown' & it_shape_14@data$sowing_doy< 120)] = 'Dry'
# it_shape_14@data$sowing_met[which(it_shape_14@data$sowing_met == 'Unknown' & it_shape_14@data$sowing_doy >= 120)] = 'Water'

it_shape_14 = it_shape_14[!is.na(it_shape_14@data$crop_type == 'Rice'),]
it_shape_14 = it_shape_14[it_shape_14@data$crop_type == 'Rice' &
                            it_shape_14@data$sowing_met != 'Unknown' &
                            !is.na(it_shape_14@data$sowing_doy),]

shape_14_sinu = spTransform(it_shape_14,CRS(proj4string(crop_rast_14)))
shape_14_water = spTransform(it_shape_14[it_shape_14@data$sowing_met == 'Water' ,],CRS(proj4string(crop_rast_14)))
shape_14_dry = spTransform(it_shape_14[it_shape_14@data$sowing_met == 'Dry' ,],CRS(proj4string(crop_rast_14)))

writeOGR(shape_14_sinu, dsn = Accessory_folder,layer  = basename(file_path_sans_ext(name_shape_14_sinu)), driver = 'ESRI Shapefile',overwrite_layer = T )
writeOGR(shape_14_water, dsn = Accessory_folder,layer = basename(file_path_sans_ext(name_shape_14_sinu_water)), driver = 'ESRI Shapefile',overwrite_layer = T )
writeOGR(shape_14_dry, dsn = Accessory_folder  ,layer = basename(file_path_sans_ext(name_shape_14_sinu_dry)), driver = 'ESRI Shapefile',overwrite_layer = T )

raster_obs_14 = gdal_rasterize(name_shape_14_sinu, name_rast_14_sinu,a = "sowing_doy", output_Raster = T, ot = 'UInt32',tr = c(15,15), tap = T, of = 'GTiff')
raster_obs_wat_14 = gdal_rasterize(name_shape_14_sinu_water, name_rast_14_sinu_water,a = "sowing_doy", output_Raster = T, ot = 'UInt32',tr = c(15,15), tap = T, of = 'GTiff')
raster_obs_14_dry = gdal_rasterize(name_shape_14_sinu_dry , name_rast_14_sinu_dry,a = "sowing_doy", output_Raster = T, ot = 'UInt32',tr = c(15,15), tap = T, of = 'GTiff')

NAvalue(raster_obs_14) = 0
NAvalue(raster_obs_wat_14) = 0
NAvalue(raster_obs_14_dry) = 0

pro_14 = ZonalPipe(it_grid_reduced,crop_rast_14,rast_grid,'pippo',
                   'id', stat="mean", cut =T)

prodry_14 = ZonalPipe(it_grid_reduced,raster_obs_14_dry,rast_grid, 'pippo',
                      'id', stat="mean", cut = F)

prodry_14_count = ZonalPipe(it_grid_reduced,raster_obs_14_dry,rast_grid, 'pippo',
                            'id', stat="countna", cut = F)

prowat_14 = ZonalPipe(it_grid_reduced,raster_obs_wat_14,rast_grid, 'pippo',
                      'id', stat="mean", cut = F)

prowatcount_14 = ZonalPipe(it_grid_reduced,raster_obs_wat_14,rast_grid, 'pippo',
                           'id', stat="countna", cut = F)

proall_14 = ZonalPipe(it_grid_reduced,raster_obs_14,rast_grid, 'pippo',
                      'id', stat="mean", cut = F)

proallcount_14 = ZonalPipe(it_grid_reduced,raster_obs_14,rast_grid, 'pippo',
                           'id', stat="countna", cut = F)


#2015

name_shape_15_sinu = file.path(Accessory_folder, 'Static_info_2015_sinu.shp')
name_shape_15_sinu_water = file.path(Accessory_folder, 'Static_info_2015_sinu_water.shp')
name_shape_15_sinu_dry =    file.path(Accessory_folder, 'Static_info_2015_sinu_dry.shp')

name_rast_15_sinu = file.path(Accessory_folder, 'Static_info_2015_sinu.tiff')
name_rast_15_sinu_water = file.path(Accessory_folder, 'Static_info_2015_sinu_water.tiff')
name_rast_15_sinu_dry =    file.path(Accessory_folder,'Static_info_2015_sinu_dry.tiff')

it_shape_15 = readOGR (dirname(it_shape_file_15), file_path_sans_ext(basename(it_shape_file_15)))
it_rast_15 = raster(it_raster_file_15, band = 2)
it_2km_15 = raster(it_2km_tif_15)
# it_grid_sp = readOGR(dirname(it_grid_reduced), file_path_sans_ext(basename(it_grid_reduced)))
raster::values(it_rast_15)[which(raster::values(it_rast_15) > 180)] =NA
raster::values(it_rast_15)[which(raster::values(it_rast_15) == 0)] =NA

it_shape_15 = readOGR (dirname(it_shape_file_15), basename(file_path_sans_ext((it_shape_file_15))))
levels(it_shape_15@data$sowing_met) = c("Dry","Water","Unknown")
crop_rast_15 = crop(it_rast_15, extent(it_grid_250))
it_shape_15@data$sowing_met[which(is.na(it_shape_15@data$sowing_met))] = 'Unknown'
# it_shape_15@data$sowing_met[which(it_shape_14@data$sowing_met == 'Unknown' & it_shape_14@data$sowing_doy< 120)] = 'Dry'
# it_shape_14@data$sowing_met[which(it_shape_14@data$sowing_met == 'Unknown' & it_shape_14@data$sowing_doy >= 120)] = 'Water'

it_shape_15 = it_shape_15[!is.na(it_shape_15@data$crop_type == 'Rice'),]
it_shape_15 = it_shape_15[it_shape_15@data$crop_type == 'Rice' &
                            it_shape_15@data$sowing_met != 'Unknown' &
                            !is.na(it_shape_15@data$sowing_doy),]

shape_15_sinu = spTransform(it_shape_15,CRS(proj4string(crop_rast_15)))
shape_15_water = spTransform(it_shape_15[it_shape_15@data$sowing_met == 'Water' ,],CRS(proj4string(crop_rast_15)))
shape_15_dry = spTransform(it_shape_15[it_shape_15@data$sowing_met == 'Dry' ,],CRS(proj4string(crop_rast_15)))

writeOGR(shape_15_sinu, dsn = Accessory_folder,layer  = basename(file_path_sans_ext(name_shape_15_sinu)), driver = 'ESRI Shapefile',overwrite_layer = T )
writeOGR(shape_15_water, dsn = Accessory_folder,layer = basename(file_path_sans_ext(name_shape_15_sinu_water)), driver = 'ESRI Shapefile',overwrite_layer = T )
writeOGR(shape_15_dry, dsn = Accessory_folder  ,layer = basename(file_path_sans_ext(name_shape_15_sinu_dry)), driver = 'ESRI Shapefile',overwrite_layer = T )

raster_obs_15 = gdal_rasterize(name_shape_15_sinu, name_rast_15_sinu,a = "sowing_doy", output_Raster = T, ot = 'UInt32',tr = c(15,15), tap = T, of = 'GTiff')
raster_obs_wat_15 = gdal_rasterize(name_shape_15_sinu_water, name_rast_15_sinu_water,a = "sowing_doy", output_Raster = T, ot = 'UInt32',tr = c(15,15), tap = T, of = 'GTiff')
raster_obs_15_dry = gdal_rasterize(name_shape_15_sinu_dry , name_rast_15_sinu_dry,a = "sowing_doy", output_Raster = T, ot = 'UInt32',tr = c(15,15), tap = T, of = 'GTiff')

NAvalue(raster_obs_15) = 0
NAvalue(raster_obs_wat_15) = 0
NAvalue(raster_obs_15_dry) = 0

pro_15 = ZonalPipe(it_grid_reduced,crop_rast_15,rast_grid,'pippo',
                   'id', stat="mean", cut = T)

prodry_15 = ZonalPipe(it_grid_reduced,raster_obs_15_dry,rast_grid, 'pippo',
                      'id', stat="mean", cut = F)

prodry_15_count = ZonalPipe(it_grid_reduced,raster_obs_15_dry,rast_grid, 'pippo',
                            'id', stat="countna", cut = F)

prowat_15 = ZonalPipe(it_grid_reduced,raster_obs_wat_15,rast_grid, 'pippo',
                      'id', stat="mean", cut = F)

prowatcount_15 = ZonalPipe(it_grid_reduced,raster_obs_wat_15,rast_grid, 'pippo',
                           'id', stat="countna", cut = F)

proall_15 = ZonalPipe(it_grid_reduced,raster_obs_15,rast_grid, 'pippo',
                      'id', stat="mean", cut = F)

proallcount_15 = ZonalPipe(it_grid_reduced,raster_obs_15,rast_grid, 'pippo',
                           'id', stat="countna", cut = F)

#Fine calcoli - inizio analisi e plotting ----

protot_14 = join(pro_14@data, proall_14@data, by = c('id','z'), type = 'left')
protot_14 = join(protot_14, prodry_14@data, by = c('id','z'), type = 'left')
protot_14 = join(protot_14, prowat_14@data, by = c('id','z'), type = 'left')
protot_14 = join(protot_14, proallcount_14@data, by = c('id','z'), type = 'left')
protot_14 = join(protot_14, prodry_14_count@data, by = c('id','z'), type = 'left')
protot_14 = join(protot_14, prowatcount_14@data, by = c('id','z'), type = 'left')
names(protot_14) = c('id' ,     'z', 'Mod' , 'FieldTot', 'FieldDry',  'FieldWat','counttot','countdry','countwat')
protot_14_tot = droplevels(subset(protot_14, !is.nan(Mod) & counttot > 0))
# protot_14_dry = droplevels(subset(protot_14, !is.nan(Mod) & countdry > 125 ))
# protot_14_wat = droplevels(subset(protot_14, !is.nan(Mod) & countwat > 125))

# profull_14= rbind(protot_14_tot,protot_14_dry,protot_14_wat)
profull_14 = protot_14_tot
# profull_14$FieldDry [which(profull_14$countdry < 0.5*profull_14$countwat)] = NA
# profull_14$FieldWat [which(profull_14$countwat < 0.5*profull_14$countdry)] = NA
#protot_14 = droplevels(subset(protot_14,  counttot > 50 ))
profull_14$difftot = profull_14$Mod - profull_14$FieldTot
profull_14$diffwat = profull_14$Mod - profull_14$FieldWat
profull_14$diffdry = profull_14$Mod - profull_14$FieldDry

protot_melt_14 = melt(profull_14, measure.vars = c('Mod' , 'FieldTot',  'FieldWat', 'FieldDry', 'difftot','diffwat','diffdry'))
stats_14 = ddply(protot_melt_14, .(variable) ,summarize, count = length(value),avg = mean(value, na.rm = T), stdev = sd(value, na.rm = T), mae = mean(abs(value),na.rm = T))

protot_15 = join(pro_15@data, proall_15@data, by = c('id','z'), type = 'left')
protot_15 = join(protot_15, prodry_15@data, by = c('id','z'), type = 'left')
protot_15 = join(protot_15, prowat_15@data, by = c('id','z'), type = 'left')
protot_15 = join(protot_15, proallcount_15@data, by = c('id','z'), type = 'left')
protot_15 = join(protot_15, prodry_15_count@data, by = c('id','z'), type = 'left')
protot_15 = join(protot_15, prowatcount_15@data, by = c('id','z'), type = 'left')
names(protot_15) = c('id' ,     'z', 'Mod' , 'FieldTot', 'FieldDry',  'FieldWat','counttot','countdry','countwat')
protot_15_tot = droplevels(subset(protot_15, !is.nan(Mod) & counttot > 0 ))
# protot_15_dry = droplevels(subset(protot_15, !is.nan(Mod) & countdry > 125))
# protot_15_wat = droplevels(subset(protot_15, !is.nan(Mod) & countwat > 125 ))

# profull_15= rbind(protot_15_tot,protot_15_dry,protot_15_wat)
profull_15 = protot_15_tot
#protot_15 = droplevels(subset(protot_15,  counttot > 50 ))
profull_15$difftot = profull_15$Mod - profull_15$FieldTot
profull_15$diffwat = profull_15$Mod - profull_15$FieldWat
profull_15$diffdry = profull_15$Mod - profull_15$FieldDry

protot_melt_15 = melt(profull_15, measure.vars = c('Mod' , 'FieldTot',  'FieldWat', 'FieldDry', 'difftot','diffwat','diffdry'))

stats_15 = ddply(protot_melt_15, .(variable) ,summarize, count = length(value),avg = mean(value, na.rm = T), stdev = sd(value, na.rm = T), mae = mean(abs(value),na.rm = T))

profull_14$Year =2014
profull_15$Year =2015
profull_14_15 = rbind(profull_14,profull_15)
protot_melt_14_15 = melt(profull_14_15, measure.vars = c('Mod' , 'FieldTot',  'FieldWat', 'FieldDry', 'difftot','diffwat','diffdry'))


it_250 = profull_14_15
it_250_melt  = protot_melt_14_15
stats_14_15 = ddply(it_250_melt, .(variable,Year) ,summarize, count = length(value),avg = mean(value, na.rm = T), stdev = sd(value, na.rm = T), mae = mean(abs(value),na.rm = T))
save(it_250,it_250_melt, file = 'd:/temp/phenorice/processing/Validation_Ermes/IT/IT_val_250.RData')


#END ----


p = ggplot(droplevels(subset(protot_melt_14, variable !='difftot'& variable !='diffwat'& variable !='diffdry' & variable != 'FieldTot')), aes(x = value, color = variable ))
# p =p+ stat_bin(binwidth = 8, aes(y=..count../sum(..count..)))+facet_grid(~variable)+theme_bw()
# p = p+geom_histogram(binwidth = 8)+facet_grid(~variable)+theme_bw()
p = p + geom_freqpoly(binwidth = 8) #+facet_grid(~variable)+theme_bw()
#p = p+geom_boxplot(adjust = 1.4)+facet_grid(variable~sowing_met)+theme_bw()+geom_jitter()
p = p+geom_density(alpha = 0.2, adjust = 1) #+facet_grid(~variable)+theme_bw()

p

p = ggplot(droplevels(subset(protot_melt_14, variable !='difftot'& variable !='diffwat'& variable !='diffdry')), aes(x = variable, y = value ))
p = p + geom_violin()+theme_bw()   +ylim(50,200)
p



p = ggplot(droplevels(subset(protot_melt_15, variable !='difftot'& variable !='diffwat'& variable !='diffdry')), aes(x = value, color = variable ))
p =p+ stat_bin(binwidth = 8, aes(y=..count../sum(..count..)))+facet_grid(~variable)+theme_bw()
p = p+geom_histogram(binwidth = 8)+facet_grid(~variable)+theme_bw()
p = p + geom_freqpoly(binwidth = 16) #+facet_grid(~variable)+theme_bw()
#p = p+geom_boxplot(adjust = 1.4)+facet_grid(variable~sowing_met)+theme_bw()+geom_jitter()
p = p+geom_density(alpha = 0.2, adjust = 1.2) #+facet_grid(~variable)+theme_bw()

p

p = ggplot(droplevels(subset(protot_melt_15, variable !='difftot'& variable !='diffwat'& variable !='diffdry')), aes(x = variable, y = value ))
p = p + geom_boxplot()+theme_bw()   +ylim(50,200)
p


p = ggplot(droplevels(subset(protot_melt_14_15, variable !='difftot'& variable !='diffwat'& variable !='diffdry')), aes(x = variable, y = value ))
p = p + geom_boxplot()+theme_bw()   +ylim(50,200)  + geom_jitter()
p

p = ggplot(droplevels(subset(protot_melt_14_15, variable !='difftot'& variable !='diffwat'& variable !='diffdry'& variable !='FieldTot')), aes(x = value, color = variable ))
p =p+ stat_bin(binwidth = 8, aes(y=..count../sum(..count..)))+facet_grid(~variable)+theme_bw()
p = p+geom_histogram(binwidth = 8)+facet_grid(~variable)+theme_bw()
p = p + geom_freqpoly(binwidth = 8) #+facet_grid(~variable)+theme_bw()
#p = p+geom_boxplot(adjust = 1.4)+facet_grid(variable~sowing_met)+theme_bw()+geom_jitter()
p = p+geom_density(alpha = 0.2, adjust = 1.2) #+facet_grid(~variable)+theme_bw()

p
