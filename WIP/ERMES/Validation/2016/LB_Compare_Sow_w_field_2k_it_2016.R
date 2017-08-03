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
out_folder = file.path(out_folder_gen, "IT")
dir.create(out_folder, recursive = TRUE)
Accessory_folder = file.path(out_folder_gen, 'Accessory')
dir.create(Accessory_folder, recursive = TRUE)

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

name_shape_16_sinu = file.path(Accessory_folder, 'Static_info_2016_sinu.shp')
name_shape_16_sinu_water = file.path(Accessory_folder, 'Static_info_2016_sinu_water.shp')
name_shape_16_sinu_dry =    file.path(Accessory_folder, 'Static_info_2016_sinu_dry.shp')

name_rast_16_sinu = file.path(Accessory_folder, 'Static_info_2016_sinu.tiff')
name_rast_16_sinu_water = file.path(Accessory_folder, 'Static_info_2016_sinu_water.tiff')
name_rast_16_sinu_dry =    file.path(Accessory_folder,'Static_info_2016_sinu_dry.tiff')


it_grid_reduced = file.path(out_folder,'Accessory','IT_ERMES_Regional_Grid.shp')
rast_grid  = file.path(out_folder,'Accessory','IT_ERMES_Regional_Grid.tif')
it_grid_sinu = file.path(out_folder,'Accessory','grid_2k_sinu.shp')

it_shape_14 = readOGR (dirname(it_shape_file_14), file_path_sans_ext(basename(it_shape_file_14)))
it_rast_14 = raster(it_raster_file_14, band = 2)
it_2km_14 = raster(it_2km_tif_14)
# it_grid_sp = readOGR(dirname(it_grid_reduced), file_path_sans_ext(basename(it_grid_reduced)))
raster::values(it_rast_14)[which(raster::values(it_rast_14) > 180)] =NA
raster::values(it_rast_14)[which(raster::values(it_rast_14) == 0)] =NA

# spTransform(it_shape_14,CRS(proj4string(crop_rast_14)))
#
it_grid_2k = readOGR(dsn = dirname(it_grid_reduced),layer = basename(file_path_sans_ext(it_grid_reduced)))
it_grid_25_sinu = spTransform(it_grid_2k,CRS(proj4string(it_rast_14)))
writeOGR(it_grid_25_sinu, dsn = Accessory_folder,layer  ='grid_2k_sinu', driver = 'ESRI Shapefile',overwrite_layer = T)

levels(it_shape_14@data$sowing_met) = c("Dry","Water","Unknown")
crop_rast_14 = crop(it_rast_14, extent(it_grid_25_sinu))
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

pro_142k = ZonalPipe(it_grid_sinu,crop_rast_14,rast_grid,'pippo',
                   'int_id', stat="mean", cut = T)

prodry_142k = ZonalPipe(it_grid_sinu,raster_obs_14_dry,rast_grid, 'pippo',
                      'int_id', stat="mean", cut = F)

prodry_142k_count = ZonalPipe(it_grid_sinu,raster_obs_14_dry,rast_grid, 'pippo',
                            'int_id', stat="countna", cut = F)

prowat_142k = ZonalPipe(it_grid_sinu,raster_obs_wat_14,rast_grid, 'pippo',
                      'int_id', stat="mean", cut = F)

prowatcount_142k = ZonalPipe(it_grid_sinu,raster_obs_wat_14,rast_grid, 'pippo',
                           'int_id', stat="countna", cut = F)

proall_142k = ZonalPipe(it_grid_sinu,raster_obs_14,rast_grid, 'pippo',
                      'int_id', stat="mean", cut = F)

proallcount_142k = ZonalPipe(it_grid_sinu,raster_obs_14,rast_grid, 'pippo',
                           'int_id', stat="countna", cut = F)

#2015

it_shape_15 = readOGR (dirname(it_shape_file_15), file_path_sans_ext(basename(it_shape_file_15)))
it_rast_15 = raster(it_raster_file_15, band = 2)
it_2km_15 = raster(it_2km_tif_15)
# it_grid_sp = readOGR(dirname(it_grid_reduced), file_path_sans_ext(basename(it_grid_reduced)))
raster::values(it_rast_15)[which(raster::values(it_rast_15) > 180)] =NA
raster::values(it_rast_15)[which(raster::values(it_rast_15) == 0)] =NA

# spTransform(it_shape_15,CRS(proj4string(crop_rast_15)))
#
it_grid_2k = readOGR(dsn = dirname(it_grid_reduced),layer = basename(file_path_sans_ext(it_grid_reduced)))
it_grid_25_sinu = spTransform(it_grid_2k,CRS(proj4string(it_rast_15)))
writeOGR(it_grid_25_sinu, dsn = Accessory_folder,layer  ='grid_2k_sinu', driver = 'ESRI Shapefile',overwrite_layer = T)

levels(it_shape_15@data$sowing_met) = c("Dry","Water","Unknown")
crop_rast_15 = crop(it_rast_15, extent(it_grid_25_sinu))
it_shape_15@data$sowing_met[which(is.na(it_shape_15@data$sowing_met))] = 'Unknown'
# it_shape_15@data$sowing_met[which(it_shape_15@data$sowing_met == 'Unknown' & it_shape_15@data$sowing_doy< 120)] = 'Dry'
# it_shape_15@data$sowing_met[which(it_shape_15@data$sowing_met == 'Unknown' & it_shape_15@data$sowing_doy >= 120)] = 'Water'

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

pro_152k = ZonalPipe(it_grid_sinu,crop_rast_15,rast_grid,'pippo',
                     'int_id', stat="mean", cut = T)

prodry_152k = ZonalPipe(it_grid_sinu,raster_obs_15_dry,rast_grid, 'pippo',
                        'int_id', stat="mean", cut = F)

prodry_152k_count = ZonalPipe(it_grid_sinu,raster_obs_15_dry,rast_grid, 'pippo',
                              'int_id', stat="countna", cut = F)

prowat_152k = ZonalPipe(it_grid_sinu,raster_obs_wat_15,rast_grid, 'pippo',
                        'int_id', stat="mean", cut = F)

prowatcount_152k = ZonalPipe(it_grid_sinu,raster_obs_wat_15,rast_grid, 'pippo',
                             'int_id', stat="countna", cut = F)

proall_152k = ZonalPipe(it_grid_sinu,raster_obs_15,rast_grid, 'pippo',
                        'int_id', stat="mean", cut = F)

proallcount_152k = ZonalPipe(it_grid_sinu,raster_obs_15,rast_grid, 'pippo',
                             'int_id', stat="countna", cut = F)

#2016

#2015

it_shape_16 = readOGR (dirname(it_shape_file_16), file_path_sans_ext(basename(it_shape_file_16)))
it_rast_16 = raster(it_raster_file_16, band = 2)
it_2km_16 = raster(it_2km_tif_16)
# it_grid_sp = readOGR(dirname(it_grid_reduced), file_path_sans_ext(basename(it_grid_reduced)))
raster::values(it_rast_16)[which(raster::values(it_rast_16) > 180)] =NA
raster::values(it_rast_16)[which(raster::values(it_rast_16) == 0)] =NA

# spTransform(it_shape_16,CRS(proj4string(crop_rast_16)))
#
it_grid_2k = readOGR(dsn = dirname(it_grid_reduced),layer = basename(file_path_sans_ext(it_grid_reduced)))
it_grid_25_sinu = spTransform(it_grid_2k,CRS(proj4string(it_rast_16)))
writeOGR(it_grid_25_sinu, dsn = Accessory_folder,layer  ='grid_2k_sinu', driver = 'ESRI Shapefile',overwrite_layer = T)

levels(it_shape_16@data$sowing_met) = c("Dry","Water","Unknown")
crop_rast_16 = crop(it_rast_16, extent(it_grid_25_sinu))
it_shape_16@data$sowing_met[which(is.na(it_shape_16@data$sowing_met))] = 'Unknown'
# it_shape_16@data$sowing_met[which(it_shape_16@data$sowing_met == 'Unknown' & it_shape_16@data$sowing_doy< 120)] = 'Dry'
# it_shape_16@data$sowing_met[which(it_shape_16@data$sowing_met == 'Unknown' & it_shape_16@data$sowing_doy >= 120)] = 'Water'

it_shape_16 = it_shape_16[!is.na(it_shape_16@data$crop_type == 'Rice'),]
it_shape_16 = it_shape_16[it_shape_16@data$crop_type == 'Rice' &
                            it_shape_16@data$sowing_met != 'Unknown' &
                            !is.na(it_shape_16@data$sowing_doy),]

shape_16_sinu = spTransform(it_shape_16,CRS(proj4string(crop_rast_16)))
shape_16_water = spTransform(it_shape_16[it_shape_16@data$sowing_met == 'Water' ,],CRS(proj4string(crop_rast_16)))
shape_16_dry = spTransform(it_shape_16[it_shape_16@data$sowing_met == 'Dry' ,],CRS(proj4string(crop_rast_16)))

writeOGR(shape_16_sinu, dsn = Accessory_folder,layer  = basename(file_path_sans_ext(name_shape_16_sinu)), driver = 'ESRI Shapefile',overwrite_layer = T )
writeOGR(shape_16_water, dsn = Accessory_folder,layer = basename(file_path_sans_ext(name_shape_16_sinu_water)), driver = 'ESRI Shapefile',overwrite_layer = T )
writeOGR(shape_16_dry, dsn = Accessory_folder  ,layer = basename(file_path_sans_ext(name_shape_16_sinu_dry)), driver = 'ESRI Shapefile',overwrite_layer = T )

raster_obs_16 = gdal_rasterize(name_shape_16_sinu, name_rast_16_sinu,a = "sowing_doy", output_Raster = T, ot = 'UInt32',tr = c(16,16), tap = T, of = 'GTiff')
raster_obs_wat_16 = gdal_rasterize(name_shape_16_sinu_water, name_rast_16_sinu_water,a = "sowing_doy", output_Raster = T, ot = 'UInt32',tr = c(16,16), tap = T, of = 'GTiff')
raster_obs_16_dry = gdal_rasterize(name_shape_16_sinu_dry , name_rast_16_sinu_dry,a = "sowing_doy", output_Raster = T, ot = 'UInt32',tr = c(16,16), tap = T, of = 'GTiff')

NAvalue(raster_obs_16) = 0
NAvalue(raster_obs_wat_16) = 0
NAvalue(raster_obs_16_dry) = 0

pro_162k = ZonalPipe(it_grid_sinu,crop_rast_16,rast_grid,'pippo',
                     'int_id', stat="mean", cut = T)

prodry_162k = ZonalPipe(it_grid_sinu,raster_obs_16_dry,rast_grid, 'pippo',
                        'int_id', stat="mean", cut = F)

prodry_162k_count = ZonalPipe(it_grid_sinu,raster_obs_16_dry,rast_grid, 'pippo',
                              'int_id', stat="countna", cut = F)

prowat_162k = ZonalPipe(it_grid_sinu,raster_obs_wat_16,rast_grid, 'pippo',
                        'int_id', stat="mean", cut = F)

prowatcount_162k = ZonalPipe(it_grid_sinu,raster_obs_wat_16,rast_grid, 'pippo',
                             'int_id', stat="countna", cut = F)

proall_162k = ZonalPipe(it_grid_sinu,raster_obs_16,rast_grid, 'pippo',
                        'int_id', stat="mean", cut = F)

proallcount_162k = ZonalPipe(it_grid_sinu,raster_obs_16,rast_grid, 'pippo',
                             'int_id', stat="countna", cut = F)                             


#Fine calcoli - inizio analisi e plotting ----

protot_142k = join(pro_142k@data, proall_142k@data, by = names(pro_142k@data)[1:10], type = 'left')
protot_142k = join(protot_142k, prodry_142k@data, by = names(pro_142k@data)[1:10], type = 'left')
protot_142k = join(protot_142k, prowat_142k@data, by = names(pro_142k@data)[1:10], type = 'left')
protot_142k = join(protot_142k, proallcount_142k@data, by = names(pro_142k@data)[1:10], type = 'left')
protot_142k = join(protot_142k, prodry_142k_count@data, by = names(pro_142k@data)[1:10], type = 'left')
protot_142k = join(protot_142k, prowatcount_142k@data, by = names(pro_142k@data)[1:10], type = 'left')
names(protot_142k) = c(names(pro_142k@data)[1:10], 'Mod' , 'FieldTot', 'FieldDry',  'FieldWat','counttot','countdry','countwat')
protot_142k_tot = droplevels(subset(protot_142k, !is.nan(Mod) & counttot > 100 ))
# protot_14_dry = droplevels(subset(protot_14, !is.nan(Mod) & countdry > 125 ))
# protot_14_wat = droplevels(subset(protot_14, !is.nan(Mod) & countwat > 125))

# profull_14= rbind(protot_14_tot,protot_14_dry,protot_14_wat)
profull_142k = protot_142k_tot
# profull_14$FieldDry [which(profull_14$countdry < 0.5*profull_14$countwat)] = NA
# profull_14$FieldWat [which(profull_14$countwat < 0.5*profull_14$countdry)] = NA
#protot_14 = droplevels(subset(protot_14,  counttot > 50 ))
profull_142k$difftot = profull_142k$Mod - profull_142k$FieldTot
profull_142k$diffwat = profull_142k$Mod - profull_142k$FieldWat
profull_142k$diffdry = profull_142k$Mod - profull_142k$FieldDry

protot_melt_142k = melt(profull_142k, measure.vars = c('Mod' , 'FieldTot',  'FieldWat', 'FieldDry', 'difftot','diffwat','diffdry'))


# 
# p = ggplot(droplevels(subset(protot_melt_142k, variable !='difftot'& variable !='diffwat'& variable !='diffdry' & variable != 'FieldTot')), aes(x = value, color = variable ))
# # p =p+ stat_bin(binwidth = 8, aes(y=..count../sum(..count..)))+facet_grid(~variable)+theme_bw()
# # p = p+geom_histogram(binwidth = 8)+facet_grid(~variable)+theme_bw()
# p = p + geom_freqpoly(binwidth = 8) #+facet_grid(~variable)+theme_bw()
# #p = p+geom_boxplot(adjust = 1.4)+facet_grid(variable~sowing_met)+theme_bw()+geom_jitter()
# p = p+geom_density(alpha = 0.2, adjust = 1) #+facet_grid(~variable)+theme_bw()
# 
# p
# 
# p = ggplot(droplevels(subset(protot_melt_142k, variable !='difftot'& variable !='diffwat'& variable !='diffdry')), aes(x = variable, y = value ))
# p = p + geom_violin()+theme_bw()   +ylim(50,200)
# p

stats_14 = ddply(protot_melt_142k, .(variable) ,summarize, count = length(value),avg = mean(value, na.rm = T), stdev = sd(value, na.rm = T), mae = mean(abs(value),na.rm = T))

#2015
protot_152k = join(pro_152k@data, proall_152k@data, by = names(pro_152k@data)[1:10], type = 'left')
protot_152k = join(protot_152k, prodry_152k@data, by = names(pro_152k@data)[1:10], type = 'left')
protot_152k = join(protot_152k, prowat_152k@data, by = names(pro_152k@data)[1:10], type = 'left')
protot_152k = join(protot_152k, proallcount_152k@data, by = names(pro_152k@data)[1:10], type = 'left')
protot_152k = join(protot_152k, prodry_152k_count@data, by = names(pro_152k@data)[1:10], type = 'left')
protot_152k = join(protot_152k, prowatcount_152k@data, by = names(pro_152k@data)[1:10], type = 'left')
names(protot_152k) = c(names(pro_152k@data)[1:10], 'Mod' , 'FieldTot', 'FieldDry',  'FieldWat','counttot','countdry','countwat')
protot_152k_tot = droplevels(subset(protot_152k, !is.nan(Mod) & counttot >0 ))
# protot_15_dry = droplevels(subset(protot_15, !is.nan(Mod) & countdry > 125 ))
# protot_15_wat = droplevels(subset(protot_15, !is.nan(Mod) & countwat > 125))

# profull_15= rbind(protot_15_tot,protot_15_dry,protot_15_wat)
profull_152k = protot_152k_tot
# profull_15$FieldDry [which(profull_15$countdry < 0.5*profull_15$countwat)] = NA
# profull_15$FieldWat [which(profull_15$countwat < 0.5*profull_15$countdry)] = NA
#protot_15 = droplevels(subset(protot_15,  counttot > 50 ))
profull_152k$difftot = profull_152k$Mod - profull_152k$FieldTot
profull_152k$diffwat = profull_152k$Mod - profull_152k$FieldWat
profull_152k$diffdry = profull_152k$Mod - profull_152k$FieldDry

protot_melt_152k = melt(profull_152k, measure.vars = c('Mod' , 'FieldTot',  'FieldWat', 'FieldDry', 'difftot','diffwat','diffdry'))



# p = ggplot(droplevels(subset(protot_melt_152k, variable !='difftot'& variable !='diffwat'& variable !='diffdry' & variable != 'FieldTot')), aes(x = value, color = variable ))
# # p =p+ stat_bin(binwidth = 8, aes(y=..count../sum(..count..)))+facet_grid(~variable)+theme_bw()
# # p = p+geom_histogram(binwidth = 8)+facet_grid(~variable)+theme_bw()
# p = p + geom_freqpoly(binwidth = 8) #+facet_grid(~variable)+theme_bw()
# #p = p+geom_boxplot(adjust = 1.4)+facet_grid(variable~sowing_met)+theme_bw()+geom_jitter()
# p = p+geom_density(alpha = 0.2, adjust = 1) #+facet_grid(~variable)+theme_bw()
# 
# p
# 
# p = ggplot(droplevels(subset(protot_melt_152k, variable !='difftot'& variable !='diffwat'& variable !='diffdry')), aes(x = variable, y = value ))
# p = p + geom_violin()+theme_bw()   +ylim(50,200)
# p

stats_15 = ddply(protot_melt_152k, .(variable) ,summarize, count = length(value),avg = mean(value, na.rm = T), stdev = sd(value, na.rm = T), mae = mean(abs(value),na.rm = T))

#2016

protot_162k = join(pro_162k@data, proall_162k@data, by = names(pro_162k@data)[1:10], type = 'left')
protot_162k = join(protot_162k, prodry_162k@data, by = names(pro_162k@data)[1:10], type = 'left')
protot_162k = join(protot_162k, prowat_162k@data, by = names(pro_162k@data)[1:10], type = 'left')
protot_162k = join(protot_162k, proallcount_162k@data, by = names(pro_162k@data)[1:10], type = 'left')
protot_162k = join(protot_162k, prodry_162k_count@data, by = names(pro_162k@data)[1:10], type = 'left')
protot_162k = join(protot_162k, prowatcount_162k@data, by = names(pro_162k@data)[1:10], type = 'left')
names(protot_162k) = c(names(pro_162k@data)[1:10], 'Mod' , 'FieldTot', 'FieldDry',  'FieldWat','counttot','countdry','countwat')
protot_162k_tot = droplevels(subset(protot_162k, !is.nan(Mod) & counttot >0 ))
# protot_16_dry = droplevels(subset(protot_16, !is.nan(Mod) & countdry > 125 ))
# protot_16_wat = droplevels(subset(protot_16, !is.nan(Mod) & countwat > 125))

# profull_16= rbind(protot_16_tot,protot_16_dry,protot_16_wat)
profull_162k = protot_162k_tot
# profull_16$FieldDry [which(profull_16$countdry < 0.5*profull_16$countwat)] = NA
# profull_16$FieldWat [which(profull_16$countwat < 0.5*profull_16$countdry)] = NA
#protot_16 = droplevels(subset(protot_16,  counttot > 50 ))
profull_162k$difftot = profull_162k$Mod - profull_162k$FieldTot
profull_162k$diffwat = profull_162k$Mod - profull_162k$FieldWat
profull_162k$diffdry = profull_162k$Mod - profull_162k$FieldDry

protot_melt_162k = melt(profull_162k, measure.vars = c('Mod' , 'FieldTot',  'FieldWat', 'FieldDry', 'difftot','diffwat','diffdry'))



# p = ggplot(droplevels(subset(protot_melt_162k, variable !='difftot'& variable !='diffwat'& variable !='diffdry' & variable != 'FieldTot')), aes(x = value, color = variable ))
# # p =p+ stat_bin(binwidth = 8, aes(y=..count../sum(..count..)))+facet_grid(~variable)+theme_bw()
# # p = p+geom_histogram(binwidth = 8)+facet_grid(~variable)+theme_bw()
# p = p + geom_freqpoly(binwidth = 8) #+facet_grid(~variable)+theme_bw()
# #p = p+geom_boxplot(adjust = 1.4)+facet_grid(variable~sowing_met)+theme_bw()+geom_jitter()
# p = p+geom_density(alpha = 0.2, adjust = 1) #+facet_grid(~variable)+theme_bw()
# 
# p
# 
# p = ggplot(droplevels(subset(protot_melt_162k, variable !='difftot'& variable !='diffwat'& variable !='diffdry')), aes(x = variable, y = value ))
# p = p + geom_violin()+theme_bw()   +ylim(50,200)
# p

stats_16 = ddply(protot_melt_162k, .(variable) ,summarize, count = length(value),avg = mean(value, na.rm = T), stdev = sd(value, na.rm = T), mae = mean(abs(value),na.rm = T))



profull_142k$Year = 2014
profull_152k$Year = 2015
profull_162k$Year = 2016

profull_14_15_162k = rbind(profull_142k,profull_152k,profull_162k)
protot_melt_14_15_162k = melt(profull_14_15_162k, measure.vars = c('Mod' , 'FieldTot',  'FieldWat', 'FieldDry', 'difftot','diffwat','diffdry'))
stats_14_15_162k = ddply(protot_melt_14_15_162k, .(variable,Year) ,summarize, count = length(value),avg = mean(value, na.rm = T), stdev = sd(value, na.rm = T), mae = mean(abs(value),na.rm = T))

it_2k = profull_14_15_162k
it_2k_melt  = protot_melt_14_15_162k
it_stats_14_15_16 = ddply(subset(it_2k_melt),.(variable) ,summarize, count = length(value),avg = mean(value, na.rm = T), stdev = sd(value, na.rm = T), mae = mean(abs(value),na.rm = T))
save(it_2k,it_2k_melt,it_stats_14_15_16,file = file.path(out_folder,'IT_val_2k.RData'))


# 
# p = ggplot(droplevels(subset(protot_melt_14_152k, variable !='difftot'& variable !='diffwat'& variable !='diffdry')), aes(x = variable, y = value ))
# p = p + geom_boxplot()+theme_bw()   +ylim(50,200)  + geom_jitter()
# p
# 
# p = ggplot(droplevels(subset(protot_melt_14_152k, variable !='difftot'& variable !='diffwat'& variable !='diffdry'& variable !='FieldTot')), aes(x = value, color = variable ))
# p =p+ stat_bin(binwidth = 8, aes(y=..count../sum(..count..)))+facet_grid(~variable)+theme_bw()
# p = p+geom_histogram(binwidth =8)+facet_grid(~variable)+theme_bw()
# p = p + geom_freqpoly(binwidth = 8) #+facet_grid(~variable)+theme_bw()
# #p = p+geom_boxplot(adjust = 1.4)+facet_grid(variable~sowing_met)+theme_bw()+geom_jitter()
# p = p+geom_density(alpha = 0.2, adjust = 2) #+facet_grid(~variable)+theme_bw()
# 
# p
# 

