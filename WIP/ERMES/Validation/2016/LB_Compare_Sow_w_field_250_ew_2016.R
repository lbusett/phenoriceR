library(plyr)
library(ggplot2)
library(reshape2)
library(tools)
library(rgdal)
library(raster)
library(data.table)
library(gdalUtils)
library(mapview)


es_shape_file_16 = "/home/lb/projects/ermes/datasets/Field_data/2016/Spain/Static_info/ES_Field_data_static_2016_20161221.shp"
es_raster_file_16 = '/home/lb/projects/ermes/datasets/rs_products/Phenology/ES/2016/v1.0/Outputs/2016/Phenorice_ES_2016.dat'
es_2km_tif_16 = '/home/lb/projects/ermes/datasets/rs_products/Phenology/ES/2016/v1.0/Outputs/ERMES_Grid/TIFFS/2016/MinDoys/ES_Phenology_MinDoys_2016_203.tif'

es_shape_file_15 = '/home/lb/projects/ermes/datasets/Field_data/2015/Spain/ES_Static_info/ES_Static_Shapefile_2015.shp'
es_raster_file_15 = '/home/lb/projects/ermes/datasets/rs_products/Phenology/ES/2016/v1.0/Outputs/2015/Phenorice_ES_2015.dat'
es_2km_tif_15 = '/home/lb/projects/ermes/datasets/rs_products/Phenology/ES/2016/v1.0/Outputs/ERMES_Grid/TIFFS/2015/MinDoys/ES_Phenology_MinDoys_2015.tif'
    
es_shape_file_14 = '/home/lb/projects/ermes/datasets/Field_data/2014/Spain/ES_Static_info/ES_Static_Shapefile_2014.shp'
es_raster_file_14 = '/home/lb/projects/ermes/datasets/rs_products/Phenology/ES/2016/v1.0/Outputs/2014/Phenorice_ES_2014.dat'
es_2km_tif_14 = '/home/lb/projects/ermes/datasets/rs_products/Phenology/ES/2016/v1.0/Outputs/ERMES_Grid/TIFFS/2014/MinDoys/ES_Phenology_MinDoys_2014.tif'

es_grid_reduced = '/home/lb/phenorice/PhenoRice/Processing/Validation_Ermes/ES/Accessory/Grid_MODIS_250_reduced_red.shp'

out_folder_gen = "/home/lb/phenorice/PhenoRice/Processing/Validation_Ermes/2016"
dir.create(out_folder_gen, recursive = TRUE)
Accessory_folder = file.path(out_folder_gen, 'Accessory')


#Inizio calcoli -----

#2014
name_shape_14_sinu = file.path(Accessory_folder, 'Static_info_2014_sinu.shp')
name_shape_14_sinu_water = file.path(Accessory_folder, 'Static_info_2014_sinu_water.shp')
name_shape_14_sinu_dry =    file.path(Accessory_folder, 'Static_info_2014_sinu_dry.shp')

name_rast_14_sinu = file.path(Accessory_folder, 'Static_info_2014_sinu.tiff')
name_rast_14_sinu_water = file.path(Accessory_folder, 'Static_info_2014_sinu_water.tiff')
name_rast_14_sinu_dry =    file.path(Accessory_folder,'Static_info_2014_sinu_dry.tiff')

out_folder = file.path(out_folder_gen, "ES")
dir.create(out_folder, recursive = TRUE)
Accessory_folder = file.path(out_folder, 'Accessory')
es_grid_reduced = file.path(out_folder,'Accessory','Grid_MODIS_250_reduced_ES_red.shp')
rast_grid  = file.path(out_folder,'Accessory','Grid_MODIS_250_reduced_ES.tif')


es_shape_14 = readOGR (dirname(es_shape_file_14), file_path_sans_ext(basename(es_shape_file_14)))
es_shape_14$sowing_doy = datetodoy(es_shape_14$Sowing_dat)
es_rast_14 = raster(es_raster_file_14, band = 2)
es_2km_14 = raster(es_2km_tif_14)
# es_grid_sp = readOGR(dirname(es_grid_reduced), file_path_sans_ext(basename(es_grid_reduced)))
raster::values(es_rast_14)[which(raster::values(es_rast_14) > 180)] = NA
raster::values(es_rast_14)[which(raster::values(es_rast_14) == 0)] = NA


es_grid_250 = readOGR(dsn = dirname(es_grid_reduced),layer = basename(file_path_sans_ext(es_grid_reduced)))
es_shape_14 = readOGR (dirname(es_shape_file_14), basename(file_path_sans_ext((es_shape_file_14))))
es_shape_14$sowing_doy = datetodoy(es_shape_14$Sowing_dat)
levels(es_shape_14@data$Sowing_met) = c("Water","Water","Water")
crop_rast_14 = crop(es_rast_14, extent(es_grid_250))
es_shape_14@data$Sowing_met[which(is.na(es_shape_14@data$Sowing_met))] = 'Unknown'
# es_shape_14@data$Sowing_met[which(es_shape_14@data$Sowing_met == 'Unknown' & es_shape_14@data$sowing_doy< 120)] = 'Dry'
# es_shape_14@data$Sowing_met[which(es_shape_14@data$Sowing_met == 'Unknown' & es_shape_14@data$sowing_doy >= 120)] = 'Water'
es_shape_14@data$crop_type = "Rice"
es_shape_14 = es_shape_14[!is.na(es_shape_14@data$crop_type == 'Rice'),]
es_shape_14 = es_shape_14[es_shape_14@data$crop_type == 'Rice' &
                            es_shape_14@data$Sowing_met != 'Unknown' &
                            !is.na(es_shape_14@data$Sowing_dat),]

shape_14_sinu = spTransform(es_shape_14,CRS(proj4string(crop_rast_14)))
shape_14_water = spTransform(es_shape_14[es_shape_14@data$Sowing_met == 'Water' ,],CRS(proj4string(crop_rast_14)))
# shape_14_dry = spTransform(es_shape_14[es_shape_14@data$Sowing_met == 'Dry' ,],CRS(proj4string(crop_rast_14)))

writeOGR(shape_14_sinu, dsn = Accessory_folder,layer  = basename(file_path_sans_ext(name_shape_14_sinu)), driver = 'ESRI Shapefile',overwrite_layer = T )
writeOGR(shape_14_water, dsn = Accessory_folder,layer = basename(file_path_sans_ext(name_shape_14_sinu_water)), driver = 'ESRI Shapefile',overwrite_layer = T )
# writeOGR(shape_14_dry, dsn = Accessory_folder  ,layer = basename(file_path_sans_ext(name_shape_14_sinu_dry)), driver = 'ESRI Shapefile',overwrite_layer = T )

raster_obs_14 = gdal_rasterize(name_shape_14_sinu, name_rast_14_sinu,a = "sowing_doy", output_Raster = T, ot = 'UInt32',tr = c(15,15), tap = T, of = 'GTiff')
raster_obs_wat_14 = gdal_rasterize(name_shape_14_sinu_water, name_rast_14_sinu_water,a = "sowing_doy", output_Raster = T, ot = 'UInt32',tr = c(15,15), tap = T, of = 'GTiff')
# raster_obs_14_dry = gdal_rasterize(name_shape_14_sinu_dry , name_rast_14_sinu_dry,a = "sowing_doy", output_Raster = T, ot = 'UInt32',tr = c(15,15), tap = T, of = 'GTiff')

NAvalue(raster_obs_14) = 0
NAvalue(raster_obs_wat_14) = 0
# NAvalue(raster_obs_14_dry) = 0

pro_14 = ZonalPipe(es_grid_reduced,crop_rast_14,rast_grid,'pippo',
                   'id', stat="mean", cut =T)
# 
# prodry_14 = ZonalPipe(es_grid_reduced,raster_obs_14_dry,rast_grid, 'pippo',
#                       'id', stat="mean", cut = F)

# prodry_14_count = ZonalPipe(es_grid_reduced,raster_obs_14_dry,rast_grid, 'pippo',
#                             'id', stat="countna", cut = F)

prowat_14 = ZonalPipe(es_grid_reduced,raster_obs_wat_14,rast_grid, 'pippo',
                      'id', stat="mean", cut = F)

prowatcount_14 = ZonalPipe(es_grid_reduced,raster_obs_wat_14,rast_grid, 'pippo',
                           'id', stat="countna", cut = F)

proall_14 = ZonalPipe(es_grid_reduced,raster_obs_14,rast_grid, 'pippo',
                      'id', stat="mean", cut = F)

proallcount_14 = ZonalPipe(es_grid_reduced,raster_obs_14,rast_grid, 'pippo',
                           'id', stat="countna", cut = F)


#2015

name_shape_15_sinu = file.path(Accessory_folder, 'Static_info_2015_sinu.shp')
name_shape_15_sinu_water = file.path(Accessory_folder, 'Static_info_2015_sinu_water.shp')
# name_shape_15_sinu_dry =    file.path(Accessory_folder, 'Static_info_2015_sinu_dry.shp')

name_rast_15_sinu = file.path(Accessory_folder, 'Static_info_2015_sinu.tiff')
name_rast_15_sinu_water = file.path(Accessory_folder, 'Static_info_2015_sinu_water.tiff')
# name_rast_15_sinu_dry =    file.path(Accessory_folder,'Static_info_2015_sinu_dry.tiff')

es_shape_15 = readOGR (dirname(es_shape_file_15), file_path_sans_ext(basename(es_shape_file_15)))
es_rast_15 = raster(es_raster_file_15, band = 2)
es_2km_15 = raster(es_2km_tif_15)
# es_grid_sp = readOGR(dirname(es_grid_reduced), file_path_sans_ext(basename(es_grid_reduced)))
raster::values(es_rast_15)[which(raster::values(es_rast_15) > 180)] =NA
raster::values(es_rast_15)[which(raster::values(es_rast_15) == 0)] =NA

es_shape_15 = readOGR (dirname(es_shape_file_15), basename(file_path_sans_ext((es_shape_file_15))))
es_shape_15$sowing_doy = datetodoy(es_shape_15$Sowing_dat)
levels(es_shape_15@data$Sowing_met) = c("Water","Water")
crop_rast_15 = crop(es_rast_15, extent(es_grid_250))
# es_shape_15@data$Sowing_met[which(is.na(es_shape_15@data$Sowing_met))] = 'Unknown'
# es_shape_15@data$Sowing_met[which(es_shape_14@data$Sowing_met == 'Unknown' & es_shape_14@data$sowing_doy< 120)] = 'Dry'
# es_shape_14@data$Sowing_met[which(es_shape_14@data$Sowing_met == 'Unknown' & es_shape_14@data$sowing_doy >= 120)] = 'Water'
es_shape_15@data$crop_type = "Rice"
es_shape_15 = es_shape_15[!is.na(es_shape_15@data$crop_type == 'Rice'),]
es_shape_15 = es_shape_15[es_shape_15@data$crop_type == 'Rice' &
                            es_shape_15@data$Sowing_met != 'Unknown' &
                              !is.na(es_shape_15@data$Sowing_dat),]

shape_15_sinu = spTransform(es_shape_15,CRS(proj4string(crop_rast_15)))
shape_15_water = spTransform(es_shape_15[es_shape_15@data$Sowing_met == 'Water' ,],CRS(proj4string(crop_rast_15)))
# shape_15_dry = spTransform(es_shape_15[es_shape_15@data$Sowing_met == 'Dry' ,],CRS(proj4string(crop_rast_15)))

writeOGR(shape_15_sinu, dsn = Accessory_folder,layer  = basename(file_path_sans_ext(name_shape_15_sinu)), driver = 'ESRI Shapefile',overwrite_layer = T )
writeOGR(shape_15_water, dsn = Accessory_folder,layer = basename(file_path_sans_ext(name_shape_15_sinu_water)), driver = 'ESRI Shapefile',overwrite_layer = T )
# writeOGR(shape_15_dry, dsn = Accessory_folder  ,layer = basename(file_path_sans_ext(name_shape_15_sinu_dry)), driver = 'ESRI Shapefile',overwrite_layer = T )

raster_obs_15 = gdal_rasterize(name_shape_15_sinu, name_rast_15_sinu,a = "Sowing_dat", output_Raster = T, ot = 'UInt32',tr = c(15,15), tap = T, of = 'GTiff')
raster_obs_wat_15 = gdal_rasterize(name_shape_15_sinu_water, name_rast_15_sinu_water,a = "Sowing_dat", output_Raster = T, ot = 'UInt32',tr = c(15,15), tap = T, of = 'GTiff')
# raster_obs_15_dry = gdal_rasterize(name_shape_15_sinu_dry , name_rast_15_sinu_dry,a = "sowing_doy", output_Raster = T, ot = 'UInt32',tr = c(15,15), tap = T, of = 'GTiff')

NAvalue(raster_obs_15) = 0
NAvalue(raster_obs_wat_15) = 0
# NAvalue(raster_obs_15_dry) = 0

pro_15 = ZonalPipe(es_grid_reduced,crop_rast_15,rast_grid,'pippo',
                   'id', stat="mean", cut = T)

# prodry_15 = ZonalPipe(es_grid_reduced,raster_obs_15_dry,rast_grid, 'pippo',
#                       'id', stat="mean", cut = F)

# prodry_15_count = ZonalPipe(es_grid_reduced,raster_obs_15_dry,rast_grid, 'pippo',
#                             'id', stat="countna", cut = F)

prowat_15 = ZonalPipe(es_grid_reduced,raster_obs_wat_15,rast_grid, 'pippo',
                      'id', stat="mean", cut = F)

prowatcount_15 = ZonalPipe(es_grid_reduced,raster_obs_wat_15,rast_grid, 'pippo',
                           'id', stat="countna", cut = F)

proall_15 = ZonalPipe(es_grid_reduced,raster_obs_15,rast_grid, 'pippo',
                      'id', stat="mean", cut = F)

proallcount_15 = ZonalPipe(es_grid_reduced,raster_obs_15,rast_grid, 'pippo',
                           'id', stat="countna", cut = F)

#2016

name_shape_16_sinu = file.path(Accessory_folder, 'Static_info_2016_sinu.shp')
name_shape_16_sinu_water = file.path(Accessory_folder, 'Static_info_2016_sinu_water.shp')
name_shape_16_sinu_dry =    file.path(Accessory_folder, 'Static_info_2016_sinu_dry.shp')

name_rast_16_sinu = file.path(Accessory_folder, 'Static_info_2016_sinu.tiff')
name_rast_16_sinu_water = file.path(Accessory_folder, 'Static_info_2016_sinu_water.tiff')
name_rast_16_sinu_dry =    file.path(Accessory_folder,'Static_info_2016_sinu_dry.tiff')

es_shape_16 = readOGR (dirname(es_shape_file_16), file_path_sans_ext(basename(es_shape_file_16)))
# es_shape_16$sowing_doy = datetodoy(es_shape_16$Sowing_dat)
es_rast_16 = raster(es_raster_file_16, band = 2)
es_2km_16 = raster(es_2km_tif_16)
# es_grid_sp = readOGR(dirname(es_grid_reduced), file_path_sans_ext(basename(es_grid_reduced)))
raster::values(es_rast_16)[which(raster::values(es_rast_16) > 180)] =NA
raster::values(es_rast_16)[which(raster::values(es_rast_16) == 0)] =NA

es_shape_16 = readOGR (dirname(es_shape_file_16), basename(file_path_sans_ext((es_shape_file_16))))
levels(es_shape_16@data$sowing_met) = c("Water","Water","Water")
crop_rast_16 = crop(es_rast_16, extent(es_grid_250))
es_shape_16@data$sowing_met[which(is.na(es_shape_16@data$sowing_met))] = 'Water'
# es_shape_16@data$Sowing_met[which(es_shape_14@data$Sowing_met == 'Unknown' & es_shape_14@data$sowing_doy< 120)] = 'Dry'
# es_shape_14@data$Sowing_met[which(es_shape_14@data$Sowing_met == 'Unknown' & es_shape_14@data$sowing_doy >= 120)] = 'Water'

es_shape_16 = es_shape_16[!is.na(es_shape_16@data$crop_type == 'Rice'),]
es_shape_16 = es_shape_16[es_shape_16@data$crop_type == 'Rice' &
                            es_shape_16@data$sowing_met != 'Unknown' &
                            !is.na(es_shape_16@data$sowing_doy),]

shape_16_sinu = spTransform(es_shape_16,CRS(proj4string(crop_rast_16)))
shape_16_water = spTransform(es_shape_16[es_shape_16@data$sowing_met == 'Water' ,],CRS(proj4string(crop_rast_16)))
#shape_16_dry = spTransform(es_shape_16[es_shape_16@data$sowing_met == 'Dry' ,],CRS(proj4string(crop_rast_16)))

writeOGR(shape_16_sinu, dsn = Accessory_folder,layer  = basename(file_path_sans_ext(name_shape_16_sinu)), driver = 'ESRI Shapefile',overwrite_layer = T )
writeOGR(shape_16_water, dsn = Accessory_folder,layer = basename(file_path_sans_ext(name_shape_16_sinu_water)), driver = 'ESRI Shapefile',overwrite_layer = T )
# writeOGR(shape_16_dry, dsn = Accessory_folder  ,layer = basename(file_path_sans_ext(name_shape_16_sinu_dry)), driver = 'ESRI Shapefile',overwrite_layer = T )

raster_obs_16 = gdal_rasterize(name_shape_16_sinu, name_rast_16_sinu,a = "sowing_doy", output_Raster = T, ot = 'UInt32',tr = c(16,16), tap = T, of = 'GTiff')
raster_obs_wat_16 = gdal_rasterize(name_shape_16_sinu_water, name_rast_16_sinu_water,a = "sowing_doy", output_Raster = T, ot = 'UInt32',tr = c(16,16), tap = T, of = 'GTiff')
# raster_obs_16_dry = gdal_rasterize(name_shape_16_sinu_dry , name_rast_16_sinu_dry,a = "sowing_doy", output_Raster = T, ot = 'UInt32',tr = c(16,16), tap = T, of = 'GTiff')

NAvalue(raster_obs_16) = 0
NAvalue(raster_obs_wat_16) = 0
# NAvalue(raster_obs_16_dry) = 0

pro_16 = ZonalPipe(es_grid_reduced,crop_rast_16,rast_grid,'pippo',
                   'id', stat="mean", cut = T)

# prodry_16 = ZonalPipe(es_grid_reduced,raster_obs_16_dry,rast_grid, 'pippo',
                      # 'id', stat="mean", cut = F)

# prodry_16_count = ZonalPipe(es_grid_reduced,raster_obs_16_dry,rast_grid, 'pippo',
                            # 'id', stat="countna", cut = F)

prowat_16 = ZonalPipe(es_grid_reduced,raster_obs_wat_16,rast_grid, 'pippo',
                      'id', stat="mean", cut = F)

prowatcount_16 = ZonalPipe(es_grid_reduced,raster_obs_wat_16,rast_grid, 'pippo',
                           'id', stat="countna", cut = F)

proall_16 = ZonalPipe(es_grid_reduced,raster_obs_16,rast_grid, 'pippo',
                      'id', stat="mean", cut = F)

proallcount_16 = ZonalPipe(es_grid_reduced,raster_obs_16,rast_grid, 'pippo',
                           'id', stat="countna", cut = F)



#Fine calcoli - inizio analisi e plotting ----

protot_14 = join(pro_14@data, proall_14@data, by = c('id','z'), type = 'left')

protot_14 = join(protot_14, proallcount_14@data, by = c('id','z'), type = 'left')

names(protot_14) = c('id' ,     'z', 'Mod' , 'FieldTot','counttot')
protot_14_tot = droplevels(subset(protot_14, !is.nan(Mod) & counttot > 0))

profull_14 = protot_14_tot
profull_14$difftot = profull_14$Mod - profull_14$FieldTot
protot_melt_14 = melt(profull_14, measure.vars = c('Mod' , 'FieldTot',   'difftot'))


protot_15 = join(pro_15@data, proall_15@data, by = c('id','z'), type = 'left')
protot_15 = join(protot_15, proallcount_15@data, by = c('id','z'), type = 'left')
names(protot_15) = c('id' ,     'z', 'Mod' , 'FieldTot','counttot')
protot_15_tot = droplevels(subset(protot_15, !is.nan(Mod) & counttot > 0 ))

profull_15 = protot_15_tot
profull_15$difftot = profull_15$Mod - profull_15$FieldTot
protot_melt_15 = melt(profull_15, measure.vars = c('Mod' , 'FieldTot', 'difftot'))

protot_16 = join(pro_16@data, proall_16@data, by = c('id','z'), type = 'left')
protot_16 = join(protot_16, proallcount_16@data, by = c('id','z'), type = 'left')
names(protot_16) = c('id' ,     'z', 'Mod' , 'FieldTot','counttot')
protot_16_tot = droplevels(subset(protot_16, !is.nan(Mod) & counttot > 0 ))

profull_16 = protot_16_tot
profull_16$difftot = profull_16$Mod - profull_16$FieldTot
protot_melt_16 = melt(profull_16, measure.vars = c('Mod' , 'FieldTot', 'difftot'))


profull_14$Year =2014
profull_15$Year =2015
profull_16$Year =2016

profull_14_15_16 = rbind(profull_14,profull_15,profull_16)
protot_melt_14_15_16 = melt(profull_14_15, measure.vars = c('Mod' , 'FieldTot',   'difftot'))
stats_14_15_16 = ddply(subset(es_250_melt,counttot > 125), .(variable,Year) ,summarize, count = length(value),avg = mean(value, na.rm = T), stdev = sd(value, na.rm = T), mae = mean(abs(value),na.rm = T))

es_250 = profull_14_15_16
es_250_melt  = protot_melt_14_15_16
save(es_250,es_250_melt, file = 'd:/temp/phenorice/processing/Validation_Ermes/ES/ES_val_250.RData')


#END ----

# 
# p = ggplot(droplevels(subset(protot_melt_14, variable !='difftot'& variable !='diffwat'& variable !='diffdry' & variable != 'FieldTot')), aes(x = value, color = variable ))
# # p =p+ stat_bin(binwidth = 8, aes(y=..count../sum(..count..)))+facet_grid(~variable)+theme_bw()
# # p = p+geom_histogram(binwidth = 8)+facet_grid(~variable)+theme_bw()
# p = p + geom_freqpoly(binwidth = 8) #+facet_grid(~variable)+theme_bw()
# #p = p+geom_boxplot(adjust = 1.4)+facet_grid(variable~Sowing_met)+theme_bw()+geom_jitter()
# p = p+geom_density(alpha = 0.2, adjust = 1) #+facet_grid(~variable)+theme_bw()
# 
# p
# 
# p = ggplot(droplevels(subset(protot_melt_14_15_16, variable !='difftot'& variable !='diffwat'& variable !='diffdry')), aes(x = variable, y = value ))
# p = p + geom_violin()+theme_bw()   +ylim(50,200)
# p
# 
# 
# 
# p = ggplot(droplevels(subset(protot_melt_14_15_16, variable !='difftot'& variable !='diffwat'& variable !='diffdry')), aes(x = value, color = variable ))
# p =p+ stat_bin(binwidth = 8, aes(y=..count../sum(..count..)))+facet_grid(~variable)+theme_bw()
# p = p+geom_histogram(binwidth = 8)+facet_grid(~variable)+theme_bw()
# p = p + geom_freqpoly(binwidth = 16) #+facet_grid(~variable)+theme_bw()
# #p = p+geom_boxplot(adjust = 1.4)+facet_grid(variable~Sowing_met)+theme_bw()+geom_jitter()
# p = p+geom_density(alpha = 0.2, adjust = 1.2) #+facet_grid(~variable)+theme_bw()
# 
# p
# 
# p = ggplot(droplevels(subset(protot_melt_14_15_16, variable !='difftot'& variable !='diffwat'& variable !='diffdry')), aes(x = variable, y = value ))
# p = p + geom_boxplot()+theme_bw()   +ylim(50,200)
# p
# 
# 
# p = ggplot(droplevels(subset(protot_melt_14_15_16, variable !='difftot'& variable !='diffwat'& variable !='diffdry')), aes(x = variable, y = value ))
# p = p + geom_boxplot()+theme_bw()   +ylim(50,200)  + geom_jitter()
# p
# 
# p = ggplot(droplevels(subset(protot_melt_14_15_16, variable !='difftot'& variable !='diffwat'& variable !='diffdry'& variable !='FieldTot')), aes(x = value, color = variable ))
# p =p+ stat_bin(binwidth = 8, aes(y=..count../sum(..count..)))+facet_grid(~variable)+theme_bw()
# p = p+geom_histogram(binwidth = 8)+facet_grid(~variable)+theme_bw()
# p = p + geom_freqpoly(binwidth = 8) #+facet_grid(~variable)+theme_bw()
# #p = p+geom_boxplot(adjust = 1.4)+facet_grid(variable~Sowing_met)+theme_bw()+geom_jitter()
# p = p+geom_density(alpha = 0.2, adjust = 1.2) #+facet_grid(~variable)+theme_bw()
# 
# p
# 
