# TODO: Add comment
#
# Author: LB
###############################################################################

# LB_Create_pol_grid = function(in_raster_file, out_res, out_folder, out_name_shp,grid_extent){
#
# 	library(rgdal)
# 	library(raster)
# 	rast<- raster('Y:/ermes/datasets/rs_products/Phenology/ES/Outputs/v4.0/2015/raster/old_min_identification_Full_Out_2015.dat')
# 	out_res = 231.656358
# 	grid_extent = extent(rast)
# 	#	bb <- bbox(rast)                          # Define Bounding box
# 	cs <- c(out_res,out_res)  # cell size. To be set by hand!
#
# 	cc <- c(grid_extent@xmin,grid_extent@ymin) + (cs/2)  # cell offset   # move 1/2 pixel if necessary
#
# 	# compute number of cells per direction
# 	if (cs < 300) {				# needed to avoid that larger grids cover only one portion of the area --> so, add a row/coulmn for larger grids
# 		cd <- c(dim(rast)[2],dim(rast)[1])
# 	} else {
# 		cd <- ceiling(c(((grid_extent@xmax-grid_extent@xmin)/cs[1]),((grid_extent@ymax-grid_extent@ymin)/cs[2])))
# 	}
# 	grd <- GridTopology(cellcentre.offset=cc, cellsize=cs, cells.dim=cd)   # Define grd characteristics
#
# 	#transform to spatial grid
# 	sp_grd <- SpatialGridDataFrame(grd,
# 			data=data.frame(id=1:prod(cd)),
# 			proj4string=CRS(proj4string(rast)))
# 	summary(sp_grd)
# 	sp_polygons = as(sp_grd, "SpatialPolygonsDataFrame")#create the shape
# 	print('Creating MODIS Grid - Please Wait !')
# 	writeOGR(sp_polygons,dsn = 'D:/Temp/PhenoRice/Processing/Validation_Ermes/ES/Accessory', layer = 'Grid_MODIS_250_reduced_ES', driver="ESRI Shapefile", overwrite_layer = T) #save the shapefile
# 	gc()
# # }


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
es_2km_tif_15 = 'Y:/ermes/datasets/rs_products/Phenology/ES/Outputs/v4.0/ERMES_Grid/TIFFS/2015/MinDoys/ES_Phenology_MinDoys_2015_004.tif'

es_shape_file_14 = 'D:/Temp/PhenoRice/Processing/Validation_Ermes/Input/ES_Static_info_2014.shp'
es_raster_file_14 = 'Y:/ermes/datasets/rs_products/Phenology/ES/Outputs/v1.0/2014/raster/old_min_identification_Full_Out_2014.dat'
es_2km_tif_14 = 'Y:/ermes/datasets/rs_products/Phenology/ES/Outputs/v1.0/ERMES_Grid/TIFFS/2014/MinDoys/ES_Phenology_MinDoys_2014_001.tif'

es_grid_reduced = 'D:/Temp/PhenoRice/Processing/Validation_Ermes/ES/Accessory/Grid_MODIS_250_reduced_ES_red.shp'

out_folder = "D:/Temp/PhenoRice/Processing/Validation_Ermes/ES"
Accessory_folder = file.path(out_folder, 'Accessory')

#Inizio calcoli -----

#2014
name_shape_14_sinu = file.path(Accessory_folder, 'Static_info_2014_sinu.shp')
name_rast_14_sinu = file.path(Accessory_folder, 'Static_info_2014_sinu.tiff')

name_shape_15_sinu = file.path(Accessory_folder, 'Static_info_2015_sinu.shp')
name_rast_15_sinu = file.path(Accessory_folder, 'Static_info_2015_sinu.tiff')

es_grid_reduced = file.path(out_folder,'Accessory','Grid_MODIS_250_reduced_ES_red.shp')
rast_grid  = file.path(out_folder,'Accessory','Grid_MODIS_250_reduced_red.tif')


es_shape_14 = readOGR (dirname(es_shape_file_14), file_path_sans_ext(basename(es_shape_file_14)))
es_rast_14 = raster(es_raster_file_14, band = 2)
es_2km_14 = raster(es_2km_tif_14)
raster::values(es_rast_14)[which(raster::values(es_rast_14) > 180)] =NA
raster::values(es_rast_14)[which(raster::values(es_rast_14) == 0 )] =NA


es_grid_250 = readOGR(dsn = dirname(es_grid_reduced),layer = basename(file_path_sans_ext(es_grid_reduced)))
es_shape_14 = readOGR (dirname(es_shape_file_14), basename(file_path_sans_ext((es_shape_file_14))))
es_shape_14@data$sowing_met = "Water"
crop_rast_14 = crop(es_rast_14, extent(es_grid_250))

es_shape_14 = es_shape_14[!is.na(es_shape_14@data$crop_type == 'Rice'),]
es_shape_14 = es_shape_14[es_shape_14@data$crop_type == 'Rice' &
				es_shape_14@data$sowing_met != 'Unknown' &
				!is.na(es_shape_14@data$sowing_doy),]

shape_14_sinu = spTransform(es_shape_14,CRS(proj4string(crop_rast_14)))

writeOGR(shape_14_sinu, dsn = Accessory_folder,layer  = basename(file_path_sans_ext(name_shape_14_sinu)), driver = 'ESRI Shapefile',overwrite_layer = T )

raster_obs_14 = gdal_rasterize(name_shape_14_sinu, name_rast_14_sinu,a = "sowing_doy", output_Raster = T, ot = 'UInt32',tr = c(15,15), tap = T, of = 'GTiff')

NAvalue(raster_obs_14) = 0

pro_14 = ZonalPipe(es_grid_reduced,crop_rast_14,rast_grid,'pippo',
		'id', stat="mean", cut = T)

proall_14 = ZonalPipe(es_grid_reduced,raster_obs_14,rast_grid, 'pippo',
		'id', stat="mean", cut = F)

proallcount_14 = ZonalPipe(es_grid_reduced,raster_obs_14,rast_grid, 'pippo',
		'id', stat="countna", cut = F)


#2015

es_shape_15 = readOGR (dirname(es_shape_file_15), file_path_sans_ext(basename(es_shape_file_15)))
es_rast_15 = raster(es_raster_file_15, band = 2)
es_2km_15 = raster(es_2km_tif_15)

raster::values(es_rast_15)[which(raster::values(es_rast_15) > 180)] =NA
raster::values(es_rast_15)[which(raster::values(es_rast_15) == 0)] = NA

es_shape_15@data$sowing_met = "Water"
crop_rast_15 = crop(es_rast_15, extent(es_grid_250))

es_shape_15 = es_shape_15[!is.na(es_shape_15@data$crop_type == 'Rice'),]
es_shape_15 = es_shape_15[es_shape_15@data$crop_type == 'Rice' &
				es_shape_15@data$sowing_met != 'Unknown' &
				!is.na(es_shape_15@data$sowing_doy),]

shape_15_sinu = spTransform(es_shape_15,CRS(proj4string(crop_rast_15)))

writeOGR(shape_15_sinu, dsn = Accessory_folder,layer  = basename(file_path_sans_ext(name_shape_15_sinu)), driver = 'ESRI Shapefile',overwrite_layer = T )

raster_obs_15 = gdal_rasterize(name_shape_15_sinu, name_rast_15_sinu,a = "sowing_doy", output_Raster = T, ot = 'UInt32',tr = c(15,15), tap = T, of = 'GTiff')
#
NAvalue(raster_obs_15) = 0

pro_15 = ZonalPipe(es_grid_reduced,crop_rast_15,rast_grid,'pippo',
		'id', stat="mean", cut = T)

proall_15 = ZonalPipe(es_grid_reduced,raster_obs_15,rast_grid, 'pippo',
		'id', stat="mean", cut = F)

proallcount_15 = ZonalPipe(es_grid_reduced,raster_obs_15,rast_grid, 'pippo',
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
profull_14$Year =2014
profull_15$Year =2015

profull_14_15 = rbind(profull_14,profull_15)
protot_melt_14_15 = melt(profull_14_15, measure.vars = c('Mod' , 'FieldTot',   'difftot'))
stats_14_15 = ddply(subset(es_250_melt,counttot > 125), .(variable,Year) ,summarize, count = length(value),avg = mean(value, na.rm = T), stdev = sd(value, na.rm = T), mae = mean(abs(value),na.rm = T))

es_250 = profull_14_15
es_250_melt  = protot_melt_14_15
save(es_250,es_250_melt, file = 'd:/temp/phenorice/processing/Validation_Ermes/ES/ES_val_250.RData')



p = ggplot(droplevels(subset(protot_melt_14, variable !='difftot')), aes(x = value, color = variable ))
# p =p+ stat_bin(binwidth = 8, aes(y=..count../sum(..count..)))+facet_grid(~variable)+theme_bw()
# p = p+geom_histogram(binwidth = 8)+facet_grid(~variable)+theme_bw()
p = p + geom_freqpoly(binwidth = 8) #+facet_grid(~variable)+theme_bw()
#p = p+geom_boxplot(adjust = 1.4)+facet_grid(variable~sowing_met)+theme_bw()+geom_jitter()
p = p+geom_density(alpha = 0.2, adjust = 1.5)+xlim(100,180) #+facet_grid(~variable)+theme_bw()

p

p = ggplot(droplevels(subset(protot_melt_14, variable !='difftot')), aes(x = variable, y = value ))
p = p + geom_violin()+theme_bw()   +ylim(50,200)
p

stats_14 = ddply(protot_melt_14, .(variable) ,summarize, count = length(value),avg = mean(value, na.rm = T), stdev = sd(value, na.rm = T), mae = mean(abs(value),na.rm = T))


p = ggplot(droplevels(subset(protot_melt_15, variable !='difftot')), aes(x = value, color = variable ))
p =p+ stat_bin(binwidth = 8, aes(y=..count../sum(..count..)))+facet_grid(~variable)+theme_bw()
p = p+geom_histogram(binwidth = 8)+facet_grid(~variable)+theme_bw()
p = p + geom_freqpoly(binwidth = 8) #+facet_grid(~variable)+theme_bw()
#p = p+geom_boxplot(adjust = 1.4)+facet_grid(variable~sowing_met)+theme_bw()+geom_jitter()
p = p+geom_density(alpha = 0.2, adjust = 1.2) #+facet_grid(~variable)+theme_bw()

p

p = ggplot(droplevels(subset(protot_melt_15,  variable !='difftot')), aes(x = variable, y = value ))
p = p + geom_boxplot()+theme_bw()   +ylim(50,200)
p

stats_15 = ddply(protot_melt_15, .(variable) ,summarize, count = length(value),avg = mean(value, na.rm = T), stdev = sd(value, na.rm = T), mae = mean(abs(value),na.rm = T))




p = ggplot(droplevels(subset(protot_melt_14_15, variable !='difftot'& variable !='diffwat'& variable !='diffdry')), aes(x = variable, y = value ))
p = p + geom_boxplot()+theme_bw()   +ylim(50,200)  + geom_jitter()
p

p = ggplot(droplevels(subset(protot_melt_14_15, variable !='difftot')), aes(x = value, color = variable ))
p =p+ stat_bin(binwidth = 8, aes(y=..count../sum(..count..)))+facet_grid(~variable)+theme_bw()
p = p+geom_histogram(binwidth = 8)+facet_grid(~variable)+theme_bw()
p = p + geom_freqpoly(binwidth = 8) #+facet_grid(~variable)+theme_bw()
#p = p+geom_boxplot(adjust = 1.4)+facet_grid(variable~sowing_met)+theme_bw()+geom_jitter()
p = p+geom_density(alpha = 0.2, adjust = 1.2) #+facet_grid(~variable)+theme_bw()

p

