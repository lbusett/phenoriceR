# TODO: Add comment
# 
# Author: LB
###############################################################################
LB_Create_pol_grid = function(in_raster_file, out_res, out_folder, out_name_shp){
	
	library(rgdal)
	library(raster)
	rast<- raster(in_raster_file)
	bb <- bbox(rast)                          # Define Bounding box
	cs <- c(out_res,out_res)  # cell size. To be set by hand! 
	
	cc <- bb[, 1] + (cs/2)  # cell offset   # move 1/2 pixel if necessary
	
	cd <- ceiling(diff(t(bb))/cs)  # compute number of cells per direction
	
	grd <- GridTopology(cellcentre.offset=cc, cellsize=cs, cells.dim=cd)   # Define grd characteristics
	
	#transform to spatial grid
	sp_grd <- SpatialGridDataFrame(grd,
			data=data.frame(id=1:prod(cd)),
			proj4string=CRS(proj4string(rast)))
	summary(sp_grd)
	
	sp_polygons = as(sp_grd, "SpatialPolygonsDataFrame")#create the shape
	writeOGR(sp_polygons,dsn = out_folder, layer =out_shape,driver="ESRI Shapefile", overwrite_layer = T) #save the shapefile
}


in_raster_file = 'W:/francesco/PhenoRice_processing/PHL/Output_t7/2014/raster/EVI_WLmask_PHL__map_max_2014.dat'
out_folder <-  'W:/francesco/PhenoRice_processing/PHL/Analysis/GRID' #save the grid here
out_shape <- 'PHL_Grid_5Km' #this is the grid name
out_res = 10192.864/2

LB_Create_pol_grid(in_raster_file , out_res,out_folder, out_shape)