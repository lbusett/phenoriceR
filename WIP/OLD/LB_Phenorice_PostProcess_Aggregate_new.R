# TODO: Add comment
# 
# Author: LB
###############################################################################

LB_Create_pol_grid = function(in_raster_file, out_res, out_folder, out_name_shp,grid_extent){
	
	library(rgdal)
	library(raster)
	rast<- raster(in_raster_file)
#	bb <- bbox(rast)                          # Define Bounding box
	cs <- c(out_res,out_res)  # cell size. To be set by hand! 
	
	cc <- c(grid_extent$xmin,grid_extent$ymin) + (cs/2)  # cell offset   # move 1/2 pixel if necessary

	# compute number of cells per direction
	if (cs < 300) {				# needed to avoid that larger grids cover only one portion of the area --> so, add a row/coulmn for larger grids
		cd <- ceiling(c(((grid_extent$xmax-grid_extent$xmin)/cs[1]),((grid_extent$ymax-grid_extent$ymin)/cs[2])))-c(0,1)
	} else {
		cd <- ceiling(c(((grid_extent$xmax-grid_extent$xmin)/cs[1]),((grid_extent$ymax-grid_extent$ymin)/cs[2])))
	}
	grd <- GridTopology(cellcentre.offset=cc, cellsize=cs, cells.dim=cd)   # Define grd characteristics

	#transform to spatial grid
	sp_grd <- SpatialGridDataFrame(grd,
			data=data.frame(id=1:prod(cd)),
			proj4string=CRS(proj4string(rast)))
	summary(sp_grd)
#	browser()
	sp_polygons = as(sp_grd, "SpatialPolygonsDataFrame")#create the shape
	print('Creating MODIS Grid - Please Wait !')
	writeOGR(sp_polygons,dsn = out_folder, layer =out_name_shp,driver="ESRI Shapefile", overwrite_layer = T) #save the shapefile
	gc()
}

library(ggplot2)
library(reshape)
library(data.table)
library(rgdal)
library(SDMTools)
library(plyr)
library(gdalUtils)
library(raster)
library(foreign)
library(tools)
memory.limit(16000)
{{
		
# Define inputs and outputs
		main_folder = 'Z:/Processing_v21/old_min_identification/Outputs' 
		results_folder = file.path(main_folder,'ndii_new/2013/raster')			# Folder containinf phenorice results
		outputs_folder = file.path(main_folder,'ndii_new/postprocess')		# Folder where to put results of this processing
		temp_files_folder = file.path(main_folder,'normal_new/postprocess/temp')
		#phrice_refmap_orig= file.path(main_folder,'Reference_map','SAR_rice_BINARY_v3_reclass_v2.tif')			#Reference map of rice crop extension - PHL
		phrice_refmap_orig= file.path(main_folder,'Analysis/Reference','SIARL_Rice_2013.dat')			#Reference map of rice crop extension - Lomb
		
		hans_mask_file = 'D:/Temp/PHL_Analysis/Ancillary/Masks/Hansen_Forest_Map/Hansen_GFC2013_treecover2000_20N_120E_GAIN-LOSS_clip_FORESTperc_SINU.tif' #Mask of forests - PHL
		
		in_shape_admin_file = file.path(main_folder,'Analysis/Boundaries/com2011/com2011.shp')   # shape of admin areas - Lomb
#		in_shape_admin_file = file.path(main_folder,'Ancillary/Shapes/Admin_PHL.shp')   # shape of admin areas - PHL
		out_mod_grid_df_file = file.path(outputs_folder,'out_df_modgrid.RData')   # output rdata file used to store results
		dir.create(dirname(out_mod_grid_df_file), recursive = T)
#?? Use a different Dynamic map ??? Add here !!!
		
		dir.create(outputs_folder, recursive = T)
		
		mask_hansen = F
		
# Extent of the "reference grid" used - tipically a subset of the MODIS image including the full extent
# of the "reference" map - the corners specified below should correspond to upper and left corners of
# the subset of the MODIS image to be considered (so, no "random" approximate coordinates)
		grid_folder = file.path(outputs_folder,'GRIDs')   #save the grid here		
		grid_extent = data.frame(xmin = 13557688.37,ymin = 1188165.46, xmax = 13650350.93, ymax =  1280828.02)			#Grid - PHL # Reduce this extent otherwise you will run out of memory !
		
		grid_extent = data.frame(xmin = 612373.40+231.656358*150,ymin = 4961432.228+231.656358*150, xmax = 914453.2908-231.656358*600, ymax =  5087916.60000-231.656358*100)			#Grid - Lomb
		
		grid_resolutions = c(231.656358,2084.907, 5096.432, 10192.864,20385.73 )		# Resolution of output grids
		grid_shapes <- c('Grid_MODIS_250','Grid_2Km','Grid_5Km','Grid_10Km','Grid_20Km')		 #this is the grid name
		dir.create(grid_folder)
		
		
# retrieve filenames of Phrice outputs to be used
		in_file = list.files(results_folder, glob2rx('*Full_Out*dat$'),full.names  = TRUE)
#		in_maxs_file = list.files(results_folder, glob2rx('*map_max*dat'),full.names  = TRUE)
#		in_mins_file = list.files(results_folder, glob2rx('*map_min*dat'),full.names  = TRUE)
#		in_ndetect_file = list.files(results_folder, glob2rx('*map_rice*dat'),full.names  = TRUE)
		
# Definition of names of "Intermediate" files required for processing
		out_ref_raster= file.path(main_folder,'Analysis/Reference','Reproj','Reference_Map_SIN.tif')
		dir.create(dirname(out_ref_raster))
		out_grid_rasters = file.path(temp_files_folder,paste(grid_shapes,'_raster_HR.tiff',sep = ''))
		out_rice_file = file.path(temp_files_folder,'ricefil_clipped.tiff' )
#		out_mins_file = file.path(temp_files_folder,'doys_min_map_clipped.tiff' )
#		out_maxs_file = file.path(temp_files_folder,'doys_max_map_clipped.tiff' )
#		out_nrice_file = file.path(temp_files_folder,'rice_map_clipped.tiff' )
		out_ref_hansen = file.path(main_folder,'Ancillary/Masks/Hansen_Forest_Map','Hansen_HR.tiff')
		
		grid_fullnames = paste(grid_folder,'/',grid_shapes,'.shp',sep = '')
		out_grid_df_files = file.path(outputs_folder,paste('out_df_',grid_shapes,'.RData', sep = ''))
		out_grid_shp_files = file.path(outputs_folder,paste('out_shp_',grid_shapes,'.shp', sep = ''))
		
		in_rast = raster(in_file)   # Get the proj4 of the input rasters
		in_proj = proj4string(in_rast)
		
	}}

#- ------------------------------------------------------------------------------- -#
#  Start the processing
#- ------------------------------------------------------------------------------- -#

#- ------------------------------------------------------------------------------- -#
# Create the regular grids (MODIS + 2,5,10,20 K) if needed  
#- ------------------------------------------------------------------------------- -#
{{
		
		for (gr_res in seq(along = grid_fullnames)) {
			
			
			if(file.exists(grid_fullnames[gr_res]) == FALSE) {
				LB_Create_pol_grid(in_file , grid_resolutions[gr_res],grid_folder, grid_shapes[gr_res],grid_extent)		
			}
			
		} #End Cycle on gr_res
	}}
#- ------------------------------------------------------------------------------- -#
# Reproject the reference raster map to MODIS projection
#- ------------------------------------------------------------------------------- -#
{{
		in_ref = raster(phrice_refmap_orig)
		in_ref_proj = proj4string(in_ref)
		if(file.exists(out_ref_raster) == FALSE) {
			ref_reproj = gdalwarp(phrice_refmap_orig,out_ref_raster, s_srs = in_ref_proj, t_srs = in_proj,
					r = "near",of = 'GTiff', overwrite = T, srcnodata = 128, dstnodata = 255,ot = 'Byte', output_Raster = TRUE)
			ext_ref_repr = extent(ref_reproj)
			res_ref_reproj = res(ref_reproj)
		} else {
			ref_reproj = raster(out_ref_raster)
			ext_ref_repr = extent(ref_reproj)
			res_ref_reproj = res(ref_reproj)
		}
	}}

# Start cyclying on grids --> On cycle one use the grid @ modis resolution to summarize results
# and compute results of "standard" accuracy analysis. On later  cycle ,compute results aggregated
#on hte various resolutions grids

for (cycle in seq(along = grid_shapes)) {
	
	
	if (cycle == 1) {   
#- ------------------------------------------------------------------------------- -#
#  # 1st cycle --> 
#-       1) Perform "tabulate areas" to retrieve the percentage area classified as rice 
#-				  in the refernce map for ech MODIS pixel
#-       2) Perform "tabulate areas" to retrieve the average forest fractional cover according to 
#-          Hansen map for each MODIS pixel
#-       3) Associate info on Administrative Areas for each MODIS pixel
#-			 4) Save results in out_mod_grid_df.RData
#-       5) Add computed info to the DBF of the mod_grid_250 shapefile 
#-	
#- ------------------------------------------------------------------------------- -#
		{{
				#- ------------------------------------------------------------------------------- -#
				# If not already existing, Rasterize polygons on the extent of the reference map, at a resolution 
				# equal to that of the reference map. For each pixel, values of this raster correspond to a
				# unique identifier of the cell of the  ERMES grid to which the pixel belongs
				#- ------------------------------------------------------------------------------- -#
				
				{{	
#						if (file.exists(out_grid_rasters[cycle]) == FALSE){
							dir.create(dirname(out_grid_rasters[cycle]))
							raster_grid = gdal_rasterize(grid_fullnames[cycle],out_grid_rasters[cycle],a = "id", output_Raster = T,
									te = c(ext_ref_repr@xmin, (ext_ref_repr@ymin+res_ref_reproj[1]),(ext_ref_repr@xmax-res_ref_reproj[1]), (ext_ref_repr@ymax)), ot = 'UInt32',tr = res_ref_reproj, tap = T, of = 'GTiff')
							gc()
#						}
					}}
				#- ------------------------------------------------------------------------------- -#
				# Compute the average forest cover for each pixel   
				#- ------------------------------------------------------------------------------- -#	
				{{ if (mask_hansen == T) {
						# Get the values of the "0000dummy" raster of cells codes
						raster_cells <- new("GDALReadOnlyDataset", out_grid_rasters[cycle])
						data_raster_cells = getRasterData(raster_cells)
						dim(data_raster_cells) = dim(data_raster_cells)[1]*dim(data_raster_cells)[2]
						GDAL.close(raster_cells)
						gc()
						
						# Resize Hansen Mask and increase resolution, then get the values
						
						if (file.exists (out_ref_hansen) == FALSE) {
							gdalwarp(hans_mask_file,out_ref_hansen, s_srs = in_proj, t_srs = in_proj,
									te = c(ext_ref_repr@xmin, (ext_ref_repr@ymin+res_ref_reproj[1]),(ext_ref_repr@xmax-res_ref_reproj[1]), (ext_ref_repr@ymax)),
									r = "near",of = 'GTiff', overwrite = T, srcnodata = 128, dstnodata = 255,ot = 'Byte', output_Raster = TRUE,tr = res_ref_reproj, tap = T)
						} 
						# Get the values of the Hansen Mask 
						hans_rast = new("GDALReadOnlyDataset",out_ref_hansen)
						hans_data =  getRasterData(hans_rast)
						dim(hans_data) = dim(hans_data)[1]*dim(hans_data)[2]
						GDAL.close(hans_rast)
						gc()
						
						# Create a Data.table with two columns. First column taken from the cell_codes raster - tells to which cell each pixel correspond
						# Second column taken from the forest cover map - tells which forest cover values correspond to each cell
						
						cells_dt = data.table(id=data_raster_cells,value = hans_data )
						setkey(cells_dt, id)  # Set a indexing key to increase speed of zonal statistics computation
						rm(data_raster_cells)
						rm(hans_data)
						gc()
						
# Compute the aggregated values for each cell of the ERMES grid - average, stdev, min, max and n° of non no-data pixels in each cell
						aggregate_hans_values = cells_dt[, list(average = as.numeric(mean(value, na.rm = T)),
										stdev = as.numeric(sd(value, na.rm = T)),
										n_cells = as.numeric(length(value))
								), by = key(cells_dt)]
						
						aggregate_hans_values = aggregate_hans_values[id != 0 ]		# Remove the values for areas outdide the MODIS grid
					}}
				}
#- ------------------------------------------------------------------------------- -#
# Compute the percentage area classified as rice for each pixel 
#- ------------------------------------------------------------------------------- -#
				{{
						
						# Get the values of the "dummy" raster of cells codes
						raster_cells <- new("GDALReadOnlyDataset", out_grid_rasters[cycle])
						data_raster_cells = getRasterData(raster_cells)
						dim(data_raster_cells) = dim(data_raster_cells)[1]*dim(data_raster_cells)[2]
						GDAL.close(raster_cells)
						gc()
						
						# Get the values of the "reference map
						ref_map <- new("GDALReadOnlyDataset",out_ref_raster)
						ref_map_data = getRasterData(ref_map)
						dim(ref_map_data) = dim(ref_map_data)[1]*dim(ref_map_data)[2]
						GDAL.close(ref_map)
						gc()
						
						# Create a Data.table with two columns. First column taken from the cell_codes raster - tells to which cell each pixel correspond
						# Second column taken from the reference rice map (0 or 1)
						
						cells_dt = data.table(id=data_raster_cells,value = ref_map_data )
						setkey(cells_dt, id)  # Set a indexing key to increase speed of zonal statistics computation
						rm(ref_map_data)
						rm(data_raster_cells)
						gc()
						
						# Compute the aggregated Reference Map values for each cell of the ERMES grid - average, stdev, min, max and n° of pixels in each cell
						aggregate_reference_values = cells_dt[, list(ref_rice_fc = mean(value, na.rm = T),
										
										n_cells = as.numeric(length(which(is.finite(value) == T)))
								), by = key(cells_dt)]
						
						aggregate_reference_values = aggregate_reference_values[id != 0 ]
						rm(cells_dt)
					}}
				
				#- ------------------------------------------------------------------------------- -#
				# Retrieve necessary data from the phenorice output rasters 
				#- ------------------------------------------------------------------------------- -#
				{{
#						ndetect = raster(fullout_file, band = 1)
#						mindoy = raster(fullout_file, band = 4)
#						maxdoy = raster(fullout_file, band = 8)
#						
#						tempndetect_file = file.path(temp_files_folder, 'ndetect.tif')
#						tempmindoy_file = file.path(temp_files_folder,  'mindoy.tif')
#						tempmaxdoy_file = file.path(temp_files_folder,  'maxdoy.tif')
#						
						gdalwarp(in_file,out_rice_file, te = c(grid_extent$xmin,grid_extent$ymin,grid_extent$xmax,grid_extent$ymax), overwrite = T)
						
						#- Retrieve the values of the number of rice season
#						gdalwarp(nrice,out_nrice_file, te = c(grid_extent$xmin,grid_extent$ymin,grid_extent$xmax,grid_extent$ymax), overwrite = T)
						rice_rast = new("GDALReadOnlyDataset", out_rice_file)
						data_rice = getRasterData(rice_rast)
						
						data_nrice = data_rice[,,1]  ; dim(data_nrice) = dim(data_nrice)[1]*dim(data_nrice)[2]
						mins_q1 = data_rice[,,2] ; dim(mins_q1) = dim(mins_q1)[1]*dim(mins_q1)[2]
						mins_q2 = data_rice[,,3] ; dim(mins_q2) = dim(mins_q2)[1]*dim(mins_q2)[2]
						mins_q3 = data_rice[,,4] ; dim(mins_q3) = dim(mins_q3)[1]*dim(mins_q3)[2]
						mins_q4 = data_rice[,,5] ; dim(mins_q4) = dim(mins_q4)[1]*dim(mins_q4)[2]
						maxs_q1 = data_rice[,,6] ; dim(maxs_q1) = dim(maxs_q1)[1]*dim(maxs_q1)[2]
						maxs_q2 = data_rice[,,7] ; dim(maxs_q2) = dim(maxs_q2)[1]*dim(maxs_q2)[2]
						maxs_q3 = data_rice[,,8] ; dim(maxs_q3) = dim(maxs_q3)[1]*dim(maxs_q3)[2]
						maxs_q4 = data_rice[,,9] ; dim(maxs_q4) = dim(maxs_q4)[1]*dim(maxs_q4)[2]
						GDAL.close(rice_rast)
						
#						#- Retrieve the values of the minimums and maximums from the phenorice outputs
#						gdalwarp(tempmindoy_file,out_mins_file, te = c(grid_extent$xmin,grid_extent$ymin,grid_extent$xmax,grid_extent$ymax), overwrite = T)
#						min_rast = new("GDALReadOnlyDataset", out_mins_file)
#						data_mins = getRasterData(min_rast)
#						mins_q1 = data_mins[,,1] ; dim(mins_q1) = dim(mins_q1)[1]*dim(mins_q1)[2]
#						mins_q2 = data_mins[,,2] ; dim(mins_q2) = dim(mins_q2)[1]*dim(mins_q2)[2]
#						mins_q3 = data_mins[,,3] ; dim(mins_q3) = dim(mins_q3)[1]*dim(mins_q3)[2]
#						mins_q4 = data_mins[,,4] ; dim(mins_q4) = dim(mins_q4)[1]*dim(mins_q4)[2]
#						GDAL.close(min_rast)
#						
#						# Retrieve the values of the minimums and maximums from the phenorice outputs
#						gdalwarp(tempmaxdoy_file,out_maxs_file, te = c(grid_extent$xmin,grid_extent$ymin,grid_extent$xmax,grid_extent$ymax), overwrite = T)
#						max_rast = new("GDALReadOnlyDataset", out_maxs_file)
#						data_maxs = getRasterData(max_rast)
#						maxs_q1 = data_maxs[,,1] ; dim(maxs_q1) = dim(maxs_q1)[1]*dim(maxs_q1)[2]
#						maxs_q2 = data_maxs[,,2] ; dim(maxs_q2) = dim(maxs_q2)[1]*dim(maxs_q2)[2]
#						maxs_q3 = data_maxs[,,3] ; dim(maxs_q3) = dim(maxs_q3)[1]*dim(maxs_q3)[2]
#						maxs_q4 = data_maxs[,,4] ; dim(maxs_q4) = dim(maxs_q4)[1]*dim(maxs_q4)[2]
#						GDAL.close(max_rast)
						
						# Get the cell_ids from the dbf of the grd shapefile and create a dataframe
						phrice_res_df = read.dbf(file.path(grid_folder,paste(grid_shapes[cycle],'.dbf', sep = '')))  
						phrice_res_df = data.frame(id = phrice_res_df$id)
						phrice_res_df$n_seasons = as.numeric(data_nrice)
						phrice_res_df$min_q1 = as.numeric(mins_q1)
						phrice_res_df$min_q2 = as.numeric(mins_q2)
						phrice_res_df$min_q3 = as.numeric(mins_q3)
						phrice_res_df$min_q4 = as.numeric(mins_q4)
						phrice_res_df$max_q1 = as.numeric(maxs_q1)
						phrice_res_df$max_q2 = as.numeric(maxs_q2)
						phrice_res_df$max_q3 = as.numeric(maxs_q3)
						phrice_res_df$max_q4 = as.numeric(maxs_q4)
						phrice_res_df [phrice_res_df == 0 ] = NA
						phrice_res_df$min_q1 [which(phrice_res_df$min_q1 >= 400 )] = NA
						phrice_res_df$min_q2 [which(phrice_res_df$min_q2  >= 400 )] = NA
						phrice_res_df$min_q3 [which(phrice_res_df$min_q3  >= 400 )] = NA
						phrice_res_df$min_q4 [which(phrice_res_df$min_q4  >= 400 )] = NA
						phrice_res_df$max_q1 [which(phrice_res_df$max_q1 >= 400 )] = NA
						phrice_res_df$max_q2 [which(phrice_res_df$max_q2  >= 400 )] = NA
						phrice_res_df$max_q3 [which(phrice_res_df$max_q3  >= 400 )] = NA
						phrice_res_df$max_q4 [which(phrice_res_df$max_q4  >= 400 )] = NA
						phrice_res_df$length_q1 = 	(phrice_res_df$max_q1+365)-(phrice_res_df$min_q1+365)
						phrice_res_df$length_q2 = 	(phrice_res_df$max_q2+365)-(phrice_res_df$min_q2+365)
						phrice_res_df$length_q3 = 	(phrice_res_df$max_q3+365)-(phrice_res_df$min_q3+365)
						phrice_res_df$length_q4 = 	(phrice_res_df$max_q4+365)-(phrice_res_df$min_q4+365)
						
						rm(mins_q1,mins_q2,mins_q3,mins_q4)
						rm(maxs_q1,maxs_q2,maxs_q3,maxs_q4)
						rm(data_maxs,data_mins)
						gc()
					}}		
				
#- ------------------------------------------------------------------------------- -#
# Get all the retrieved datasets and put them in a single data frame and save outputs
#- ------------------------------------------------------------------------------- -#
				
				{{
						# Get data for forest cover, number of cells, rice fractional cover
						if (mask_hansen == T) {				
		
						out_df_modgrid = data.frame(id = aggregate_reference_values$id, ncells = aggregate_reference_values$n_cells,
								ref_rice_fc = aggregate_reference_values$ref_rice_fc, hans_forcov = aggregate_hans_values$average)
						} else {
							out_df_modgrid = data.frame(id = aggregate_reference_values$id, ncells = aggregate_reference_values$n_cells,
									ref_rice_fc = aggregate_reference_values$ref_rice_fc)
						}
							
						
						# Join with the data retrieved from the rasters
#						browser()
						out_df_modgrid = join(out_df_modgrid,phrice_res_df, by = 'id', type = 'left' )
						is.na(out_df_modgrid) <- do.call(cbind,lapply(out_df_modgrid, is.infinite))  # Convert Infinite to NA
						is.na(out_df_modgrid) <- do.call(cbind,lapply(out_df_modgrid, is.nan))
						# Compute the ricearea form the percentage of rice area+ncells+resolution of reference map + compute
						# total non-nodata area of the pixel 
						out_df_modgrid$ref_ricearea = out_df_modgrid$ref_rice_fc*out_df_modgrid$ncells*(res(ref_reproj)[1]^2)
						out_df_modgrid$tot_cellarea = out_df_modgrid$ncells*(res(ref_reproj)[1]^2)
						
						#Retrieve and add info regarding administrative units
						in_shape_administrative = readOGR(dirname(in_shape_admin_file),file_path_sans_ext(basename(in_shape_admin_file)))  #TBC
						in_shape_admin_reproj = spTransform(in_shape_administrative, CRS(in_proj))
						centroids = getSpPPolygonsLabptSlots(readOGR(grid_folder,grid_shapes[cycle]))	# get coordinates of centroids of each 250m cell
						centroids = SpatialPoints(centroids,proj4string =  CRS(in_proj)) # convert to spatialpoints
						
						# do an "intersect" with the admin_areas shapes --> returns a df with the id of the cell, plus the "administrative units" info
						# extracted from the polygin in which the centroid of the cell falls
						admin_cells = over(centroids,in_shape_admin_reproj)
						
						###### NEED TO BE CHANGED IF THE STRUCTURE OF THE ADMIN DATA IS DIFFERENT !!!! ####
						#admin_cells = admin_cells [,c("NAME_1","NAME_2")]
		
						admin_cells = admin_cells [,c("COD_REG","COD_PRO")] 
						admin_cells$id = phrice_res_df$id
						names(admin_cells)[1:2] = c('Region','Province')
						out_df_modgrid = join(out_df_modgrid,admin_cells, by = 'id', type = 'left' )  # join to total df the info on Region and Province
						
						# SAve the outputs
						save(out_df_modgrid, file = out_mod_grid_df_file)		# Save the RData table
						out_shp_dbf = data.frame(id = phrice_res_df$id)			# Retrieve cell ids from the "empty grid
						out_shp_dbf = join(out_shp_dbf,out_df_modgrid, by = 'id', type = 'left' )  # join the info using id as base
						write.dbf(out_shp_dbf, file.path(grid_folder,paste(grid_shapes[cycle],'.dbf', sep = ''))) # overwrite the dbf table of the MOD 250m grid shapefile
						gc()
					}}	
				
			}}
		
	} else { # end of 1st cycle
		
		#- ------------------------------------------------------------------------------- -#
		#  Cycle on the other grids --> For each resolution grid, create a data frame in which
		# the information relative to the id of the "coarse" resolution cell to which each MODIS
		# pixel belongs is added to the data frame of "outputs" created befor (out_df_modgrid)
		#- ------------------------------------------------------------------------------- -#
		
		{{
				big_grid = readOGR(grid_folder,grid_shapes[cycle])	# read the shapefile of the grid
				# Do an intersect between the centroids of the 250 grid and the big grid--> Identidy the id of the big
				# grid for each cell of the 250 m one
				cells_on_big_grid = over(centroids,big_grid)	
				names(cells_on_big_grid)[1] = 'id_big'
				cells_on_big_grid$id = phrice_res_df$id   # add the id of the 250 m grid 
				cells_on_big_grid = cells_on_big_grid[which(is.finite(cells_on_big_grid$id_big) == TRUE),]  # remove useless cells
				cells_on_big_grid = data.table(cells_on_big_grid)	# create a data table to be used for tabulate areas
				setkey(cells_on_big_grid, 'id_big')	# set the key--> id of the coarse resolution
				comp_dt = join(cells_on_big_grid,out_df_modgrid, by = 'id', type = 'left')	# join the data taken from the mod_grid_data frame
				setkey(comp_dt, 'id_big')
				save(comp_dt, file = out_grid_df_files[cycle])
				
#	Compute aggregated values on the big grid cells	
		
				}}
	} #End else on cycle = 1
	
} #End for on Cycle

#								n_cells = as.numeric(length(which(is.finite(value) == T)))
		##						), by = key(cells_dt)]
#		
#		cells_on_big_grid2 = cells_on_big_grid[which(is.finite(cells_on_big_grid2$id) == TRUE),]
#		cells_on_big_grid2$id = as.factor(cells_on_big_grid2$id )
#		
#		
#		centroids = getSpPPolygonsLabptSlots(readOGR(grid_folder,grid_shapes[1]))
#		centroids = SpatialPoints(centroids,proj4string =  CRS(in_proj))
#		
#		big_cells = over(mod_grd,big_grd)
#		
		




#
#out_grid_raster = 'D:/Temp/Grid_Raster.tiff'
#
#in_grid = readOGR(grid_folder,grid_shape)
#in_grid_proj = proj4string(in_grid)
#extent_grid = extent(in_grid) 
#
##out_grid_raster = 'W:/francesco/PhenoRice_processing/PHL/Analysis/SARMap/Reproj/SAR_Rice_SIN'
#
##in_data_db = 'W:/francesco/PhenoRice_processing/PHL/Analysis/DataBase/Out5/dB_PHL_2013_Out5_v2.RData'
##in_data_full = get(load(in_data_db))
#
#in_data_db = 'W:/francesco/PhenoRice_processing/PHL/Analysis/DataBase/Out4/dB_PHL_2013.RData'
#in_data_full = get(load(in_data_db))
#
#
#
#
#
#
#in_data_administrative = readOGR('W:/lorenzo/phenorice/philippines/GIS','Modis_grid_adm_info_point')
#in_shape_administrative = readOGR('W:/lorenzo/phenorice/philippines/GIS','Andmin_SA')
#in_shape_administrative@data = in_shape_administrative@data [,c("NAME_1","NAME_2")] 
#names(in_shape_administrative@data) = c('Region','Province')
#
## To be used on other script
#in_data_formap = read.table("W:/lorenzo/phenorice/philippines/GIS/Forest_Area_Hansen.txt", header = T)
#names(in_data_formap)[3]='for_cover'
#in_data_formap$area = NULL
#
#names(in_data_administrative@data)[2] = 'ID'
#in_data_full = join(in_data_full, in_data_administrative@data, by = 'ID', type = 'left')
#in_data_full = join(in_data_full, in_data_formap, by = 'ID', type = 'left')
##save(in_data_full, file = file.choose(new = T))
#
#
#in_data_masked = droplevels(subset(in_data_full, for_cover < 50))
##in_data_masked = droplevels(subset(in_data_full, mask_eviforest == 1))
##in_data_masked = droplevels(subset(in_data_full, (mask_eviforest == 1) & for_cover < 15))
#
#{{in_data_masked$min_q1 [which(in_data_masked$min_q1 == 0 )] = NA
#		in_data_masked$min_q2 [which(in_data_masked$min_q2 == 0 )] = NA
#		in_data_masked$min_q3 [which(in_data_masked$min_q3 == 0 )] = NA
#		in_data_masked$min_q4 [which(in_data_masked$min_q4 == 0 )] = NA
#		
#		in_data_masked$max_q1 [which(in_data_masked$max_q1 >= 400 )] = NA
#		in_data_masked$max_q2 [which(in_data_masked$max_q2  >= 400 )] = NA
#		in_data_masked$max_q3 [which(in_data_masked$max_q3  >= 400 )] = NA
#		in_data_masked$max_q4 [which(in_data_masked$max_q4  >= 400 )] = NA
#		
#		in_data_masked$max_q1 [which(in_data_masked$max_q1 == 0 )] = NA
#		in_data_masked$max_q2 [which(in_data_masked$max_q2 == 0 )] = NA
#		in_data_masked$max_q3 [which(in_data_masked$max_q3 == 0 )] = NA
#		in_data_masked$max_q4 [which(in_data_masked$max_q4 == 0 )] = NA
#	}}
#
#{{# Compute Accuracy on areas - all quarters
#		acc_areas_comp = function (data_in){
#			
#			acc_areas = list()
#			MOD_Area = sum(data_in$MODIS_rice_area_m2)
#			SAR_Area = sum(data_in$SAR_rice_area_m2)
#			
## Compute Accuracy on areas - 1st and second
#			MOD_pixels = which(data_in$MODIS_rice_area_m2 > 0)
#			MOD_pixels_norice = which(data_in$MODIS_rice_area_m2 == 0)
#			ones_ones = sum(data_in$SAR_rice_area_m2 [MOD_pixels])
#			zeroes_zeroes = sum(data_in$SAR_norice_area_m2 [MOD_pixels_norice])
#			
#			sar_1_mod_0 = sum(data_in$SAR_rice_area_m2 [MOD_pixels_norice])
#			sar_0_mod_1 = sum(data_in$MODIS_rice_area_m2)-sum(data_in$SAR_rice_area_m2[MOD_pixels])
#			
#			acc = NULL
#			acc$OA = (ones_ones+zeroes_zeroes)/(ones_ones+zeroes_zeroes+sar_1_mod_0+sar_0_mod_1)*100
#			acc$Omission_Err = 100*(sar_1_mod_0/(sar_1_mod_0+ ones_ones))
#			acc$Commission_Err = 100*(sar_0_mod_1/(ones_ones+sar_0_mod_1))
#			acc$MOD_Area = MOD_Area
#			acc$SAR_Area = SAR_Area
#			acc$quarter = 'All'
#			acc_areas [[1]] = acc
#			
## Compute Accuracy on areas - 1std and 2ndth
#			
#			MOD_pixels = which(is.finite(data_in$min_q1) | is.finite(data_in$min_q2))
#			MOD_Area = sum(data_in$MODIS_rice_area_m2[MOD_pixels])
#			MOD_pixels_norice = which(!(is.finite(data_in$min_q1) | is.finite(data_in$min_q2)))
#			ones_ones = sum(data_in$SAR_rice_area_m2 [MOD_pixels])
#			zeroes_zeroes = sum(data_in$SAR_norice_area_m2 [MOD_pixels_norice])
#			
#			sar_1_mod_0 = sum(data_in$SAR_rice_area_m2 [MOD_pixels_norice])
#			sar_0_mod_1 = sum(data_in$MODIS_rice_area_m2)-sum(data_in$SAR_rice_area_m2[MOD_pixels])
#			
#			acc = NULL
#			acc$OA = (ones_ones+zeroes_zeroes)/(ones_ones+zeroes_zeroes+sar_1_mod_0+sar_0_mod_1)*100
#			acc$Omission_Err = 100*(sar_1_mod_0/(sar_1_mod_0+ ones_ones))
#			acc$Commission_Err = 100*(sar_0_mod_1/(ones_ones+sar_0_mod_1))
#			acc$MOD_Area = MOD_Area
#			acc$SAR_Area = SAR_Area
#			acc$quarter = '1st_Season'
#			acc_areas [[3]] = acc
#			
## Compute Accuracy on areas - 3rd and 4th
#			MOD_pixels = which(is.finite(data_in$min_q3) | is.finite(data_in$min_q4))
#			MOD_Area = sum(data_in$MODIS_rice_area_m2[MOD_pixels])
#			MOD_pixels_norice = which(!(is.finite(data_in$min_q3) | is.finite(data_in$min_q4)))
#			ones_ones = sum(data_in$SAR_rice_area_m2 [MOD_pixels])
#			zeroes_zeroes = sum(data_in$SAR_norice_area_m2 [MOD_pixels_norice])
#			
#			sar_1_mod_0 = sum(data_in$SAR_rice_area_m2 [MOD_pixels_norice])
#			sar_0_mod_1 = sum(data_in$MODIS_rice_area_m2)-sum(data_in$SAR_rice_area_m2[MOD_pixels])
#			
#			acc = NULL
#			acc$OA = (ones_ones+zeroes_zeroes)/(ones_ones+zeroes_zeroes+sar_1_mod_0+sar_0_mod_1)*100
#			acc$Omission_Err = 100*(sar_1_mod_0/(sar_1_mod_0+ ones_ones))
#			acc$Commission_Err = 100*(sar_0_mod_1/(ones_ones+sar_0_mod_1))
#			acc$MOD_Area = MOD_Area
#			acc$SAR_Area = SAR_Area
#			acc$quarter = '2nd_Season'
#			acc_areas [[2]] = acc
#			acc_areas =  rbindlist(acc_areas)
#			
#		}
#		
#		acc_areas_tot = acc_areas_comp(in_data_masked)
#		acc_areas_tot$NAME_2  = 'All'
#		acc_areas_Admin = ddply(in_data_masked, .(NAME_2), function(df)acc_areas_comp(df))
#		acc_areas_tot = rbind(acc_areas_tot,acc_areas_Admin)
#		names(acc_areas_tot)[7] = 'Province'
#	}}
#
## Add results to shapefile @data
#
#data_2_join = droplevels(subset(acc_areas_tot, quarter == 'All'))
#in_shape_administrative@data = join(in_shape_administrative@data,data_2_join, by = 'Province', type = 'left' )
#
#{{# Plot the results
#		data_melted = melt(acc_areas_tot)
#		names(data_melted)[1] = 'Season'
#		data_melted$Season = as.factor(data_melted$Season)
#		levels(data_melted$Season) = c('1st','2nd','All')
#		area_data <- droplevels(subset(data_melted,(variable %in% names(acc_areas_tot)[4:7])))
#		names(area_data)[4] = 'Area'
#		area_data$Area = area_data$Area/10000
#		
## Plot Area Comparisons on all map
#		p = ggplot (droplevels(subset(area_data,Province =='All')),aes(x = Season, y = Area, fill = variable)) + theme_bw()
#		p = p + geom_bar(stat = 'identity', position = position_dodge()) + facet_wrap(~Province)
#		print(p)
#		
## Plot Area Comparisons  by Provinces
#		p = ggplot (droplevels(subset(area_data,Province !='All')),aes(x = Season, y = Area, fill = variable)) + theme_bw()
#		p = p + geom_bar(stat = 'identity', position = position_dodge()) + facet_wrap(~Province)
#		print(p)
#		
## Plot Accracies on all mop
#		acc_data <- droplevels(subset(data_melted,(variable %in% names(acc_areas_tot)[1:3])))
#		names(acc_data)[4] = 'Metric'
#		
#		p = ggplot (droplevels(subset(acc_data,Province =='All')),aes(x = Season, y = Metric, fill = variable)) + theme_bw()
#		p = p + geom_bar(stat = 'identity', position = position_dodge(width = 1.0)) + facet_wrap(~Province)
#		p = p + scale_fill_discrete('Metric', labels = c('Overall Accuracy', 'Omission Err. Rice', 'Commission Err. Rice'))
#		print(p)
#		
##p = ggplot (droplevels(subset(acc_data,(Province !='All' & variable  != 'Overall Accuracy'))),aes(x = Season, y = Metric, fill = variable)) + theme_bw()
##p = p + geom_bar(stat = 'identity', position = position_dodge(width = 1.0)) + facet_wrap(~Province)
##p = p + scale_fill_discrete('Metric', labels = c('Overall Accuracy', 'Omission Err. Rice', 'Commission Err. Rice'))
##p
#		
#		acc_data2 = droplevels(subset(acc_data,(Province !='All' & variable  != 'OA')))
#		p = ggplot (acc_data2,aes(x = Season, y = Metric, fill = variable)) + theme_bw()
#		p = p + geom_bar(stat = 'identity', position = position_dodge(width = 1.0)) + facet_wrap(~Province)
#		p = p + scale_fill_discrete('Metric', labels = c('Omission Err. Rice', 'Commission Err. Rice'))
#		print(p)
#	}}
#
#{{# Compute accuracies as number of pixels
#		acc_pixels = function(data_in){
#			threshs = seq(0.05,0.95,0.05)
#			acc_out = list()
#			
#			for(th in seq_along(threshs)){
#				
#				sar_binary = 1*(data_in$SAR_rice_area_percentage > threshs[th])
#				mod_binary = 1*(data_in$MODIS_rice_area > 0)
#				
#				conf_table = confusion.matrix(sar_binary,mod_binary)
#				accuracy = accuracy(sar_binary,mod_binary)
#				accuracy$Omission_Err_Rice = 100-100*accuracy$sensitivity
#				accuracy$Commission_Err_Rice = 100*conf_table[2,1]/(conf_table[2,1]+conf_table[2,2])
#				accuracy$threshold = threshs[th]*100
#				acc_out[[th]] = accuracy
#			}
#			acc_out = rbindlist(acc_out)
#			acc_out
#		}
#		
#		acc_pix_all = acc_pixels(in_data_masked)
#		acc_pix_all$NAME_2 = 'All'
#		acc_pix_admin =  ddply(in_data_masked, .(NAME_2), function(df)acc_pixels(df))
#		acc_pix_all = rbind(acc_pix_all,acc_pix_admin)
#		names(acc_pix_all)[10] = 'Province'
#	}}
#
## Add results to admin shapefil
#
#data_accpix_join = droplevels(subset(acc_pix_all, (threshold == '75' & Province != 'All')))
#data_accpix_join = data_accpix_join[, names(data_accpix_join) %in% c('Province','Kappa','Omission_Err_Rice','Commission_Err_Rice'), with=FALSE]
#in_shape_administrative@data = join(in_shape_administrative@data,data_accpix_join, by = 'Province', type = 'left' )
#
## Plot results
#data_melted = melt(acc_pix_all, id.vars = c(1,10))
#plot_data <- droplevels(subset(data_melted,(variable %in% names(acc_pix_all)[c(1,8,9,10)])))
#
## Plot Accracies on all mop
#p = ggplot (droplevels(subset(plot_data,Province =='All')),aes(x = threshold, y = value, pch = variable, colour = variable)) + theme_bw()
#p = p + geom_point()+geom_line(linetype = 'dashed') + facet_wrap(~Province)
#p = p + scale_colour_discrete('Legend', labels = c( 'Omission Err. Rice', 'Commission Err. Rice'))+
#		scale_shape_discrete('Legend', labels = c( 'Omission Err. Rice', 'Commission Err. Rice'))+
#		scale_x_continuous('% of rice in MODIS pixel', limits = c(0,100), breaks = seq(0,100,10) )+
#		scale_y_continuous('Metric (%)', limits = c(0,100),breaks = seq(0,100,10))+
#		theme(legend.justification = c(0,1),legend.position=c(0,1))
#print(p)
#
## By provinces
#
#p = ggplot (droplevels(subset(plot_data,Province !='All')),aes(x = threshold, y = value, pch = variable, colour = variable)) + theme_bw()
#p = p + geom_point()+geom_line(linetype = 'dashed') + facet_wrap(~Province)
#p = p + scale_colour_discrete('Legend', labels = c( 'Omission Err. Rice', 'Commission Err. Rice'))+
#		scale_shape_discrete('Legend', labels = c( 'Omission Err. Rice', 'Commission Err. Rice'))+
#		scale_x_continuous('% of rice in MODIS pixel', limits = c(0,100), breaks = seq(0,100,10) )+
#		scale_y_continuous('Metric (%)', limits = c(0,100),breaks = seq(0,100,10))+
#		theme(legend.justification = c(1,0),legend.position=c(1,0))
#print(p)
#
## Boxplots of dates
#
#data = in_data_masked
#data$length_q1 = 	(data$max_q1+365)-(data$min_q1+365)
#data$length_q2 = 	(data$max_q2+365)-(data$min_q2+365)
#data$length_q3 = 	(data$max_q3+365)-(data$min_q3+365)
#data$length_q4 = 	(data$max_q4+365)-(data$min_q4+365)
## Compute average doys
#
#doys_provinces = ddply (data, .(NAME_2), summarize, avg_q1 = mean(min_q1, na.rm = T)
#		, avg_q2 = mean(min_q2, na.rm = T)
#		, avg_q3 = mean(min_q3, na.rm = T)
#		, avg_q4 = mean(min_q4, na.rm = T)
#		, avg_max_q1 = mean(max_q1, na.rm = T)
#		, avg_max_q2 = mean(max_q2, na.rm = T)
#		, avg_max_q3 = mean(max_q3, na.rm = T)
#		, avg_max_q4 = mean(max_q4, na.rm = T)
#		, avg_length_q1 = mean(max_q1, na.rm = T)
#		, avg_length_q2 = mean(max_q2, na.rm = T)
#		, avg_length_q3 = mean(max_q3, na.rm = T)
#		, avg_length_q4 = mean(max_q4, na.rm = T))
#
#
#
#names(doys_provinces)[1] = 'Province'
#
## Join results to shapefile
#
#is.na(doys_provinces) <- do.call(cbind,lapply(doys_provinces, is.nan))
#is.na(data) <- do.call(cbind,lapply(data, is.nan))
#
#
##a[,!is.finite(a[,])] = NA
#in_shape_administrative@data = join(in_shape_administrative@data ,doys_provinces, by = 'Province', type = 'left')
#
##writeOGR (in_shape_administrative,'W:/lorenzo/phenorice/philippines/GIS',outshape,driver="ESRI Shapefile",overwrite_layer=T) 
#
## Print boxplots of minimums
#pro = melt(data,value.name = "DOY", measure.vars = c('min_q1','min_q2','min_q3','min_q4'), na.rm = T )
#
##pro = melt(data,value.name = "DOY", measure.vars = c('min_q1','min_q3'), na.rm = T )
#
#names(pro)[36] = 'Quarter'
#names(pro)[37] = 'DOY'
##pro$DOY = as.Date(pro$DOY - 1, origin = "2013-01-01")
#p = ggplot(pro, aes(x = Quarter, y = DOY))+theme_bw()
#p = p + geom_boxplot(outlier.colour = 'transparent')+geom_jitter(position = position_jitter(w = 0.1), size = 0.1)
#p = p + facet_wrap(~NAME_2)
#print(p)
#
#p = ggplot(pro, aes(x = Quarter, y = DOY))+theme_bw()
#p = p + geom_boxplot(outlier.colour = 'transparent')+geom_jitter(position = position_jitter(w = 0.1), size = 0.1)
#print(p)
#
## Print boxplots of maximums
#pro = melt(data,value.name = "DOY", measure.vars = c('max_q1','max_q2','max_q3','max_q4'), na.rm = T )
#
##pro = melt(data,value.name = "DOY", measure.vars = c('max_q1','max_q3'), na.rm = T )
#
#names(pro)[36] = 'Quarter'
#names(pro)[37] = 'DOY'
##pro$DOY = as.Date(pro$DOY - 1, origin = "2013-01-01")
#
#p = ggplot(pro, aes(x = Quarter, y = DOY))+theme_bw()
#p = p + geom_boxplot(outlier.colour = 'transparent')+geom_jitter(position = position_jitter(w = 0.1), size = 0.1)
#print(p)
#
#p = ggplot(pro, aes(x = Quarter, y = DOY))+theme_bw()
#p = p + geom_boxplot(outlier.colour = 'transparent')+geom_jitter(position = position_jitter(w = 0.1), size = 0.1)
#p = p + facet_wrap(~NAME_2)
#print(p)
#
## Print boxplots of maximums
#pro = melt(data,value.name = "DOY", measure.vars = c('length_q1','length_q2','length_q3','length_q4'), na.rm = T )
#
#names(pro)[36] = 'Quarter'
#names(pro)[37] = 'DOY'
##pro$DOY = as.Date(pro$DOY - 1, origin = "2013-01-01")
#
#p = ggplot(pro, aes(x = Quarter, y = DOY))+theme_bw()
#p = p + geom_boxplot(outlier.colour = 'transparent')+geom_jitter(position = position_jitter(w = 0.1), size = 0.1)
#print(p)
#
#p = ggplot(pro, aes(x = Quarter, y = DOY))+theme_bw()
#p = p + geom_boxplot(outlier.colour = 'transparent')+geom_jitter(position = position_jitter(w = 0.1), size = 0.1)
#p = p + facet_wrap(~NAME_2)
#print(p)
#
#
#dev.off()
#
#
##
##pro = melt(data,value.name = "DOY", measure.vars = c('min_1st','min_2nd'), na.rm = T )
##
##names(pro)[43] = 'Season'
##names(pro)[44] = 'DOY'
##pro$DOY = as.Date(pro$DOY - 1, origin = "2013-01-01")
##p = ggplot(pro, aes(x = Season, y = DOY))+theme_bw()
##p = p + geom_boxplot(outlier.colour = 'transparent')+geom_jitter(position = position_jitter(w = 0.1), size = 0.3)
##p = p + facet_wrap(~NAME_2)
##print(p)
##
##p = ggplot(pro, aes(x = Season, y = DOY))+theme_bw()
##p = p + geom_boxplot(outlier.colour = 'transparent')+geom_jitter(position = position_jitter(w = 0.1), size = 0.3)
##print(p)
#
