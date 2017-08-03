# TODO: Add comment
#
# Author: LB
###############################################################################

library(raster)
library(sp)
library(gdalUtils)
library(rgdal)
library(data.table)
library(plyr)
library(hash)
library(tools)
library(stringr)
memory.limit(12000)

Main_Folder = "Y:/ermes/datasets/rs_products/Phenology/%cc%/Outputs/v1.0"
Main_Out_Folder = "Y:/ermes/datasets/rs_products/Phenology/%cc%/Outputs/v4.0"
Grid_Folder = "Y:/ermes/datasets/ERMES_Folder_Structure/%cc%/Regional/%cc%_Reference_Grid/%cc%_ERMES_Regional_Grid.shp"


vers = '004'
selvar = c(1,1,1,1)   # Array of variables to be extracted/created. (mindoys, sosdoys, maxdoys, vimax)

start_year = 2003
end_year 	 = 2015

year_subfolders = as.character(seq(start_year,end_year,1))
memory.limit(8000)


countries = c('IT','ES','GR')
 # countries = 'GR'

# Define paths
# main_folder = '//10.0.1.252/nr_working/lorenzo/phenorice/Lombardy/ERMES_Subset/'
#phenorice_res_subfolders = c('Outputs/v1.0_EVI')

# phenorice_out_folders = hash('IT' = '//10.0.1.252/nr_working/lorenzo/phenorice/lombardy/ERMES_Subset/Processing_v21/old_min_identification/Outputs/ndii_new',
# 		'ES' = '//10.0.1.252/nr_working/lorenzo/phenorice/Spain/ERMES_Subset/Processing_v21/old_min_identification/Outputs/ndii_new',
# 		'GR'= '//10.0.1.252/nr_working/lorenzo/phenorice/Greece/ERMES_Subset/Processing_v21/old_min_identification/Outputs/ndii_new')
#
# ermes_grids = hash('IT' = '//10.0.1.252/ftp/ERMES/ERMES_Archive/Italy/Vector/Reference_ERMES_Grid/shape_polygons/ERMES_Grid_Italy_shp_LAEA_poly.shp',
# 		'ES' = '//10.0.1.252/ftp/ERMES/ERMES_Archive/Spain/Vector/Reference_ERMES_Grid/shape_polygons/ERMES_Grid_Spain_shp_LAEA.shp',
# 		'GR'= '//10.0.1.252/ftp/ERMES/ERMES_Archive/Greece/Vector/Reference_ERMES_Grid/shape_polygons/ermes_Grid_Greece_shp_LAEA.shp')

# year_subfolders = as.character(seq(start_year,end_year,1))

#year_subfolders = as.character(2014)

for (country in countries) {
	print(country)

	# Define input and output folders based on country
	phenorice_out_folder = file.path(str_replace_all(Main_Folder,"%cc%",country))
	ermes_grid = str_replace_all(Grid_Folder,"%cc%",country)
	out_folder = file.path(str_replace_all(Main_Out_Folder,"%cc%",country),'ERMES_Grid')
	dir.create(out_folder, recursive = T)

	temp_files_folder = file.path('D:/temp/bigfiles',country)
	dir.create(temp_files_folder, recursive = T)

	# Define general use variables

	laea_crs = CRS("+init=epsg:3035 +proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0 +units=m +no_defs")
	mod_crs = CRS('+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs' )

	ERMES_cells_poly = readOGR(dirname(ermes_grid) , basename(file_path_sans_ext(ermes_grid)))
	ERMES_cells_poly_mod = spTransform(ERMES_cells_poly, mod_crs)
	writeOGR(ERMES_cells_poly_mod,dsn = file.path(temp_files_folder,'grid_sinu'), layer =paste(basename(file_path_sans_ext(ermes_grid)),country, sep = '_'),driver="ESRI Shapefile", overwrite_layer = T) #save the shapefile
	ERMES_cells_poly_sinu = file.path(temp_files_folder,'grid_sinu',paste(basename(file_path_sans_ext(ermes_grid)),'_',country,'.shp', sep = ''))

	# Rasterize polygons converted to sinusoidal on the extent of phenorice output and at 46.312 m resolution

#	ERMES_cells_poly_sinu = file.path(main_folder,'Grids','ERMES_Grid_Italy_shp_sinu.shp')


	# Rasterize polygons converted to sinusoidal on the extent of phenorice output and at 46.312 m resolution
		results_total =  list()									# Initialize full results table

	results_full =  list()
	for (year  in seq(along = year_subfolders)) {
	  yy = year_subfolders[year]
	  print(yy)
		print(paste('Aggregating Results for: ', country, ' - Year: ',yy,sep = ''))

# Find the phenorice output files of interest and open them to "raster" objects
		phrice_path  =  file.path(phenorice_out_folder,yy,'raster/')			# set path
		fullout_file = list.files(phrice_path, paste('*Full_Out_',yy,'.dat$', sep = ''), full.names = T)		# get name of output file
		extent_phrice =  extent(raster(fullout_file))		# Get extent
		print('regridding')
		if (!file.exists(file.path(temp_files_folder, paste('ERMES_Grid_Rasterized_',country,'_SINU.tif',sep = '')))){   # create rasterized polygon file if not existing, at 46 m resolution
			raster_cells_sinu = gdal_rasterize(ERMES_cells_poly_sinu,
					file.path(temp_files_folder, paste('ERMES_Grid_Rasterized_',country,'_SINU.tif',sep = '')),
					a = "int_id", output_Raster = T, te = c(extent_phrice@xmin, extent_phrice@ymin,extent_phrice@xmax, extent_phrice@ymax), tr = c(46.3312, 46.3312))
			cells_values = getValues(raster_cells_sinu)
			cells_values = as.integer(cells_values)
		} else {

			raster_cells_sinu = raster(file.path(temp_files_folder, paste('ERMES_Grid_Rasterized_',country,'_SINU.tif',sep = '')))  # if raster already exist, just get the values

			cells_values = getValues(raster_cells_sinu)
			cells_values = as.integer(cells_values)
		}


		dir.create(file.path(temp_files_folder, 'temp_phrice_outs'), recursive = T)

		phrice_out_list = list()

		# resize temporary phenorice output tiffs to 47 meters resolution rasters
		if (selvar [1] == 1){

			mindoy = raster(fullout_file, band = 2)
			extent_phrice =  extent(raster(mindoy))   # get extent of phenorice output

			tempmindoy_file = file.path(temp_files_folder, 'temp_phrice_outs', 'mindoy.tif')
			writeRaster(mindoy, filename = tempmindoy_file, overwrite = T)

			print('warping')
			gdalwarp(tempmindoy_file,file.path(temp_files_folder, 'min_doy_big.tif'), s_srs = mod_crs, t_srs = mod_crs,
					tr = c(46.3312, 46.3312), r = "near",  dstnodata =  -1,  overwrite = T)
			x <- new("GDALReadOnlyDataset", file.path(temp_files_folder, 'min_doy_big.tif'))

			phrice_out_list$MinDoys = getRasterTable(x)$band1
			zeroes = which(phrice_out_list$MinDoys == 0 | phrice_out_list$MinDoys > 365 )   # transfor zeroes and flags to NA
			phrice_out_list$MinDoys [zeroes] = NA
			GDAL.close(x)
			file.remove(file.path(temp_files_folder, 'min_doy_big.tif'))
			file.remove(tempmindoy_file)

		}

		if (selvar [2] == 1){

			maxdoy = raster(fullout_file, band = 3)
			tempmaxdoy_file = file.path(temp_files_folder, 'temp_phrice_outs', 'maxdoy.tif')
			writeRaster(maxdoy, filename = tempmaxdoy_file, overwrite = T)

			gdalwarp(tempmaxdoy_file,file.path(temp_files_folder, 'max_doy_big.tif'), s_srs = mod_crs, t_srs = mod_crs,
					tr = c(46.3312, 46.3312), r = "near",  dstnodata =  -1,  overwrite = T)
			x <- new("GDALReadOnlyDataset", file.path(temp_files_folder, 'max_doy_big.tif'))
			phrice_out_list$MaxDoys = getRasterTable(x)$band1
			zeroes = which(phrice_out_list$MaxDoys == 0 | phrice_out_list$MaxDoys > 365)
			phrice_out_list$MaxDoys [zeroes] = NA
			GDAL.close(x)
			file.remove(file.path(temp_files_folder, 'max_doy_big.tif'))
			file.remove(tempmaxdoy_file)
		}

		if (selvar [3] == 1){

			sosdoy = raster(fullout_file, band = 5)
			tempsosdoy_file = file.path(temp_files_folder, 'temp_phrice_outs', 'sosdoy.tif')
			writeRaster(sosdoy, filename = tempsosdoy_file, overwrite = T)

			gdalwarp(tempsosdoy_file,file.path(temp_files_folder, 'sos_doy_big.tif'), s_srs = mod_crs, t_srs = mod_crs,
					tr = c(46.3312, 46.3312), r = "near",  dstnodata =  -1,  overwrite = T)
			x <- new("GDALReadOnlyDataset", file.path(temp_files_folder, 'sos_doy_big.tif'))
			phrice_out_list$SoSDoys = getRasterTable(x)$band1
			zeroes = which(phrice_out_list$SoSDoys == 0 | phrice_out_list$SoSDoys > 365)
			phrice_out_list$SoSDoys [zeroes] = NA
			GDAL.close(x)
			file.remove(file.path(temp_files_folder, 'sos_doy_big.tif'))
			file.remove(tempsosdoy_file)
		}

		if (selvar [4] == 1){

			maxvi = raster(fullout_file, band = 13)
			tempmaxvi_file = file.path(temp_files_folder, 'temp_phrice_outs', 'maxvi.tif')
			writeRaster(maxvi, filename = tempmaxvi_file, overwrite = T)

			gdalwarp(tempmaxvi_file,file.path(temp_files_folder, 'max_vi_big.tif'), s_srs = mod_crs, t_srs = mod_crs,
					tr = c(46.3312, 46.3312), r = "near", dstnodata =  -1,  overwrite = T)
			x <- new("GDALReadOnlyDataset", file.path(temp_files_folder, 'max_vi_big.tif'))
			phrice_out_list$MaxVis = getRasterTable(x)$band1
			zeroes = which(phrice_out_list$MaxVis == 0)
			phrice_out_list$MaxVis [zeroes] = NA
			GDAL.close(x)
			file.remove(tempmaxvi_file)
			file.remove(file.path(temp_files_folder, 'max_vi_big.tif'))
		}
		# Perform "aggregation" to ERMES grid exploiting "data.table" methods --- doy of minimum
		phrice_aggregate = function(values,cells_values) {
#			cells_values = acells_values
			cells_dt = data.table(int_id=cells_values, value = as.numeric(values)) # create data table - cell ids + values of the variable
			setkey(cells_dt, int_id)			# set indexing key of data table
			# compute aggregate statistics on the different cells
			stats_by_cell = cells_dt[, list(n_rice_in_cell = length(which(is.finite(value) == T)),
							n_tot_in_cell = length(value),
							perc_rice =  length(which(is.finite(value) == T))/length(value),
							mean= mean(value, na.rm = T),
							min = min (value, na.rm = T),
							max = max (value, na.rm = T),
							stdev =sd (value, na.rm = T)),
					by=int_id]

			is.na(stats_by_cell) <- do.call(cbind,lapply(stats_by_cell, is.infinite))
			is.na(stats_by_cell) <- do.call(cbind,lapply(stats_by_cell, is.nan))


			return(stats_by_cell[2:length(unique(stats_by_cell$int_id))])
			# Put the results in the "data" structure of a spatialpointsdataframe with points for each ERMES cell and return result
#			return(SpatialPointsDataFrame(ERMES_cells@coords, cbind(ERMES_cells@data, year = yy, thresh = thresh, stats_by_cell[3:9137]),proj4string = laea_crs))
		}

# compute results - ouput is a list of spatial pixels data frames - one for each variable put in the list
# "phrice_out_list"

		print('zonalstats')
		results = ldply(phrice_out_list,phrice_aggregate, cells_values, .progress = 'text', .parallel = T)

		print('factorizing')
		names(results)[1] = 'variable'
		results$variable = as.factor(results$variable)
		results$year = yy
		print('joining')
		results = join(results, ERMES_cells_poly@data, by = 'int_id')

		rm(phrice_out_list)


# save the results for the different years as RData files
		out_file_folder = (file.path(out_folder,'RData'))
		dir.create (out_file_folder, recursive = T, showWarnings = F)
		out_file = file.path(out_file_folder, paste('ERMES_Aggregates_',yy,'.RData', sep = ''))
		save(results, file = out_file)
# make a cbind of all results for the selected threshold (i.e., all years)
		print('rbinding')
		results_full [[year]] = results


# save the results for the different years as 2Km tiff files
		print('2km')
for (var in unique(results$variable)) {

			data = subset(results, variable == var)
			data$mean [which(data$is_rice == 0)] = NA

			out_tif_folder = file.path(out_folder,'TIFFS',yy,var)
			dir.create(out_tif_folder,recursive = T, showWarnings = F)
			outfile_raster = file.path(out_tif_folder,paste(country, 'Phenology',var,yy,vers,sep = '_'))
			raster_out_mean = rasterFromXYZ(as.data.frame(data)[, c("x_LAEA", "y_LAEA", "mean")], res=c(2000,2000), crs = laea_crs, digits=5)
			writeRaster(raster_out_mean,filename = outfile_raster, format = 'GTiff', overwrite = T, NAflag = -1,options=c("COMPRESS=NONE"))

		}


	} #End Cycle on yy
# browser()
# save the results for all years
	out_file_full = file.path(out_file_folder, paste('ERMES_Aggregates_',min(year_subfolders),'_',max(year_subfolders),'.RData', sep = ''))
	results_out = rbindlist(results_full)
	save(results_out, file = out_file_full)

	# results_total = rbind(results_total, results_full)

	gc()


} #End Cycle on countries

# out_file_total_folder = (file.path(out_folder,'RData'))
# out_file_total = file.path(out_file_total_folder, paste('ERMES_Aggregates_',min(year_subfolders),'_',max(year_subfolders),'Total.RData', sep = ''))
# save(results_total, file = out_file_total)

