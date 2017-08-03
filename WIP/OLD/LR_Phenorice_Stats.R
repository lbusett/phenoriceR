# Function to aggregate values of phenorice output on GADM polygons.
#
# Author: Luigi Ranghetti (ranghetti.l@irea.cnr.it)
# License: GPL >2
###############################################################################


# -------------------- Set parameters ---------------------

# Choose your country (use ISO-3166 alpha-3 code! http://en.wikipedia.org/wiki/ISO_3166-1_alpha-3)
country_code = 'ITA'

# Choose the administrative level (from 0 - whole country - to 4 - maximum detail).
# See http://www.gadm.org/download to check availability of levels for your country
gadm_level = 2

# Indicate the path of the phenorice output raster (full) to use
phenorice_path = 'C:/the/directory/of/the/identification_Full_Out_2014.dat'

# Indicate the directory path where files (gadm shapes and rasters, and eventually xlsx and/or csv output) have to be saved
main_dir = 'C:/the/working/directory/Example_aggr'

# Indicate the name of the xlsx output file (without the extension), or leave NA not to generate it.
xlsx_filename = 'identification_Full_Out_2014_aggr_adm'

# Indicate the name prefix of the csv output files (without the extension), or leave NA not to generate it.
csv_filename = NA

# choose output statistics (set according to below-defined names - DO NOT MODIFY NAMES!)
aggr_stats        = c(  TRUE , TRUE , TRUE , TRUE , TRUE , TRUE ,  TRUE   , TRUE , TRUE  )
names(aggr_stats) = c( 'mean', 'sd' , 'min', 'max', 'q05', 'q25', 'median', 'q75', 'q95' )

# --- End of user definitions - Do not edit after here! ---



# Define function
Phenorice_aggr_adm <- function(country_code, gadm_level, phenorice_path, main_dir, aggr_stats, csv_filename=NA, xlsx_filename=NA ) {
	# mandatory parameters are explained above;
	# csv/xlsx_filename: if NA, no csv and/or xlsx are saved;
	# otherwise they are the name prefixes of the two csv and/or xlsx(directory is main_dir)
	
	require(sp)
	require(rgdal)
	require(gdalUtils)
	require(tools)
	require(data.table)
	require(raster)
	require(xlsx)
	
	# Load phenorice output
	cat('Loading phenorice output raster (it can take some time)...\n')
	phenorice_full <- readGDAL(phenorice_path)
	phenorice_cellsize <- phenorice_full@grid@cellsize # save it (to compute areas) because phenorice_full will be removed
	cat('[',date(),'] Done.\n\n')
	
	# Download, load and reproject GADM boundaries
	# (gadm shapefiles have been used instead of RData since gdal_rasterize needs them)
	cat('Download and reproject GADM boundaries...\n')
	gadm_shp_url <- paste0('http://biogeo.ucdavis.edu/data/gadm2/shp/',country_code,'_adm.zip')
	gadm_shape_dir <- file.path(main_dir,'gadm',basename(file_path_sans_ext(gadm_shp_url)))
	dir.create(gadm_shape_dir, recursive=TRUE, showWarnings=FALSE)
	if (!file.exists(paste0(gadm_shape_dir,'.zip'))) {download.file(gadm_shp_url,paste0(gadm_shape_dir,'.zip'))}
	if (!file.exists(gadm_shape_dir)) {unzip(paste0(gadm_shape_dir,'.zip'), exdir=gadm_shape_dir)}
	gadm_shape_name = paste0(country_code,'_adm',gadm_level,'_sinu.shp')
	if (!file.exists(file.path(gadm_shape_dir,gadm_shape_name))) {
		ogr2ogr(file.path(gadm_shape_dir,gsub('_sinu','',gadm_shape_name)), file.path(gadm_shape_dir,gadm_shape_name),
				f='ESRI Shapefile', s_srs='EPSG:4326', t_srs=phenorice_full@proj4string@projargs)
	}
	gadm <- readOGR(gadm_shape_dir,file_path_sans_ext(gadm_shape_name))
	cat('[',date(),'] Done.\n\n')
	
	# Assign band names from header file
	phenorice_header_path <-paste0(file_path_sans_ext(phenorice_path),'.hdr')
	if (file.exists(phenorice_header_path)) {
		phenorice_header <- readLines(phenorice_header_path)
		band_names_linestart <- grep('band names =',phenorice_header)
		band_names_lineend   <- grep('}$',phenorice_header)
		band_names_lineend <- band_names_lineend[band_names_lineend > band_names_linestart][1]
		phenorice_bandnames <- paste(phenorice_header[band_names_linestart:band_names_lineend],collapse='')
		phenorice_bandnames <- unlist(strsplit(gsub("(band names = )|[,\\{\\}]"," ",phenorice_bandnames)," "))
		phenorice_bandnames <- phenorice_bandnames[nchar(phenorice_bandnames)>0]
		names(phenorice_full@data) <- phenorice_bandnames
	}
	
	# Rasterize GADM polygons on the extent and resolution of phenorice raster, and load
	cat('Rasterize and load GADM polygons...\n')
	gadm_tif_name = gsub('_sinu\\.shp','.tif',gadm_shape_name)
	if (!file.exists(file.path(gadm_shape_dir,gadm_tif_name))) {
		gdal_translate(phenorice_path, file.path(gadm_shape_dir,gadm_tif_name),ot='Int16',b=1) # Temp file which will be overwritten by gdal_rasterize
		raster_grid = gdal_rasterize(file.path(gadm_shape_dir,gadm_shape_name), file.path(gadm_shape_dir,gadm_tif_name),
				a=paste0('ID_',gadm_level), l=file_path_sans_ext(gadm_shape_name))
	}
	gadm_raster <- readGDAL(file.path(gadm_shape_dir,gadm_tif_name))
	cat('[',date(),'] Done.\n\n')
	
	# Create source data.table
	phenorice_dt <- data.table(cbind(
					'Adm_Name'=gadm@data[[paste0('NAME_',gadm_level)]][match(gadm_raster$band1,gadm@data[[paste0('ID_',gadm_level)]])],
					'Adm_ID'=gadm_raster$band1,
					phenorice_full@data), key='Adm_ID')
	rm(phenorice_full,gadm_raster); gc()
	
	# Functions for metrics (only the ones selected by aggr_stats will be used)
	stats_function <- list()
	stats_function[['mean']] = function(x) {mean(x, na.rm=TRUE)}
	stats_function[['sd']] = function(x) {sd(x, na.rm=TRUE)}
	stats_function[['min']] = function(x) {as.numeric(min(x, na.rm=TRUE))}
	stats_function[['max']] = function(x) {as.numeric(max(x, na.rm=TRUE))}
	stats_function[['q05']] = function(x) {as.numeric(quantile(x, 0.05, na.rm=TRUE))}
	stats_function[['q25']] = function(x) {as.numeric(quantile(x, 0.25, na.rm=TRUE))}
	stats_function[['median']] = function(x) {as.numeric(median(x, na.rm=TRUE))}
	stats_function[['q75']] = function(x) {as.numeric(quantile(x, 0.75, na.rm=TRUE))}
	stats_function[['q95']] = function(x) {as.numeric(quantile(x, 0.95, na.rm=TRUE))}
	
	
	## Aggregate ##
	
	# if "aggr_stats" parameter has no names, they are created. Note: in this case they must be given in this order!
	if (is.null(names(aggr_stats))) {names(aggr_stats) = c('mean','sd','min','max','q05','q25','median','q75','q95')}
	
	# First sublist: metrics of N_Seasons
	cat('Computing metrics for variable N_Seasons ( 1 of',ncol(phenorice_dt)-2,')...\n')
	aggr_dt_N_Seasons = phenorice_dt[,list(
					metric = 'N_Seasons',
					n_pixels = length(N_Seasons),
					area = length(N_Seasons)*prod(phenorice_cellsize)/1E4, # in hectares
					n_isrice   = sum(N_Seasons>0),
					n_1season  = sum(N_Seasons==1),
					n_2seasons = sum(N_Seasons==2),
					n_3seasons = sum(N_Seasons==3),
					n_4seasons = sum(N_Seasons==4),
					mode = as.numeric(modal(N_Seasons[N_Seasons>0]))
			),by=list(Adm_Name,Adm_ID)]
	setkey(aggr_dt_N_Seasons, 'Adm_ID')
	
	# Other sublist: n_pixels and selected metrics
	aggr_dt_list <- list() # list of partial aggregate tables (one per names(phenorice_dt) )
	
	# Use Min_DOY columns to select the pixels to use for each quarter of season
	record_touse <- data.frame("X_1st"=rep(NA,nrow(phenorice_dt)), "X_2nd"=NA, "X_3rd"=NA, "X_4th"=NA )
	record_touse[["X_1st"]] = phenorice_dt[["Min_DOY_1st_Quarter"]]>0&phenorice_dt[["Min_DOY_1st_Quarter"]]<=366
	record_touse[["X_2nd"]] = phenorice_dt[["Min_DOY_2nd_Quarter"]]>0&phenorice_dt[["Min_DOY_2nd_Quarter"]]<=366
	record_touse[["X_3rd"]] = phenorice_dt[["Min_DOY_3rd_Quarter"]]>0&phenorice_dt[["Min_DOY_3rd_Quarter"]]<=366
	record_touse[["X_4th"]] = phenorice_dt[["Min_DOY_4th_Quarter"]]>0&phenorice_dt[["Min_DOY_4th_Quarter"]]<=366
	
	for (variab in names(phenorice_dt)[-c(1:3)]) {
		
		cat('Computing metrics for variable',variab,'(',which(names(phenorice_dt)==variab)-2,'of',ncol(phenorice_dt)-2,')...\n')
		
		# compute the metrics for each variab
		sel_quarter = unlist(strsplit(variab,'_'))[length(unlist(strsplit(variab,'_')))-1]
		aggr_dt_list[[variab]] = phenorice_dt[record_touse[[paste0('X_',sel_quarter)]],
				c(
						n_pixels=length(get(variab)),
						area = length(get(variab))*prod(phenorice_cellsize)/1E4, # in hectares
						as.list(sapply(names(aggr_stats), function(x) {if (aggr_stats[x]) stats_function[[x]](get(variab))})[aggr_stats])
				# [as.list(sapply()) instead of lapply() to preserve item names]
				),by=Adm_ID]
		
		aggr_dt_list[[variab]] = cbind('metric' = variab,
				'Adm_Name' = phenorice_dt[['Adm_Name']][match(unique(phenorice_dt[['Adm_ID']]),phenorice_dt[['Adm_ID']])],
				aggr_dt_list[[variab]][CJ(unique(phenorice_dt[['Adm_ID']])),])
		
	}
	cat('[',date(),'] Done.\n\n')
	
	# Merge subtables
	aggr_dt <- rbindlist(aggr_dt_list)
	aggr_dt[['n_pixels']][is.na(aggr_dt[['n_pixels']])] <-0
	aggr_dt[['area']][is.na(aggr_dt[['area']])] <-0
	rm(aggr_dt_list); gc()
	
	# If specified, export outputs as csv and/or xlsx
	if (!is.na(csv_filename)) {
		cat('Exporting outputs as CSV...\n')
		write.csv(aggr_dt, file.path(main_dir,paste0(csv_filename,'.csv')), row.names=FALSE, na='')
		write.csv(aggr_dt_N_Seasons, file.path(main_dir,paste0(csv_filename,'_N_Seasons.csv')), row.names=FALSE, na='')
		cat('[',date(),'] Done.\n\n')
	}
	if (!is.na(xlsx_filename)) {
		cat('Exporting outputs as XLSX...\n')
		write.xlsx2(aggr_dt, file.path(main_dir,paste0(xlsx_filename,'.xlsx')), sheetName='Metrics', row.names=FALSE, showNA=FALSE)
		write.xlsx2(aggr_dt_N_Seasons, file.path(main_dir,paste0(xlsx_filename,'.xlsx')), sheetName='N_Seasons', append=TRUE, row.names=FALSE, showNA=FALSE)
		# OCIO: write.xlsx2 does not work using RJ terminal in Eclipse! Use Rterm instead
		cat('[',date(),'] Done.\n\n')
	}
	
	# Return the two data tables as a list
	cat('[',date(),'] Finished!\n\n')
	return(list(N_Seasons = aggr_dt_N_Seasons, Other_Metrics = aggr_dt))
	
}


# Run function
phenorice_aggr_stats <- Phenorice_aggr_adm( country_code = country_code, gadm_level = gadm_level,
		phenorice_path = phenorice_path, main_dir = main_dir, aggr_stats = aggr_stats,
		xlsx=xlsx_filename, csv=csv_filename)

