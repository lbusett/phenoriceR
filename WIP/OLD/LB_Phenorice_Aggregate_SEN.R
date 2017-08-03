# TODO: Add comment
#
# Author: LB
###############################################################################

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


# Define inputs and outputs
main_folder = '//10.0.1.252/nr_working/shared/PhenoRice/Processing/Senegal/Outputs/50_30_30_120_dec_ok/newquarts/masked'
country_code = c('SEN')


years = seq(2003,2014,1)
for (year in years) {

  # main_folder = '//10.0.1.252/nr_working/shared/PhenoRice/Processing/IT/Outputs/2013/'
  # country_code = c('ITA')

  gadm_level = 3
  phrice_outfolder = file.path(main_folder,year,'raster')
  outputs_folder = "D:/Temp/PhenoRice/Processing/SEN/Outputs/newquarts/masked/"		# Folder where to put results of this processing
  year_out_folder = paste0(outputs_folder,year)
  out_mod_grid_df_file = file.path(year_out_folder,paste0('out_df_modgrid_',year,'.RData'))
  # refmap_file= 'D:/Temp/PhenoRice/Processing/PHL/Outputs/2013/Validate/Reference/PHL_DS_2013_Reference.tif'

  #- ------------------------------------------------------------------------------- -#
  # Initialize: setup input/output files and folders
  #- ------------------------------------------------------------------------------- -#

  in_phrice_file = list.files(phrice_outfolder, glob2rx('*Full_Out*dat'),full.names  = TRUE)

  grid_folder = file.path(outputs_folder,'GRIDs')   #save the grid here
  grid_resolutions = c(231.656358,2084.907, 5096.432, 10192.864,20385.73 )		# Resolution of output grids
  grid_shapes <- c('Grid_MODIS_250','Grid_2Km','Grid_5Km','Grid_10Km','Grid_20Km')		 #this is the grid name


  #
   dir.create(outputs_folder, recursive = T)
   dir.create(year_out_folder, recursive = T)
  # out_repro_raster= file.path(outputs_folder,'Reference','Reproj','Reference_Map_SIN.tif')
  # dir.create(dirname(out_repro_raster), recursive = T)
  in_phrice_file_small = in_phrice_file

  # in_phrice_file_small = file.path(outputs_folder,'Input','Clipped','Phenorice_Clipped.tif')
  # in_phrice_file_small_vrt= file.path(outputs_folder,'Input','Clipped','Phenorice_Clipped.vrt')
  # dir.create(dirname(in_phrice_file_small),recursive = T)

  out_grid_rasters = file.path(outputs_folder,'Temporary',paste(grid_shapes,'_raster_HR.tiff',sep = ''))
  grid_fullnames = paste(grid_folder,'/',grid_shapes,'.shp',sep = '')
  out_grid_df_files = file.path(outputs_folder,paste('out_df_',grid_shapes,'_',year,'.RData', sep = ''))
  out_grid_shp_files = file.path(outputs_folder,paste('out_shp_',grid_shapes,'.shp', sep = ''))


  #- ------------------------------------------------------------------------------- -#
  #  Load Phenorice map and retrieve projection and extent
  #- ------------------------------------------------------------------------------- -#
  phrice_map = raster(in_phrice_file)
  in_proj = proj4string(phrice_map)
  ext_phrice_map = extent(phrice_map)


  #- ------------------------------------------------------------------------------- -#
  #  Load reference map and define the extent
  #- ------------------------------------------------------------------------------- -#
  # refmap = raster(refmap_file)


  #- ------------------------------------------------------------------------------- -#
  # Reproject the reference raster map to MODIS projection
  #- ------------------------------------------------------------------------------- -#
  # {{
  # 		in_ref = raster(refmap_file)
  # 		in_ref_proj = proj4string(in_ref)
  # 		if(file.exists(out_repro_raster) ==	FALSE) {
  # 			ref_reproj = gdalwarp(refmap_file,out_repro_raster, s_srs = in_ref_proj, t_srs = in_proj,
  # 					r = "near",of = 'GTiff', overwrite = 	T, srcnodata = 255, dstnodata = 255,ot = 'Byte', output_Raster = TRUE)
  # 			ext_ref_repr = extent(ref_reproj)
  # 			res_ref_reproj = res(ref_reproj)
  # 		} else {
  # 			ref_reproj = raster(out_repro_raster)
  # 			ext_ref_repr = extent(ref_reproj)
  # 			res_ref_reproj = res(ref_reproj)
  #
  # 		}}

  #- ----------------------		---	--------------------------------------------------- -#
  #  Clip phenorice output on extent of reference map, using tap to avoid moving the corner
  #- ------------------------------------------------------------------------------- -#
  #
  # 	gdalbuildvrt(in_phrice_file, in_phrice_file_small_vrt, tr = c(231.656358,231.656358), te = c(bbox(ext_ref_repr)), tap = TRUE)
  # 	GdalTranslate  =  gdal_translate(in_phrice_file_small_vrt, in_phrice_file_small, overwrite = TRUE)
  in_phrice_clipped = raster(in_phrice_file_small)

  # 	gdalbuildvrt(in_phrice_file, in_phrice_file_small_vrt, tr = c(231.656358,231.656358), te = c(bbox(ext_ref_repr)), tap = TRUE)
  # 	GdalTranslate  =  gdal_translate(in_phrice_file_small_vrt, in_phrice_file_small, overwrite = TRUE)
  # 	in_phrice_clipped = raster(in_phrice_file)
  # 	in_phrice_file_small = in_phrice_file
  grid_extent = extent(in_phrice_clipped)

  #- ------------------------------------------------------------------------------- -#
  # Create the regular grids (MODIS + 2,5,10,20 K) if needed
  #- ------------------------------------------------------------------------------- -#


  for (gr_res in seq(along = grid_fullnames)) {

    LB_Create_pol_grid = function(in_raster_file, out_res, out_folder, out_name_shp,grid_extent){

      library(rgdal)
      library(raster)
      rast<- raster(in_raster_file)
      #	bb <- bbox(rast)                          # Define Bounding box
      cs <- c(out_res,out_res)  # cell size. To be set by hand!

      cc <- c(grid_extent@xmin,grid_extent@ymin) + (cs/2)  # cell offset   # move 1/2 pixel if necessary

      # compute number of cells per direction
      if (cs < 300) {				# needed to avoid that larger grids cover only one portion of the area --> so, add a row/coulmn for larger grids
        cd <- c(dim(rast)[2],dim(rast)[1])
      } else {
        cd <- ceiling(c(((grid_extent@xmax-grid_extent@xmin)/cs[1]),((grid_extent@ymax-grid_extent@ymin)/cs[2])))
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


    if(file.exists(grid_fullnames[gr_res]) == FALSE) {
      LB_Create_pol_grid(in_phrice_clipped , grid_resolutions[gr_res],grid_folder, grid_shapes[gr_res],grid_extent)
    }

  } #End Cycle on gr_res


  #- ---------------------	--------------------------------------------------------- -#
  # If not already existing, Rasterize polygons on the extent of the reference map, at a resolution
  # equal to that of the reference map. For each pixel, values of this raster correspond to a
  # unique identifier of the cell of the  ERMES grid to which the pixel belongs
  #- ------------------------------------------------------------------------------- -#

  for (cycle in seq(along = grid_shapes)) {


    if (cycle == 1) {


      #       if (file.exists(out_grid_rasters[cycle]) == FALSE){
      #         dir.create(dirname(out_grid_rasters[cycle]) ,recursive = T)
      #         raster_grid = gdal_rasterize(grid_fullnames[cycle],out_grid_rasters[cycle],a = "id", output_Raster = T,
      #                                      te = c(ext_ref_repr@xmin, (ext_ref_repr@ymin+res_ref_reproj[1]),(ext_ref_repr@xmax-res_ref_reproj[1]), (ext_ref_repr@ymax)), ot = 'UInt32',tr = res_ref_reproj, tap = T, of = 'GTiff')
      #         gc()
      #       }

      #- --------------			------		---		--		--------------------------------------------- -#
      # Compute the percentage area classified as rice for each pixel
      #- ------------------------------------------------------------------------------- -#


      # Get the values of t		 "du				 r				r of cells codes
      # 			raster_cells <- new("GDALReadOnlyDataset", out_grid_rasters[cycle])
      # 			data_raster_cells = getRasterData(raster_cells)
      # 			dim(data_raster_cells) = dim(data_raster_cells)[1]*dim(data_raster_cells)[2]
      # 			GDAL.close(raster_cells)
      # 			gc()
      #
      # 			# Get the values of the "reference map
      # 			ref_map <- new("GDALReadOnlyDataset",out_repro_raster)
      # 			ref_map_data = getRasterData(ref_map)
      # 			dim(ref_map_data) = dim(ref_map_data)[1]*dim(ref_map_data)[2]
      # 			GDAL.close(ref_map)
      # 			gc()

      # Create a Data.table with two columns. First column taken from the cell_codes raster - tells to which cell each pixel correspond
      # Second column taken from the reference rice map (0 or 1)
      #			browser()
      # 			cells_dt = data.table(id=data_raster_cells,value = ref_map_data )
      # 			setkey(cells_dt, id)  # Set a indexing key to increase speed of zonal statistics computation
      # 			rm(ref_map_data)
      # 			rm(data_raster_cells)
      # 			gc()
      #
      # 			# Compute the aggregated Reference Map values for each cell of the ERMES grid - average, stdev, min, max and n? of pixels in each cell
      # 			aggregate_reference_values = cells_dt[, list(ref_rice_fc = mean(value, na.rm = T),
      #
      # 							n_cells = as.numeric(length(which(is.finite(value) == T )))
      # 					), by = key(cells_dt)]
      #
      # 			aggregate_reference_values = aggregate_reference_values[id != 0 ]
      # 			rm(cells_dt)

      #- ---				--------------			----		--		--------------------------------------------- -#
      # Retrieve necessary data from the 		enorice output rasters
      #- --------------------------------		--------------------------------------------- -#

      rice_rast = new("GDALReadOnlyDataset", in_phrice_file_small)
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
      soss_q1 = data_rice[,,14] ; dim(soss_q1) = dim(soss_q1)[1]*dim(soss_q1)[2]
      soss_q2 = data_rice[,,15] ; dim(soss_q2) = dim(soss_q2)[1]*dim(soss_q2)[2]
      soss_q3 = data_rice[,,16] ; dim(soss_q3) = dim(soss_q3)[1]*dim(soss_q3)[2]
      soss_q4 = data_rice[,,17] ; dim(soss_q4) = dim(soss_q4)[1]*dim(soss_q4)[2]
      flows_q1 = data_rice[,,34] ; dim(flows_q1) = dim(flows_q1)[1]*dim(flows_q1)[2]
      flows_q2 = data_rice[,,35] ; dim(flows_q2) = dim(flows_q2)[1]*dim(flows_q2)[2]
      flows_q3 = data_rice[,,36] ; dim(flows_q3) = dim(flows_q3)[1]*dim(flows_q3)[2]
      flows_q4 = data_rice[,,37] ; dim(flows_q4) = dim(flows_q4)[1]*dim(flows_q4)[2]

      GDAL.close(rice_rast)

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
      phrice_res_df$sos_q1 = as.numeric(soss_q1)
      phrice_res_df$sos_q2 = as.numeric(soss_q2)
      phrice_res_df$sos_q3 = as.numeric(soss_q3)
      phrice_res_df$sos_q4 = as.numeric(soss_q4)
      phrice_res_df$flow_q1 = as.numeric(flows_q1)
      phrice_res_df$flow_q2 = as.numeric(flows_q2)
      phrice_res_df$flow_q3 = as.numeric(flows_q3)
      phrice_res_df$flow_q4 = as.numeric(flows_q4)
      phrice_res_df [phrice_res_df == 0 ] = NA
      phrice_res_df$min_q1 [which(phrice_res_df$min_q1 >= 400 )] = NA
      phrice_res_df$min_q2 [which(phrice_res_df$min_q2  >= 400 )] = NA
      phrice_res_df$min_q3 [which(phrice_res_df$min_q3  >= 400 )] = NA
      phrice_res_df$min_q4 [which(phrice_res_df$min_q4  >= 400 )] = NA
      phrice_res_df$max_q1 [which(phrice_res_df$max_q1 >= 400 )] = NA
      phrice_res_df$max_q2 [which(phrice_res_df$max_q2  >= 400 )] = NA
      phrice_res_df$max_q3 [which(phrice_res_df$max_q3  >= 400 )] = NA
      phrice_res_df$max_q4 [which(phrice_res_df$max_q4  >= 400 )] = NA
      phrice_res_df$sos_q1 [which(phrice_res_df$sos_q1 >= 400 )] = NA
      phrice_res_df$sos_q2 [which(phrice_res_df$sos_q2  >= 400 )] = NA
      phrice_res_df$sos_q3 [which(phrice_res_df$sos_q3  >= 400 )] = NA
      phrice_res_df$sos_q4 [which(phrice_res_df$sos_q4  >= 400 )] = NA
      phrice_res_df$flow_q1 [which(phrice_res_df$flow_q1 >= 400 )] = NA
      phrice_res_df$flow_q2 [which(phrice_res_df$flow_q2  >= 400 )] = NA
      phrice_res_df$flow_q3 [which(phrice_res_df$flow_q3  >= 400 )] = NA
      phrice_res_df$flow_q4 [which(phrice_res_df$flow_q4  >= 400 )] = NA
      phrice_res_df$length_q1 = (phrice_res_df$max_q1+365)-(phrice_res_df$min_q1+365)
      phrice_res_df$length_q2 = (phrice_res_df$max_q2+365)-(phrice_res_df$min_q2+365)
      phrice_res_df$length_q3 = (phrice_res_df$max_q3+365)-(phrice_res_df$min_q3+365)
      phrice_res_df$length_q4 = (phrice_res_df$max_q4+365)-(phrice_res_df$min_q4+365)

      rm(mins_q1,mins_q2,mins_q3,mins_q4)
      rm(maxs_q1,maxs_q2,maxs_q3,maxs_q4)
      rm(soss_q1,soss_q2,soss_q3,soss_q4)
      rm(flows_q1,flows_q2,flows_q3,flows_q4)
      gc()

      #- ------------------------------------------------------------------------------- -#
      # Get all the retrieved datasets and put them in a single data frame and save outputs
      #- ------------------------------------------------------------------------------- -#

      # 			out_df_modgrid = data.frame(id = aggregate_reference_values$id, ncells = aggregate_reference_values$n_cells,
      # 					ref_rice_fc = aggregate_reference_values$ref_rice_fc)
      #
      # 			out_df_modgrid = join(out_df_modgrid,phrice_res_df, by = 'id', type = 'full' )
      #
      out_df_modgrid = phrice_res_df

      is.na(out_df_modgrid) <- do.call(cbind,lapply(out_df_modgrid, is.infinite))  # Convert Infinite to NA
      is.na(out_df_modgrid) <- do.call(cbind,lapply(out_df_modgrid, is.nan))
      # Compute the ricearea form the percentage of rice area+ncells+resolution of reference map + compute
      # total non-nodata area of the pixel
      # 			out_df_modgrid$ref_ricearea = out_df_modgrid$ref_rice_fc*out_df_modgrid$ncells*(res(ref_reproj)[1]^2)
      # 			out_df_modgrid$tot_cellarea = out_df_modgrid$ncells*(res(ref_reproj)[1]^2)


      # Download, load and reproject GADM boundaries
      # (gadm shapefiles have been used instead of RData since gdal_rasterize needs them)
      cat('Download and reproject GADM boundaries...\n')
      gadm_shp_url <- paste0('http://biogeo.ucdavis.edu/data/gadm2/shp/',country_code,'_adm.zip')
      gadm_shape_dir <- file.path(outputs_folder,'gadm',basename(file_path_sans_ext(gadm_shp_url)))
      dir.create(gadm_shape_dir, recursive=TRUE, showWarnings=FALSE)
      if (!file.exists(paste0(gadm_shape_dir,'.zip'))) {download.file(gadm_shp_url,paste0(gadm_shape_dir,'.zip'))}
      gadm_shape_name = paste0(country_code,'_adm',gadm_level,'_sinu.shp')
      if (!file.exists(gadm_shape_name)) {unzip(paste0(gadm_shape_dir,'.zip'), exdir=gadm_shape_dir)}
      if (!file.exists(file.path(gadm_shape_dir,gadm_shape_name))) {
        ogr2ogr(file.path(gadm_shape_dir,gsub('_sinu','',gadm_shape_name)), file.path(gadm_shape_dir,gadm_shape_name),
                f='ESRI Shapefile', s_srs='EPSG:4326', t_srs=in_proj)
      }
      in_shape_admin_reproj <- readOGR(gadm_shape_dir,file_path_sans_ext(gadm_shape_name))

      centroids = getSpPPolygonsLabptSlots(readOGR(grid_folder,grid_shapes[cycle]))	# get coordinates of centroids of each 250m cell
      centroids = SpatialPoints(centroids,proj4string =  CRS(in_proj)) # convert to spatialpoints
      admin_cells = over(centroids,in_shape_admin_reproj)

      admin_cells = admin_cells [,c("ISO",paste0("ID_", gadm_level),paste0("NAME_", gadm_level))]
      admin_cells$id = phrice_res_df$id
      out_df_modgrid = join(out_df_modgrid,admin_cells, by = 'id', type = 'left' )  # join to total df the info on Region and Province

      # SAve the outputs
      save(out_df_modgrid, file = out_mod_grid_df_file)		# Save the RData table
      #			out_shp_dbf = data.frame(id = phrice_res_df$id)			# Retrieve cell ids from the "empty grid
      #			out_shp_dbf = join(out_shp_dbf,out_df_modgrid, by = 'id', type = 'left' )  # join the info using id as base
      ##			write.dbf(out_shp_dbf, file.path(grid_folder,paste(grid_shapes[cycle],'.dbf', sep = ''))) # overwrite the dbf table of the MOD 250m grid shapefile
      gc()


    } else { # end of 1st cycle

      #- ------------------------------------------------------------------------------- -#
      #  Cycle on the other grids --> For each resolution grid, create a data frame in which
      # the information relative to the id of the "coarse" resolution cell to which each MODIS
      # pixel belongs is added to the data frame of "outputs" created befor (out_df_modgrid)
      #- ------------------------------------------------------------------------------- -#


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


    } #End else on cycle = 1

  } #En
}


# }
