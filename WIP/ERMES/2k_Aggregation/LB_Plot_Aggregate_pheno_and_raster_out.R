<<<<<<< HEAD
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
library(ggplot2)
library(reshape)
library(grid)
library(gridExtra)
library(hash)
library("scales")
library(tools)
memory.limit(8000)

countries = c('IT', 'ES','GR')


Main_Folder = "//10.0.1.252/projects/ermes/datasets/rs_products/Phenology/%cc%/Outputs/v1.0/ERMES_Grid"
Grid_Folder = "//10.0.1.252/projects/ermes/datasets/ERMES_Folder_Structure/%cc%/Regional/%cc%_Reference_Grid/%cc%_ERMES_Regional_Grid.shp"
admin_shape = "//10.0.1.252/projects/ermes/datasets/rs_products/Phenology/Ancillary_Datasets/World_Provinces/provinces_world_laea_ermes.shp"

vers = '001'
selvar = c(1,0,0,0)

start_year = 2015
end_year 	 = 2015

laea_crs = CRS("+init=epsg:3035 +proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0 +units=m +no_defs")
geo_WGS84_crs = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0,0,0,0 ")

savepdf <- function(file, width=24, height=29.7)
{ fname <- file
	pdf(fname, width=width/2.54, height=height/2.54,
			pointsize=10)
}
for (country in countries) {

	ermes_grid = str_replace_all(Grid_Folder,"%cc%",country)
	in_RData_file = file.path(str_replace_all(Main_Folder,"%cc%",country),'RData',paste0('ERMES_Aggregates_',start_year,'_',end_year,'.RData'))
	data_in = get(load(file = in_RData_file))
	out_folder = file.path(str_replace_all(Main_Folder,"%cc%",country),'RData','pdf_plots')

	dir.create(out_folder, recursive = T)
	# Retieve spatialpointsdataframe of cells for Italy

	ERMES_cells_poly = readOGR(dirname(ermes_grid) , basename(file_path_sans_ext(ermes_grid)))
	ext_grd = extent(ERMES_cells_poly)

#  # Retieve a polygon map of italian regions

	mapit = readOGR(dirname(admin_shape),file_path_sans_ext(basename(admin_shape)),	drop_unsupported_fields=T)
	mapit_df = fortify(mapit, region = "name")
	mapit_data = data.frame(id = unique(mapit_df$id), value = rnorm(174))

	# Add spatial info to the data_in fata frame + add some dummy variables
	data_in$group = 1
	data_in = join(data_in, ERMES_cells_poly@data, by = 'int_id')
	data_in$variable = as.factor(data_in$variable)                 #  convert "variable" column to factor
	data_in$year = as.factor(data_in$year)                 #  convert "year" column to factor
	is.na(data_in) <- do.call(cbind,lapply(data_in, is.nan))
	data_in = subset(data_in, !is.na(mean))																	# remove data with "NA" in the mean column

	data_in$percol <- cut(100*data_in$perc_rice, breaks = c(0,1,10,20,30,40,50,60,70,80,90,100,110))     # catgorize the rice fc - 10 classes
	data_in$variable = factor(data_in$variable,levels(data_in$variable)[c(3,1,2,4)])  # reorder the variables
	data_in$rice_area = data_in$perc_rice*2000*2000           # compute retrieved area

	data_in = subset(data_in, is_rice == 1)  # ????? Consider if yes or not

	out_pdf = file.path(out_folder, 'grid_plots.pdf')
	savepdf(out_pdf)    # initialize plotting device

	if (country == 'IT') {ncols = 2} else {ncols = 4}

	# Build the plot for the rice fraction : Add points to the map, set colors and variables and set limits
	data_mindoy = droplevels(subset(data_in, variable == 'MinDoys'))
	p <- ggplot(data = data_mindoy, aes(x = x_LAEA, y = y_LAEA))
	mapfract <- p + facet_wrap(~year, ncol = ncols)
	mapfract <- mapfract + geom_tile(aes(fill = percol))
	mapfract = mapfract +coord_fixed(xlim = c(ext_grd@xmin,ext_grd@xmax), ylim = c(ext_grd@ymin,ext_grd@ymax))
	mapfract = mapfract + theme_bw() + labs(title = "Rice Cover Fraction", x = "Longitude", y = "Latitude")+ theme(plot.title = element_text(size = 14, vjust = 1))+
			labs(x = "Longitude") + theme(axis.text.x  = element_text(size = 8) ,axis.text.y  = element_text(size = 8))+ theme(legend.position="right")
	mapfract = mapfract + theme(plot.margin = unit(c(1,1,1,1), "cm"))
	mapfract <- mapfract + scale_fill_brewer('Rice cover Fraction', palette="RdYlGn")
	mapfract <- mapfract + geom_polygon(data = mapit_df,aes(x = long, y = lat, group = group),  fill = 'transparent', colour = "black")
	print(mapfract)


	if (selvar [1] == 1){
		# Build the plot for the mindoy maps : Add points to the map, set colors and variables and set limits
		mapmin <- p + facet_wrap(~year, ncol = ncols)
		mapmin <- mapmin + geom_tile(aes(fill = mean))
		mapmin <- mapmin + scale_fill_gradientn('Doy of Sowing',colours = topo.colors(10), limits=c(75, 175), oob=squish)
		mapmin = mapmin +coord_fixed(xlim = c(ext_grd@xmin,ext_grd@xmax), ylim = c(ext_grd@ymin,ext_grd@ymax))
		mapmin = mapmin + theme_bw() + labs(title = "Doys of Sowing", x = "Longitude", y = "Latitude")+ theme(plot.title = element_text(size = 14, vjust = 1))+
				labs(x = "Longitude") + theme(axis.text.x  = element_text(size = 8) ,axis.text.y  = element_text(size = 8))+ theme(legend.position="right")
		mapmin = mapmin + theme(plot.margin = unit(c(1,1,1,1), "cm"))
		mapmin <- mapmin + geom_polygon(data = mapit_df,aes(x = long, y = lat, group = group),  fill = 'transparent', colour = "black")
		levels(data_in$variable)[1] = 'DOY of Sowing'
		print(mapmin)
	}


	# Build the plot for the sos doy maps : Add points to the map, set colors and variables and set limits
	if (selvar [3] == 1){
		data_sos = droplevels(subset(data_in, variable == 'SosDoys'))
		p <- ggplot(data = data_sos, aes(x = x_LAEA, y = y_LAEA))
		mapsos <- p + facet_wrap(~year, ncol = ncols)
		mapsos <- mapsos + geom_tile(aes(fill = mean))
		mapsos <- mapsos + scale_fill_gradientn('DOY of Emergence',colours = topo.colors(10), limits=c(105, 205), oob=squish)
		mapsos = mapsos +coord_fixed(xlim = c(ext_grd@xmin,ext_grd@xmax), ylim = c(ext_grd@ymin,ext_grd@ymax))
		mapsos = mapsos + theme_bw() + labs(title = "DOY of Emergence", x = "Longitude", y = "Latitude")+ theme(plot.title = element_text(size = 14, vjust = 1))+
				labs(x = "Longitude") + theme(axis.text.x  = element_text(size = 8) ,axis.text.y  = element_text(size = 8))+ theme(legend.position="right")
		mapsos = mapsos + theme(plot.margin = unit(c(1,1,1,1), "cm"))
		mapsos <- mapsos + geom_polygon(data = mapit_df,aes(x = long, y = lat, group = group),  fill = 'transparent', colour = "black")
		levels(data_in$variable)[4] = 'DOY of Emergence'
		print(mapsos)
	}

	if (selvar [2] == 1){
		# Build the plot for the maxdoy maps : Add points to the map, set colors and variables and set limits

		data_maxdoy = droplevels(subset(data_in, variable == 'MaxDoys'))
		p <- ggplot(data = data_maxdoy, aes(x = x_LAEA, y = y_LAEA))
		mapmax <- p + facet_wrap(~year, ncol = ncols)
		mapmax <- mapmax + geom_tile(aes(fill = mean))
		mapmax <- mapmax + scale_fill_gradientn('Doy of Heading',colours = topo.colors(10), limits=c(185, 285), oob=squish)
		mapmax = mapmax +coord_fixed(xlim = c(ext_grd@xmin,ext_grd@xmax), ylim = c(ext_grd@ymin,ext_grd@ymax))
		mapmax = mapmax + theme_bw() + labs(title = "Doys of Heading", x = "Longitude", y = "Latitude")+ theme(plot.title = element_text(size = 14, vjust = 1))+
				labs(x = "Longitude") + theme(axis.text.x  = element_text(size = 8) ,axis.text.y  = element_text(size = 8))+ theme(legend.position="right")
		mapmax = mapmax + theme(plot.margin = unit(c(1,1,1,1), "cm"))
		mapmax <- mapmax + geom_polygon(data = mapit_df,aes(x = long, y = lat, group = group),  fill = 'transparent', colour = "black")
		levels(data_in$variable)[2] = 'DOY of Heading'
		print(mapmax)
	}

	# Build the plot for the maxvi maps : Add points to the map, set colors and variables and set limits
	if (selvar [4] == 1){
		data_maxvi = droplevels(subset(data_in, variable == 'MaxVis'))
		p <- ggplot(data = data_maxvi, aes(x = x_LAEA, y = y_LAEA))
		mapvimax <- p + facet_wrap(~year, ncol = ncols)
		mapvimax <- mapvimax + geom_tile(aes(fill = mean))
		mapvimax <- mapvimax + scale_fill_gradientn('Value of VI at Heading',colours = topo.colors(10), limits=c(5000, 8000), oob=squish)
		mapvimax = mapvimax +coord_fixed(xlim = c(ext_grd@xmin,ext_grd@xmax), ylim = c(ext_grd@ymin,ext_grd@ymax))
		mapvimax = mapvimax + theme_bw() + labs(title = "Values of VI at Heading", x = "Longitude", y = "Latitude")+ theme(plot.title = element_text(size = 14, vjust = 1))+
				labs(x = "Longitude") + theme(axis.text.x  = element_text(size = 8) ,axis.text.y  = element_text(size = 8))+ theme(legend.position="right")
		mapvimax = mapvimax + theme(plot.margin = unit(c(1,1,1,1), "cm"))
		mapvimax <- mapvimax + geom_polygon(data = mapit_df,aes(x = long, y = lat, group = group),  fill = 'transparent', colour = "black")
		levels(data_in$variable)[2] = 'Value of VI at Heading'
		print(mapvimax)
	}

# Build the boxplot of interannual variability of the four variables along years

#	levels(data_in$variable) = c( 'DOY of Sowing', 'DOY of Heading', 'Value of VI at Heading', 'DOY of Emergence')
	data_in$variable = factor(data_in$variable, levels = c( 'DOY of Sowing','DOY of Emergence', 'DOY of Heading', 'Value of VI at Heading' ))
	blank_data <- data.frame(variable = rep(c("DOY of Sowing", "DOY of Sowing","DOY of Emergence","DOY of Emergence", "DOY of Heading", "DOY of Heading",
					"Value of VI at Heading", "Value of VI at Heading"), 13),  y = rep(c(75,175,105,205,185,285,5000,8000),13), x = rep(levels(data_in$year), each = 2))

	data_in$mean[which(data_in$variable == "DOY of Sowing" &  data_in$mean > 175)] = NA
	data_in$mean[which(data_in$variable == "DOY of Sowing"&  data_in$mean < 75)] = NA
	data_in$mean[which(data_in$variable == "DOY of Emergence"&  data_in$mean > 205)] = NA
	data_in$mean[which(data_in$variable == "DOY of Emergence"&  data_in$mean < 105)] = NA
	data_in$mean[which(data_in$variable == "DOY of Heading"&  data_in$mean > 285)] = NA
	data_in$mean[which(data_in$variable == "DOY of Heading"&  data_in$mean < 185)] = NA

#
#	boxp1 = ggplot (data_in, aes (x = year, y = mean))
#	boxp1 = boxp1 + geom_blank(data = blank_data, aes(x = x, y = y))+ facet_wrap(~variable)
#	boxp1 = boxp1 + geom_boxplot(outlier.colour = 'transparent') + facet_wrap(~variable, scales = "free_y") + theme_bw()
#
#	boxp1 = boxp1 + theme_bw() + labs(title = "Interannual Variability of Retrieved Variables", x = "Year", y = "Values")+
#			theme(plot.title = element_text(size = 14, vjust = 1))+
#			theme(axis.text.x  = element_text(size = 8) ,axis.text.y  = element_text(size = 8))+ theme(legend.position="right")


	# Build the boxplot of interannual variability of the four variables along years - above 10% of cover

	boxp2 = ggplot (data = subset(data_in, perc_rice > 0.1), aes (x = year, y = mean))
	boxp2 = boxp2 + geom_boxplot(outlier.colour = 'transparent') + facet_wrap(~variable, scales = "free_y", drop = T) + theme_bw()
	boxp2 = boxp2 + theme_bw() + labs(title = "Interannual Variability of Retrieved Variables - cells above 10 % cover", x = "Year", y = "Values")+
			theme(plot.title = element_text(size = 14, vjust = 1))+
			theme(axis.text.x  = element_text(size = 8) ,axis.text.y  = element_text(size = 8))+ theme(legend.position="right")
	boxp2 = boxp2 + geom_blank(data = blank_data, aes(x = x, y = y))
	print(boxp2)
#	print(grid.arrange( boxp1, boxp2, ncol=1))

	# Build the barplot of interannual variability of the detected rice area along years
#	data_sub =  subset(data_in, variable == 'DOY of Sowing')
#	data_sub_area = ddply(data_sub, .(year), summarize, tot_area = sum(rice_area)/10000)
#	barplot1 = ggplot(data_sub_area, aes(x = year, y = tot_area))
#	barplot1 = barplot1 + geom_bar(stat = 'identity')
#	barplot1 = barplot1 + theme_bw() + labs(title = "Interannual Variability of total estimated rice area", x = "Year", y = "Area (ha)")+
#			theme(plot.title = element_text(size = 14, vjust = 1))+ theme(axis.text.x  = element_text(size = 8) ,axis.text.y  = element_text(size = 8))


	# Build the barplot of interannual variability of the detected rice area - above 10% of cover
	data_sub =  subset(data_in, variable == 'DOY of Sowing' & perc_rice > 0.1)
	data_sub_area = ddply(data_sub, .(year), summarize, tot_area = sum(rice_area)/10000)
	barplot2 = ggplot(data_sub_area, aes(x = year, y = tot_area))
	barplot2 = barplot2 + geom_bar(stat = 'identity')
	barplot2 = barplot2 + theme_bw() + labs(title = "Interannual Variability of total estimated rice area - cells above 10 % cover", x = "Year", y = "Area (ha)")+
			theme(plot.title = element_text(size = 14, vjust = 1))+ theme(axis.text.x  = element_text(size = 8) ,axis.text.y  = element_text(size = 8))
	print(barplot2)
#	print(grid.arrange( barplot1, barplot2, ncol=1))

	dev.off()
}    #End Cycle on countries





#- ------------------------------------------------------------------------------- -#
#  Code to compute the deltas wrt period average
#- ------------------------------------------------------------------------------- -#
{{

		#
=======
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
  library(ggplot2)
  library(reshape)
  library(grid)
  library(gridExtra)
	library(hash)
	library("scales")
	library(tools)
  memory.limit(8000)

	countries = c('IT')

#	countries = c('IT','ES','GR')

	phenorice_out_folders = hash('IT' = '//10.0.1.252/nr_working/lorenzo/phenorice/lombardy/ERMES_Subset/Processing_v21/old_min_identification/Outputs/ndii_new',
			'ES' = '//10.0.1.252/nr_working/lorenzo/phenorice/Spain/ERMES_Subset/Processing_v21/old_min_identification/Outputs/ndii_new',
			'GR'= '//10.0.1.252/nr_working/lorenzo/phenorice/Greece/ERMES_Subset/Processing_v21/old_min_identification/Outputs/ndii_new')


	ermes_grids = hash('IT' = '//10.0.1.252/ftp/ERMES/ERMES_Archive/Italy/Vector/Reference_ERMES_Grid/shape_polygons/ERMES_Grid_Italy_shp_LAEA_poly.shp',
			'ES' = '//10.0.1.252/ftp/ERMES/ERMES_Archive/Spain/Vector/Reference_ERMES_Grid/shape_polygons/ERMES_Grid_Spain_shp_LAEA.shp',
			'GR'= '//10.0.1.252/ftp/ERMES/ERMES_Archive/Greece/Vector/Reference_ERMES_Grid/shape_polygons/ermes_Grid_Greece_shp_LAEA.shp')


	admin_shape = ('//10.0.1.252/nr_working/lorenzo/phenorice/ancillary_datasets/provinces_world_laea_sub.shp')


  laea_crs = CRS("+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0 +units=m +no_defs")
  geo_WGS84_crs = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0,0,0,0 ")

  savepdf <- function(file, width=24, height=29.7)
  { fname <- file
    pdf(fname, width=width/2.54, height=height/2.54,
        pointsize=10)
  }
	for (country in countries) {
	ermes_grid = ermes_grids[[country]]
	in_RData_file = file.path(phenorice_out_folders[[country]],'Grid_2km_new','RData','ERMES_Aggregates_2003_2014.RData')
  data_in = get(load(file = in_RData_file))
  out_folder = file.path(phenorice_out_folders[[country]],'Grid_2km_new','pdf_plots')

  dir.create(out_folder, recursive = T)
  # Retieve spatialpointsdataframe of cells for Italy

  ERMES_cells_poly = readOGR(dirname(ermes_grid) , basename(file_path_sans_ext(ermes_grid)))

#  ERMES_cells_RData = 'Y:/ermes/Activities/WP5/Task5.1/Study_Areas/Italy/ERMES_Grid_Italy_LAEA.RData'
#  ERMES_cells_Rice = readOGR(dsn = 'Y:/ermes/Activities/WP5/Task5.1/Study_Areas/Italy/shape_points', "cell_id_Italy_Rice_LAEA")
#  ERMES_cells = get(load(file = ERMES_cells_RData))
#  ERMES_cells@data$riceflag = ERMES_cells_Rice@data$Cell_rice
#  ERMES_cells@data$cell_ids = as.integer(seq(1,9135,1))
  ext_grd = extent(ERMES_cells_poly)

#  # Retieve a polygon map of italian regions
#  con <- url('http://biogeo.ucdavis.edu/data/gadm2/R/ITA_adm2.RData')
#  load(con)
#  mapit = gadm

	mapit = readOGR(dirname(admin_shape),file_path_sans_ext(basename(admin_shape)),	drop_unsupported_fields=T)
#	mapit = spTransform(mapit, laea_crs)
  mapit_df = fortify(mapit, region = "name")
  mapit_data = data.frame(id = unique(mapit_df$id), value = rnorm(2028))

  # Add spatial info to the data_in fata frame + add some dummy variables
  data_in$group = 1
  data_in = join(data_in, ERMES_cells_poly@data, by = 'int_id')
  data_in$variable = as.factor(data_in$variable)                 #  convert "variable" column to factor
  data_in$year = as.factor(data_in$year)                 #  convert "year" column to factor
#  data_in$thresh = as.factor(data_in$thresh)                 #  convert "thresh" column to factor
  is.na(data_in) <- do.call(cbind,lapply(data_in, is.nan))
  data_in = subset(data_in, !is.na(mean))																	# remove data with "NA" in the mean column

  data_in$percol <- cut(100*data_in$perc_rice, breaks = c(0,1,10,20,30,40,50,60,70,80,90,100,110))     # catgorize the rice fc - 10 classes
  data_in$variable = factor(data_in$variable,levels(data_in$variable)[c(3,1,2,4)])  # reorder the variables
  data_in$rice_area = data_in$perc_rice*2000*2000           # compute retrieved area

	data_in = subset(data_in, is_rice == 1)  # ????? Consider if yes or not

#  thresh_levs = c('minthresh_2500','minthresh_3500','no_minthresh')
#
>>>>>>> 2b5a5dafc83c379ed8e956053b71225469462c0a
#  compute_deltas = function(df) {
#         data_2003 = df$value[which(df$year == '2003')]
#         data_2004 = df$value[which(df$year == '2004')]
#         data_2005 = df$value[which(df$year == '2005')]
#         data_2006 = df$value[which(df$year == '2006')]
#         data_2007 = df$value[which(df$year == '2007')]
#         data_2008 = df$value[which(df$year == '2008')]
#         data_2009 = df$value[which(df$year == '2009')]
#         data_2010 = df$value[which(df$year == '2010')]
#         data_2011 = df$value[which(df$year == '2011')]
#         data_2012 = df$value[which(df$year == '2012')]
#         data_2013 = df$value[which(df$year == '2013')]
#         data_2014 = df$value[which(df$year == '2014')]
#         browser
#    avg = mean(c(data_2003,data_2004,data_2005,data_2006,data_2007,data_2008,data_2009,data_2010,data_2011,data_2012,data_2013,data_2014), na.rm = T)
#
#    if (length(data_2003) >0) {delta_2003 = (data_2003-avg)} else {delta_2003 = NA}
#    if (length(data_2004) >0) {delta_2004 = (data_2004-avg)} else {delta_2004 = NA}
#    if (length(data_2005) >0) {delta_2005 = (data_2005-avg)} else {delta_2005 = NA}
#    if (length(data_2006) >0) {delta_2006 = (data_2006-avg)} else {delta_2006 = NA}
#    if (length(data_2007) >0) {delta_2007 = (data_2007-avg)} else {delta_2007 = NA}
#    if (length(data_2008) >0) {delta_2008 = (data_2008-avg)} else {delta_2008 = NA}
#    if (length(data_2009) >0) {delta_2009 = (data_2009-avg)} else {delta_2009 = NA}
#    if (length(data_2010) >0) {delta_2010 = (data_2010-avg)} else {delta_2010 = NA}
#    if (length(data_2011) >0) {delta_2011 = (data_2011-avg)} else {delta_2011 = NA}
#    if (length(data_2012) >0) {delta_2012 = (data_2012-avg)} else {delta_2012 = NA}
#    if (length(data_2013) >0) {delta_2013 = (data_2013-avg)} else {delta_2013 = NA}
#    if (length(data_2014) >0) {delta_2014 = (data_2014-avg)} else {delta_2014 = NA}
#
#    deltas = data.frame(delta_2003 = delta_2003,delta_2004 = delta_2004,delta_2005 = delta_2005,delta_2006 = delta_2006,
#                        delta_2007 = delta_2007,delta_2008 = delta_2008,delta_2009 = delta_2009,delta_2010 = delta_2010,
#                        delta_2011 = delta_2011,delta_2012 = delta_2012,delta_2013 = delta_2013,delta_2014 = delta_2014)
#return(deltas)
<<<<<<< HEAD
		##     out = data.frame(x = 10)
#  }
		# Build the dataset of deviations wrt the average 2003
=======
##     out = data.frame(x = 10)
#  }
	# Build the dataset of deviations wrt the average 2003
>>>>>>> 2b5a5dafc83c379ed8e956053b71225469462c0a
#  pro = data.frame(int_id = as.factor(as.character(data_in$int_id)), value = data_in$mean, variable = data_in$variable, year = data_in$year)
#  deviations = ddply(pro, .(variable, int_id), function(df) compute_deltas(df))
#  names(deviations)[1] = 'metric'
#  coords = data.frame(cell_ids = data_in$cell_ids, x_LAEA = data_in$x_LAEA, y_LAEA = data_in$y_LAEA)
#  deviations = join(deviations, coords,by = "cell_ids", type = 'left' )
#  dev_melted = melt(deviations, id.vars = c("metric","cell_ids","x_LAEA","y_LAEA"))
#
#  levels(dev_melted$variable) = c("2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014")
#  dev_melted$perchange <- cut(dev_melted$value, breaks = c(-25,-20,-15,-10,-5,5,10,15,20,25))
#  data_mindoy = droplevels(subset(dev_melted, metric == 'min_doys' & variable == '2014'))
#  p <- ggplot(data = data_mindoy, aes(x = x_LAEA, y = y_LAEA))
#  mapfract <- p + facet_wrap(~variable, ncol = 2)
#  mapfract <- mapfract + geom_tile(aes(fill = perchange))
#  mapfract = mapfract +coord_fixed(xlim = c(ext_grd@xmin,ext_grd@xmax), ylim = c(ext_grd@ymin,ext_grd@ymax))
#  mapfract = mapfract + theme_bw() + labs(title = "Flooding DOYS", x = "Longitude", y = "Latitude")+ theme(plot.title = element_text(size = 14, vjust = 1))+
#    labs(x = "Longitude") + theme(axis.text.x  = element_text(size = 8) ,axis.text.y  = element_text(size = 8))+ theme(legend.position="right")
#  mapfract = mapfract + theme(plot.margin = unit(c(1,1,1,1), "cm"))
#  mapfract <- mapfract + scale_fill_brewer('DOY Variation wrt 2003-2013 average', palette="RdYlGn")
#  mapfract1<- mapfract + geom_polygon(data = mapit_df,aes(x = long, y = lat, group = group),  fill = 'transparent', colour = "black")
<<<<<<< HEAD
		##   print(mapfract)
=======
##   print(mapfract)
>>>>>>> 2b5a5dafc83c379ed8e956053b71225469462c0a
#
#  data_maxdoy = droplevels(subset(dev_melted, metric == 'sos_doys' & variable == '2014'))
#  p <- ggplot(data = data_maxdoy, aes(x = x_LAEA, y = y_LAEA))
#  mapfract <- p + facet_wrap(~variable, ncol = 2)
#  mapfract <- mapfract + geom_tile(aes(fill = perchange))
#  mapfract = mapfract +coord_fixed(xlim = c(ext_grd@xmin,ext_grd@xmax), ylim = c(ext_grd@ymin,ext_grd@ymax))
#  mapfract = mapfract + theme_bw() + labs(title = "SOS DOYS", x = "Longitude", y = "Latitude")+ theme(plot.title = element_text(size = 14, vjust = 1))+
#    labs(x = "Longitude") + theme(axis.text.x  = element_text(size = 8) ,axis.text.y  = element_text(size = 8))+ theme(legend.position="right")
#  mapfract = mapfract + theme(plot.margin = unit(c(1,1,1,1), "cm"))
#  mapfract <- mapfract + scale_fill_brewer('DOY Variation to 2003-2013 average', palette="RdYlGn")
#  mapfract2 <- mapfract + geom_polygon(data = mapit_df,aes(x = long, y = lat, group = group),  fill = 'transparent', colour = "black")
<<<<<<< HEAD
		##   print(mapfract)
=======
##   print(mapfract)
>>>>>>> 2b5a5dafc83c379ed8e956053b71225469462c0a
#
#  data_maxdoy = droplevels(subset(dev_melted, metric == 'max_doys' & variable == '2014'))
#  p <- ggplot(data = data_maxdoy, aes(x = x_LAEA, y = y_LAEA))
#  mapfract <- p + facet_wrap(~variable, ncol = 2)
#  mapfract <- mapfract + geom_tile(aes(fill = perchange))
#  mapfract = mapfract +coord_fixed(xlim = c(ext_grd@xmin,ext_grd@xmax), ylim = c(ext_grd@ymin,ext_grd@ymax))
#  mapfract = mapfract + theme_bw() + labs(title = "Maximum DOYS", x = "Longitude", y = "Latitude")+ theme(plot.title = element_text(size = 14, vjust = 1))+
#    labs(x = "Longitude") + theme(axis.text.x  = element_text(size = 8) ,axis.text.y  = element_text(size = 8))+ theme(legend.position="right")
#  mapfract = mapfract + theme(plot.margin = unit(c(1,1,1,1), "cm"))
#  mapfract <- mapfract + scale_fill_brewer('DOY Variation to 2003-2013 average', palette="RdYlGn")
#  mapfract3 <- mapfract + geom_polygon(data = mapit_df,aes(x = long, y = lat, group = group),  fill = 'transparent', colour = "black")
<<<<<<< HEAD
		##   print(mapfract)
=======
##   print(mapfract)
>>>>>>> 2b5a5dafc83c379ed8e956053b71225469462c0a
#
#
#  levels(dev_melted$variable) = c("2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014")
#  data_mindoy = droplevels(subset(data, variable == 'min_doys' & year == '2014'))
#  data_mindoy$catdate <- cut(data_mindoy$mean, breaks = c(90,100,110,120,130,140,150,160,170,180))
#  p <- ggplot(data = data_mindoy, aes(x = x_LAEA, y = y_LAEA))
#  mapfract <- p + facet_wrap(~year, ncol = 2)
#  mapfract <- mapfract + geom_tile(aes(fill =  catdate))
#  mapfract = mapfract +coord_fixed(xlim = c(ext_grd@xmin,ext_grd@xmax), ylim = c(ext_grd@ymin,ext_grd@ymax))
#  mapfract = mapfract + theme_bw() + labs(title = "Flooding DOYS", x = "Longitude", y = "Latitude")+ theme(plot.title = element_text(size = 14, vjust = 1))+
#    labs(x = "Longitude") + theme(axis.text.x  = element_text(size = 8) ,axis.text.y  = element_text(size = 8))+ theme(legend.position="right")
#  mapfract = mapfract + theme(plot.margin = unit(c(1,1,1,1), "cm"))
#  mapfract <- mapfract  + scale_fill_brewer('DOYs', palette="RdYlGn")
#  mapfract1<- mapfract + geom_polygon(data = mapit_df,aes(x = long, y = lat, group = group),  fill = 'transparent', colour = "black")
#  #   print(mapfract)
#
#  data_maxdoy = droplevels(subset(data, variable == 'sos_doys' & year == '2014'))
#  data_maxdoy$catdate <- cut(data_maxdoy$mean, breaks = c(110,120,130,140,150,160,170,180,190,200))
#  p <- ggplot(data = data_maxdoy, aes(x = x_LAEA, y = y_LAEA))
#  mapfract <- p + facet_wrap(~variable, ncol = 2)
#  mapfract <- mapfract + geom_tile(aes(fill = catdate))
#  mapfract = mapfract +coord_fixed(xlim = c(ext_grd@xmin,ext_grd@xmax), ylim = c(ext_grd@ymin,ext_grd@ymax))
#  mapfract = mapfract + theme_bw() + labs(title = "SOS DOYS", x = "Longitude", y = "Latitude")+ theme(plot.title = element_text(size = 14, vjust = 1))+
#    labs(x = "Longitude") + theme(axis.text.x  = element_text(size = 8) ,axis.text.y  = element_text(size = 8))+ theme(legend.position="right")
#  mapfract = mapfract + theme(plot.margin = unit(c(1,1,1,1), "cm"))
#  mapfract <- mapfract + scale_fill_brewer('DOYs', palette="RdYlGn")
#  mapfract2 <- mapfract + geom_polygon(data = mapit_df,aes(x = long, y = lat, group = group),  fill = 'transparent', colour = "black")
#  #   print(mapfract)
#
#  data_maxdoy = droplevels(subset(data, variable == 'max_doys' & year == '2014'))
#  data_maxdoy$catdate <- cut(data_maxdoy$mean, breaks = c(160,170,180,190,200,210,220,230,240,250))
#  p <- ggplot(data = data_maxdoy, aes(x = x_LAEA, y = y_LAEA))
#  mapfract <- p + facet_wrap(~variable, ncol = 2)
#  mapfract <- mapfract + geom_tile(aes(fill = catdate))
#  mapfract = mapfract +coord_fixed(xlim = c(ext_grd@xmin,ext_grd@xmax), ylim = c(ext_grd@ymin,ext_grd@ymax))
#  mapfract = mapfract + theme_bw() + labs(title = "Maximum DOYS", x = "Longitude", y = "Latitude")+ theme(plot.title = element_text(size = 14, vjust = 1))+
#    labs(x = "Longitude") + theme(axis.text.x  = element_text(size = 8) ,axis.text.y  = element_text(size = 8))+ theme(legend.position="right")
#  mapfract = mapfract + theme(plot.margin = unit(c(1,1,1,1), "cm"))
#  mapfract <- mapfract + scale_fill_brewer('DOYs', palette="RdYlGn")
#  mapfract3 <- mapfract + geom_polygon(data = mapit_df,aes(x = long, y = lat, group = group),  fill = 'transparent', colour = "black")
#  #   print(mapfract)
#
#
#  print(grid.arrange(mapfract1,mapfract2,mapfract3))
#
#
<<<<<<< HEAD
		##     sd.count <- sd(x$count)
		##     cv <- sd.count/mean.count
		##     data.frame(cv.count = cv)
		##     } )

	}}
=======
##     sd.count <- sd(x$count)
##     cv <- sd.count/mean.count
##     data.frame(cv.count = cv)
##     } )


#  <- for (thresh_ind in 1:length(levels(data_in$thresh))[1]) {    # Cycle on runs with different thresholds

    out_pdf = file.path(out_folder, 'grid_plots.pdf')
    savepdf(out_pdf)    # initialize plotting device

#    data = droplevels(subset(data_in, thresh == levels(data_in$thresh)[thresh_ind]))

		if (country == 'IT') {ncols = 2} else {ncols = 4}
    # Build the plot for the rice fraction : Add points to the map, set colors and variables and set limits
    data_mindoy = droplevels(subset(data_in, variable == 'MinDoys'))
    p <- ggplot(data = data_mindoy, aes(x = x_LAEA, y = y_LAEA))
    mapfract <- p + facet_wrap(~year, ncol = ncols)
    mapfract <- mapfract + geom_tile(aes(fill = percol))
    mapfract = mapfract +coord_fixed(xlim = c(ext_grd@xmin,ext_grd@xmax), ylim = c(ext_grd@ymin,ext_grd@ymax))
    mapfract = mapfract + theme_bw() + labs(title = "Rice Cover Fraction", x = "Longitude", y = "Latitude")+ theme(plot.title = element_text(size = 14, vjust = 1))+
      labs(x = "Longitude") + theme(axis.text.x  = element_text(size = 8) ,axis.text.y  = element_text(size = 8))+ theme(legend.position="right")
    mapfract = mapfract + theme(plot.margin = unit(c(1,1,1,1), "cm"))
    mapfract <- mapfract + scale_fill_brewer('Rice cover Fraction', palette="RdYlGn")
    mapfract <- mapfract + geom_polygon(data = mapit_df,aes(x = long, y = lat, group = group),  fill = 'transparent', colour = "black")
    print(mapfract)

    # Build the plot for the mindoy maps : Add points to the map, set colors and variables and set limits
    mapmin <- p + facet_wrap(~year, ncol = ncols)
    mapmin <- mapmin + geom_tile(aes(fill = mean))
    mapmin <- mapmin + scale_fill_gradientn('Doy of Sowing',colours = topo.colors(10), limits=c(75, 175), oob=squish)
    mapmin = mapmin +coord_fixed(xlim = c(ext_grd@xmin,ext_grd@xmax), ylim = c(ext_grd@ymin,ext_grd@ymax))
    mapmin = mapmin + theme_bw() + labs(title = "Doys of Sowing", x = "Longitude", y = "Latitude")+ theme(plot.title = element_text(size = 14, vjust = 1))+
      labs(x = "Longitude") + theme(axis.text.x  = element_text(size = 8) ,axis.text.y  = element_text(size = 8))+ theme(legend.position="right")
    mapmin = mapmin + theme(plot.margin = unit(c(1,1,1,1), "cm"))
    mapmin <- mapmin + geom_polygon(data = mapit_df,aes(x = long, y = lat, group = group),  fill = 'transparent', colour = "black")

    print(mapmin)



		# Build the plot for the sos doy maps : Add points to the map, set colors and variables and set limits

		data_sos = droplevels(subset(data_in, variable == 'SosDoys'))
		p <- ggplot(data = data_sos, aes(x = x_LAEA, y = y_LAEA))
		mapsos <- p + facet_wrap(~year, ncol = ncols)
		mapsos <- mapsos + geom_tile(aes(fill = mean))
		mapsos <- mapsos + scale_fill_gradientn('DOY of Emergence',colours = topo.colors(10), limits=c(105, 205), oob=squish)
		mapsos = mapsos +coord_fixed(xlim = c(ext_grd@xmin,ext_grd@xmax), ylim = c(ext_grd@ymin,ext_grd@ymax))
		mapsos = mapsos + theme_bw() + labs(title = "DOY of Emergence", x = "Longitude", y = "Latitude")+ theme(plot.title = element_text(size = 14, vjust = 1))+
				labs(x = "Longitude") + theme(axis.text.x  = element_text(size = 8) ,axis.text.y  = element_text(size = 8))+ theme(legend.position="right")
		mapsos = mapsos + theme(plot.margin = unit(c(1,1,1,1), "cm"))
		mapsos <- mapsos + geom_polygon(data = mapit_df,aes(x = long, y = lat, group = group),  fill = 'transparent', colour = "black")

		print(mapsos)

		# Build the plot for the maxdoy maps : Add points to the map, set colors and variables and set limits

		data_maxdoy = droplevels(subset(data_in, variable == 'MaxDoys'))
		p <- ggplot(data = data_maxdoy, aes(x = x_LAEA, y = y_LAEA))
		mapmax <- p + facet_wrap(~year, ncol = ncols)
		mapmax <- mapmax + geom_tile(aes(fill = mean))
		mapmax <- mapmax + scale_fill_gradientn('Doy of Heading',colours = topo.colors(10), limits=c(185, 285), oob=squish)
		mapmax = mapmax +coord_fixed(xlim = c(ext_grd@xmin,ext_grd@xmax), ylim = c(ext_grd@ymin,ext_grd@ymax))
		mapmax = mapmax + theme_bw() + labs(title = "Doys of Heading", x = "Longitude", y = "Latitude")+ theme(plot.title = element_text(size = 14, vjust = 1))+
				labs(x = "Longitude") + theme(axis.text.x  = element_text(size = 8) ,axis.text.y  = element_text(size = 8))+ theme(legend.position="right")
		mapmax = mapmax + theme(plot.margin = unit(c(1,1,1,1), "cm"))
		mapmax <- mapmax + geom_polygon(data = mapit_df,aes(x = long, y = lat, group = group),  fill = 'transparent', colour = "black")

		print(mapmax)


    # Build the plot for the maxvi maps : Add points to the map, set colors and variables and set limits

    data_maxvi = droplevels(subset(data_in, variable == 'MaxVis'))
    p <- ggplot(data = data_maxvi, aes(x = x_LAEA, y = y_LAEA))
    mapvimax <- p + facet_wrap(~year, ncol = ncols)
    mapvimax <- mapvimax + geom_tile(aes(fill = mean))
    mapvimax <- mapvimax + scale_fill_gradientn('Value of VI at Heading',colours = topo.colors(10), limits=c(5000, 8000), oob=squish)
    mapvimax = mapvimax +coord_fixed(xlim = c(ext_grd@xmin,ext_grd@xmax), ylim = c(ext_grd@ymin,ext_grd@ymax))
    mapvimax = mapvimax + theme_bw() + labs(title = "Values of VI at Heading", x = "Longitude", y = "Latitude")+ theme(plot.title = element_text(size = 14, vjust = 1))+
      labs(x = "Longitude") + theme(axis.text.x  = element_text(size = 8) ,axis.text.y  = element_text(size = 8))+ theme(legend.position="right")
    mapvimax = mapvimax + theme(plot.margin = unit(c(1,1,1,1), "cm"))
    mapvimax <- mapvimax + geom_polygon(data = mapit_df,aes(x = long, y = lat, group = group),  fill = 'transparent', colour = "black")

    print(mapvimax)

#browser()

    # Build the boxplot of interannual variability of the four variables along years

		levels(data_in$variable) = c( 'DOY of Sowing', 'DOY of Heading', 'Value of VI at Heading', 'DOY of Emergence')
		data_in$variable = factor(data_in$variable, levels = c( 'DOY of Sowing','DOY of Emergence', 'DOY of Heading', 'Value of VI at Heading' ))
		blank_data <- data.frame(variable = c("DOY of Sowing", "DOY of Sowing","DOY of Emergence","DOY of Emergence", "DOY of Heading", "DOY of Heading",
						"Value of VI at Heading", "Value of VI at Heading"		),  y = c(75,175,105,205,185,285,5000,8000), x = rep(levels(data_in$year),2))

		data_in$mean[which(data_in$variable == "DOY of Sowing" &  data_in$mean > 175)] = NA
		data_in$mean[which(data_in$variable == "DOY of Sowing"&  data_in$mean < 75)] = NA
		data_in$mean[which(data_in$variable == "DOY of Emergence"&  data_in$mean > 205)] = NA
		data_in$mean[which(data_in$variable == "DOY of Emergence"&  data_in$mean < 105)] = NA
		data_in$mean[which(data_in$variable == "DOY of Heading"&  data_in$mean > 285)] = NA
		data_in$mean[which(data_in$variable == "DOY of Heading"&  data_in$mean < 185)] = NA
#		data_in$mean[which(data_in$variable == "Value of VI at Heading") > 175] = NA
#		data_in$mean[which(data_in$variable == "Value of VI at Heading") < 75] = NA
#
    boxp1 = ggplot (data_in, aes (x = year, y = mean))
		boxp1 = boxp1 + geom_blank(data = blank_data, aes(x = x, y = y))+ facet_wrap(~variable)
    boxp1 = boxp1 + geom_boxplot(outlier.colour = 'transparent') + facet_wrap(~variable, scales = "free_y") + theme_bw()

    boxp1 = boxp1 + theme_bw() + labs(title = "Interannual Variability of Retrieved Variables", x = "Year", y = "Values")+
           theme(plot.title = element_text(size = 14, vjust = 1))+
           theme(axis.text.x  = element_text(size = 8) ,axis.text.y  = element_text(size = 8))+ theme(legend.position="right")


    # Build the boxplot of interannual variability of the four variables along years - above 10% of cover

    boxp2 = ggplot (data = subset(data_in, perc_rice > 0.1), aes (x = year, y = mean))
    boxp2 = boxp2 + geom_boxplot(outlier.colour = 'transparent') + facet_wrap(~variable, scales = "free_y") + theme_bw()
    boxp2 = boxp2 + theme_bw() + labs(title = "Interannual Variability of Retrieved Variables - cells above 10 % cover", x = "Year", y = "Values")+
          theme(plot.title = element_text(size = 14, vjust = 1))+
           theme(axis.text.x  = element_text(size = 8) ,axis.text.y  = element_text(size = 8))+ theme(legend.position="right")
		boxp2 = boxp2 + geom_blank(data = blank_data, aes(x = x, y = y))
    print(grid.arrange( boxp1, boxp2, ncol=1))

    # Build the barplot of interannual variability of the detected rice area along years
    data_sub =  subset(data_in, variable == 'DOY of Sowing')
    data_sub_area = ddply(data_sub, .(year), summarize, tot_area = sum(rice_area)/10000)
    barplot1 = ggplot(data_sub_area, aes(x = year, y = tot_area))
    barplot1 = barplot1 + geom_bar(stat = 'identity')
    barplot1 = barplot1 + theme_bw() + labs(title = "Interannual Variability of total estimated rice area", x = "Year", y = "Area (ha)")+
      theme(plot.title = element_text(size = 14, vjust = 1))+ theme(axis.text.x  = element_text(size = 8) ,axis.text.y  = element_text(size = 8))


    # Build the barplot of interannual variability of the detected rice area - above 10% of cover
    data_sub =  subset(data_in, variable == 'DOY of Sowing' & perc_rice > 0.1)
    data_sub_area = ddply(data_sub, .(year), summarize, tot_area = sum(rice_area)/10000)
    barplot2 = ggplot(data_sub_area, aes(x = year, y = tot_area))
    barplot2 = barplot2 + geom_bar(stat = 'identity')
    barplot2 = barplot2 + theme_bw() + labs(title = "Interannual Variability of total estimated rice area - cells above 10 % cover", x = "Year", y = "Area (ha)")+
      theme(plot.title = element_text(size = 14, vjust = 1))+ theme(axis.text.x  = element_text(size = 8) ,axis.text.y  = element_text(size = 8))
    print(grid.arrange( barplot1, barplot2, ncol=1))

    dev.off()
}    #End Cycle on countries


#  } #End Cycle on thresholds


# build the plots for the time series of boxplots (for doys and maxs and mins per

>>>>>>> 2b5a5dafc83c379ed8e956053b71225469462c0a
