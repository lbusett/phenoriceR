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
library(hydroGOF)
library(gridExtra)
library(scales)

#---- location of data derived form "LB_Phenorice_Aggregate
main_folder = 'D:/Temp/PhenoRice/Processing/IT/Outputs/2013/' 															# Folder used to store results of " Phenorice_PostProcess_Aggregate
main_folder = 'D:/Temp/PhenoRice/Processing/PHL/Outputs/2013_new/'
main_folder = 'D:/Temp/PhenoRice/Processing/IND/Outputs/2014/'

results_folder = file.path(main_folder,'Validate')			# Where to put results
country_code = 'IND'
sel_quart = c()
out_RData_file = file.path(results_folder, 'Plots_Dates.RData')

#---- Initialization

memory.limit(14000)
in_proj = '+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs'

in_modgrid_df = get(load(file.path(results_folder,'out_df_modgrid_.RData')))
in_grid_dfs = list.files(results_folder,'.RData', full.names = T)
in_grid_dfs = in_grid_dfs[c(5,3,4,1,2)]

grid_folder = file.path(results_folder,'GRIDs')   #save the grid here
grid_resolutions = c(231.656358,2084.907, 5096.432, 10192.864,20385.73 )
grid_shapes <- c('Grid_MODIS_250','Grid_2Km','Grid_5Km','Grid_10Km','Grid_20Km')		 #this is the grid name
mod_pix_area = 231.656358*231.656358

gadm_shp_url <- paste0('http://biogeo.ucdavis.edu/data/gadm2/shp/',country_code,'_adm.zip')
gadm_shape_dir <- file.path(results_folder,'gadm',basename(file_path_sans_ext(gadm_shp_url)))
gadm_level = 2
gadm_shape_name = paste0(country_code,'_adm',gadm_level,'_sinu.shp')
in_shape_admin_reproj <- readOGR(gadm_shape_dir,file_path_sans_ext(gadm_shape_name))
map_df = fortify(in_shape_admin_reproj, region = "NAME_1")

# in_modgrid_df = droplevels(subset(in_modgrid_df, ncells > 0 & is.na(ref_ricearea) == FALSE))			# Remove "cells" with no "good data" pixels

#---- Start creating the plots showing statistical disrtribution of dates

data = in_modgrid_df

# Compute average doys of minum/maximum (useful for shapefile joining)

avg_doys_provinces = ddply (data, .(NAME_1), summarize, avg_q1 = mean(min_q1, na.rm = T)
		, avg_q2 = mean(min_q2, na.rm = T)
		, avg_q3 = mean(min_q3, na.rm = T)
		, avg_q4 = mean(min_q4, na.rm = T)
		, avg_max_q1 = mean(max_q1, na.rm = T)
		, avg_max_q2 = mean(max_q2, na.rm = T)
		, avg_max_q3 = mean(max_q3, na.rm = T)
		, avg_max_q4 = mean(max_q4, na.rm = T)
		, avg_length_q1 = mean(max_q1, na.rm = T)
		, avg_length_q2 = mean(max_q2, na.rm = T)
		, avg_length_q3 = mean(max_q3, na.rm = T)
		, avg_length_q4 = mean(max_q4, na.rm = T))



is.na(avg_doys_provinces) <- do.call(cbind,lapply(avg_doys_provinces, is.nan))
is.na(data) <- do.call(cbind,lapply(data, is.nan))

	#- ------------------------------------------------------------------------------- -#
	# print boxplots of minimums -----
	#- ------------------------------------------------------------------------------- -#

pro = melt(data,value.name = "DOY", measure.vars = c('min_q1','min_q2','min_q3','min_q4'), na.rm = T )
names(pro)[length(names(pro))-1] = 'Quarter'
names(pro)[length(names(pro))] = 'DOY'
pro$Date = as.Date(pro$DOY - 1, origin = "2013-01-01")

give.n <- function(x){   # Accessory function to get N? of samples in each group
	return(c(y =-100, label = length(x)))
	}

p = ggplot(pro, aes(x = Quarter, y = DOY))+theme_bw()
p = p + geom_boxplot(outlier.colour = 'transparent')+geom_jitter(position = position_jitter(w = 0.1), size = 0.1, alpha = 0.05)
p = p +theme(plot.title = element_text(vjust = 1, hjust = 0))+
		stat_summary(fun.data = give.n, geom = "text", fun.y = 0, colour = 'red')
p1_mins = p + ggtitle('Distribution of retrieved Minimum DOYs in the 4 quarters')

p = ggplot(pro, aes(Date ,fill = Quarter, color = Quarter ))+theme_bw()
p = p + geom_histogram(aes(y=..count../sum(..count..)),alpha=.5, position = 'identity', breaks =as.numeric(seq(min(pro$Date),max(pro$Date),'8 day')))
p = p +geom_density(aes(y = 8*..count../sum(..count..)), breaks =as.numeric(seq(min(pro$Date),max(pro$Date),'8 day')), position = 'identity', fill = 'transparent', adjust = 2)
p = p +theme(plot.title = element_text(vjust = 1, hjust = 0))
p2_mins = p + ggtitle('Distribution of retrieved Minimum DOYs in the 4 quarters')+ylab('Frequency')+
		scale_x_date(breaks = date_breaks('1 month'), labels = date_format("%b-%d"))+
		theme(axis.text.x = element_text(angle = 45, hjust  = 1 , vjust = 1))

p = ggplot(pro, aes(Date ))+theme_bw()
p = p + geom_histogram(aes(y=..count../sum(..count..)),alpha=.5, position = 'identity', breaks =as.numeric(seq(min(pro$Date),max(pro$Date),'8 day')), color = 'black')
p = p +geom_density(aes(y = 8*..count../sum(..count..)), breaks =as.numeric(seq(min(pro$Date),max(pro$Date),'8 day')), position = 'identity', fill = 'transparent', adjust = 2)
p = p +theme(plot.title = element_text(vjust = 1, hjust = 0))
p3_mins = p + ggtitle('Distribution of retrieved Minimum DOYs in the Year')+ylab('Frequency [%]')+
		scale_x_date(breaks = date_breaks('1 month'), labels = date_format("%b-%d"))+
		theme(axis.text.x = element_text(angle = 45, hjust  = 1 , vjust = 1))

#By Province

p = ggplot(pro, aes(x = Quarter, y = DOY))+theme_bw()+facet_wrap(~NAME_1)
p = p + geom_boxplot(outlier.colour = 'transparent')+geom_jitter(position = position_jitter(w = 0.1), size = 0.1, alpha = 0.05)
p = p +theme(plot.title = element_text(vjust = 1, hjust = 0))+ facet_wrap(~NAME_1)
		stat_summary(fun.data = give.n, geom = "text", fun.y = 0, colour = 'red')
p1_mins_pro = p+ggtitle('Distribution of retrieved Minimum DOYs in the 4 quarters')

p = ggplot(pro, aes(Date ,fill = Quarter, color = Quarter ))+theme_bw()+facet_wrap(~NAME_1, scales = 'free_y')
p = p + geom_histogram(aes(y=..count../sum(..count..)),alpha=.5, position = 'identity', breaks =as.numeric(seq(min(pro$Date),max(pro$Date),'8 day')))
p = p +geom_density(aes(y = 8*..count../sum(..count..)), breaks =as.numeric(seq(min(pro$Date),max(pro$Date),'8 day')), position = 'identity', fill = 'transparent', adjust = 2)
p = p +theme(plot.title = element_text(vjust = 1, hjust = 0))
p2_mins_pro = p + ggtitle('Distribution of retrieved Minimum DOYs in the 4 quarters')+ylab('Frequency')+
		scale_x_date(breaks = date_breaks('1 month'), labels = date_format("%b-%d"))+
		theme(axis.text.x = element_text(angle = 45, hjust  = 1 , vjust = 1))

p = ggplot(pro, aes(Date ))+theme_bw()+facet_wrap(~NAME_1, scales = 'free_y')
p = p + geom_histogram(aes(y=..count../sum(..count..)),alpha=.5, position = 'identity', breaks =as.numeric(seq(min(pro$Date),max(pro$Date),'8 day')), color = 'black')
p = p +geom_density(aes(y = 8*..count../sum(..count..)), breaks =as.numeric(seq(min(pro$Date),max(pro$Date),'8 day')), position = 'identity', fill = 'transparent', adjust = 0.5)
p = p +theme(plot.title = element_text(vjust = 1, hjust = 0))
p3_mins_pro = p + ggtitle('Distribution of retrieved Minimum DOYs in the Year')+ylab('Frequency [%]')+
		scale_x_date(breaks = date_breaks('1 month'), labels = date_format("%b-%d"))+
		theme(axis.text.x = element_text(angle = 45, hjust  = 1 , vjust = 1))

#- ------------------------------------------------------------------------------- -#
# Print boxplots of maximums -----
#- ------------------------------------------------------------------------------- -#


pro = melt(data,value.name = "DOY", measure.vars = c('max_q1','max_q2','max_q3','max_q4'), na.rm = T )

names(pro)[length(names(pro))-1] = 'Quarter'
names(pro)[length(names(pro))] = 'DOY'
pro$Date = as.Date(pro$DOY - 1, origin = "2013-01-01")

p = ggplot(pro, aes(x = Quarter, y = DOY))+theme_bw()
p = p + geom_boxplot(outlier.colour = 'transparent')+geom_jitter(position = position_jitter(w = 0.1), size = 0.1, alpha = 0.05)
p = p +theme(plot.title = element_text(vjust = 1, hjust = 0))+
		stat_summary(fun.data = give.n, geom = "text", fun.y = 0, colour = 'red')
p1_maxs = p + ggtitle('Distribution of retrieved Maximum DOYs in the 4 quarters')

p = ggplot(pro, aes(Date ,fill = Quarter, color = Quarter ))+theme_bw()
p = p + geom_histogram(aes(y=..count../sum(..count..)),alpha=.5, position = 'identity', breaks =as.numeric(seq(min(pro$Date),max(pro$Date),'8 day')))
p = p +geom_density(aes(y = 8*..count../sum(..count..)), breaks =as.numeric(seq(min(pro$Date),max(pro$Date),'8 day')), position = 'identity', fill = 'transparent', adjust = 2)
p = p +theme(plot.title = element_text(vjust = 1, hjust = 0))
p2_maxs = p + ggtitle('Distribution of retrieved Maximum DOYs in the 4 quarters')+ylab('Frequency')+
		scale_x_date(breaks = date_breaks('1 month'), labels = date_format("%b-%d"))+
		theme(axis.text.x = element_text(angle = 45, hjust  = 1 , vjust = 1))

p = ggplot(pro, aes(Date ))+theme_bw()
p = p + geom_histogram(aes(y=..count../sum(..count..)),alpha=.5, position = 'identity', breaks =as.numeric(seq(min(pro$Date),max(pro$Date),'8 day')), color = 'black')
p = p +geom_density(aes(y = 8*..count../sum(..count..)), breaks =as.numeric(seq(min(pro$Date),max(pro$Date),'8 day')), position = 'identity', fill = 'transparent', adjust = 0.5)
p = p +theme(plot.title = element_text(vjust = 1, hjust = 0))
p3_maxs = p + ggtitle('Distribution of retrieved Maximum DOYs in the Year')+ylab('Frequency [%]')+
		scale_x_date(breaks = date_breaks('1 month'), labels = date_format("%b-%d"))+
		theme(axis.text.x = element_text(angle = 45, hjust  = 1 , vjust = 1))


#By Province

p = ggplot(pro, aes(x = Quarter, y = DOY))+theme_bw()+facet_wrap(~NAME_1)
p = p + geom_boxplot(outlier.colour = 'transparent')+geom_jitter(position = position_jitter(w = 0.1), size = 0.1, alpha = 0.05)
p = p +theme(plot.title = element_text(vjust = 1, hjust = 0))+ facet_wrap(~NAME_1)
stat_summary(fun.data = give.n, geom = "text", fun.y = 0, colour = 'red')
p1_maxs_pro = p+ggtitle('Distribution of retrieved Maximum DOYs in the 4 quarters')

p = ggplot(pro, aes(Date ,fill = Quarter, color = Quarter ))+theme_bw()+facet_wrap(~NAME_1, scales = 'free_y')
p = p + geom_histogram(aes(y=..count../sum(..count..)),alpha=.5, position = 'identity', breaks =as.numeric(seq(min(pro$Date),max(pro$Date),'8 day')))
p = p +geom_density(aes(y = 8*..count../sum(..count..)), breaks =as.numeric(seq(min(pro$Date),max(pro$Date),'8 day')), position = 'identity', fill = 'transparent', adjust = 2)
p = p +theme(plot.title = element_text(vjust = 1, hjust = 0))
p2_maxs_pro = p + ggtitle('Distribution of retrieved Maximum DOYs in the 4 quarters')+ylab('Frequency')+
		scale_x_date(breaks = date_breaks('1 month'), labels = date_format("%b-%d"))+
		theme(axis.text.x = element_text(angle = 45, hjust  = 1 , vjust = 1))

p = ggplot(pro, aes(Date ))+theme_bw()+facet_wrap(~NAME_1, scales = 'free_y')
p = p + geom_histogram(aes(y=..count../sum(..count..)),alpha=.5, position = 'identity', breaks =as.numeric(seq(min(pro$Date),max(pro$Date),'8 day')), color = 'black')
p = p +geom_density(aes(y = 8*..count../sum(..count..)), breaks =as.numeric(seq(min(pro$Date),max(pro$Date),'8 day')), position = 'identity', fill = 'transparent', adjust = 0.5)
p = p +theme(plot.title = element_text(vjust = 1, hjust = 0))
p3_maxs_pro = p + ggtitle('Distribution of retrieved Maximum DOYs in the Year')+ylab('Frequency [%]')+
		scale_x_date(breaks = date_breaks('1 month'), labels = date_format("%b-%d"))+
		theme(axis.text.x = element_text(angle = 45, hjust  = 1 , vjust = 1))


#- ------------------------------------------------------------------------------- -#
# print boxplots of lengths of season -----
#- ------------------------------------------------------------------------------- -#


pro = melt(data,value.name = "DOY", measure.vars = c('length_q1','length_q2','length_q3','length_q4'), na.rm = T )

names(pro)[length(names(pro))-1] = 'Quarter'
names(pro)[length(names(pro))] = 'DOY'
pro$Date = as.Date(pro$DOY - 1, origin = "2013-01-01")

p = ggplot(pro, aes(x = Quarter, y = DOY))+theme_bw()
p = p + geom_boxplot(outlier.colour = 'transparent')+geom_jitter(position = position_jitter(w = 0.1), size = 0.1, alpha = 0.05)
p = p +theme(plot.title = element_text(vjust = 1, hjust = 0))+
		stat_summary(fun.data = give.n, geom = "text", fun.y = 0, colour = 'red')
p1_lgt = p + ggtitle('Distribution of retrieved Season Length in the 4 quarters')

p = ggplot(pro, aes(DOY ,fill = Quarter, color = Quarter ))+theme_bw()
p = p + geom_histogram(aes(y=..count../sum(..count..)),alpha=.5, position = 'identity', breaks =as.numeric(seq(min(pro$DOY),max(pro$DOY),8)))
p = p +theme(plot.title = element_text(vjust = 1, hjust = 0))
p2_lgt = p + ggtitle('Distribution of retrieved Season Length in the 4 quarters')+
		ylab('Frequency')+
		xlab('Number of days between min and max')

p = ggplot(pro, aes(DOY ))+theme_bw()
p = p + geom_histogram(aes(y=..count../sum(..count..)),alpha=.5, position = 'identity', breaks =as.numeric(seq(min(pro$DOY),max(pro$DOY),8)), color = 'black')
p = p +theme(plot.title = element_text(vjust = 1, hjust = 0))
p3_lgt = p + ggtitle('Distribution of retrieved Season Length in the year')+
		ylab('Frequency')+
		xlab('Number of days between min and max')


#By Province

p = ggplot(pro, aes(x = Quarter, y = DOY))+theme_bw()+facet_wrap(~NAME_1)
p = p + geom_boxplot(outlier.colour = 'transparent')+geom_jitter(position = position_jitter(w = 0.1), size = 0.1, alpha = 0.05)
p = p +theme(plot.title = element_text(vjust = 1, hjust = 0))+
		stat_summary(fun.data = give.n, geom = "text", fun.y = 0, colour = 'red')
p1_lgt_pro = p + ggtitle('Distribution of retrieved Season Length in the 4 quarters')

p = ggplot(pro, aes(DOY ,fill = Quarter, color = Quarter ))+theme_bw()+facet_wrap(~NAME_1, scales = 'free_y')
p = p + geom_histogram(aes(y=..count../sum(..count..)),alpha=.5, position = 'identity', breaks =as.numeric(seq(min(pro$DOY),max(pro$DOY),8)))
p = p +theme(plot.title = element_text(vjust = 1, hjust = 0))
p2_lgt_pro = p + ggtitle('Distribution of retrieved Season Length in the 4 quarters')+
		ylab('Frequency')+
		xlab('Number of days between min and max')

p = ggplot(pro, aes(DOY ))+theme_bw()+facet_wrap(~NAME_1, scales = 'free_y')
p = p + geom_histogram(aes(y=..count../sum(..count..)),alpha=.5, position = 'identity', breaks =as.numeric(seq(min(pro$DOY),max(pro$DOY),8)), color = 'black')
p = p +theme(plot.title = element_text(vjust = 1, hjust = 0))
p3_lgt_pro = p + ggtitle('Distribution of retrieved Season Length in the Year')+
		ylab('Frequency')+
		xlab('Number of days between min and max')


## Section of code used to plot maps
#
# centroids = getSpPPolygonsLabptSlots(readOGR(grid_folder,grid_shapes[1]))	# get coordinates of centroids of each 250m cell
# centroids = SpatialPoints(centroids,proj4string =  CRS(in_proj)) # convert to spatialpoints
# centroids_coords = NULL
# centroids_coords$x = centroids@coords[,1]
# centroids_coords$y = centroids@coords[,2]
# centroids_coords$id = seq(1:length(centroids_coords$y))
# centroids_coords = data.frame(x = centroids_coords$x,y = centroids_coords$y,id = centroids_coords$id)
# data_plot = join(centroids_coords, data, by = 'id',type = 'left')
# ext_grd = extent(centroids)
#
#

#
# legends = c("Doy of Min", "Doy of Min", "Doy of Min", "Doy of Min", "Doy of Max", "Doy of Max", "Doy of Max",  "Doy of Max", "Min-Max length",    "Min-Max length",    "Min-Max length",    "Min-Max length" )
# for (var in names(data_plot)[7:18]) {
#
# #	for (cycle in 1:4) {
# ##	data_sub = melt(data_plot, id.vars = c(1,2,3),measure_vars = c(7:10))
# #	data_sub = data_sub[,c(1,2,3,(8+(cycle-1)*4:8+(cycle)*4))]
# 	data_pp = subset(data_plot,ncells > 30  )
#
#	if (length(which(is.finite(data_pp[,var]) == T))){
# #   data_pp = subset(data_pp, is.na(min_q3) == FALSE)
# 	p <- ggplot(data = data_pp, aes(x = x, y = y))
# # 	p <- ggplot(aes_string( fill = var))
#
# 	mapfract <- p + geom_tile(aes_string( fill = 'min_q1'))#+facet_wrap(~variable, scales = 'free_y')
#  	mapfract = mapfract +coord_fixed(xlim = c(ext_grd@xmin,ext_grd@xmax), ylim = c(ext_grd@ymin,ext_grd@ymax))
# 	mapfract = mapfract + theme_bw() + labs(title = paste("Map of ", var ,sep = ''), x = "Longitude", y = "Latitude")+ theme(plot.title = element_text(size = 14, vjust = 1))+
# 			labs(x = "Longitude") + theme(axis.text.x  = element_text(size = 8) ,axis.text.y  = element_text(size = 8))+ theme(legend.position="right")
# #mapfract = mapfract + theme(plot.margin = unit(c(1,1,1,1), "cm"))
# 	mapfract <- mapfract + scale_fill_gradientn(legends[var],colours = topo.colors(10), na.value = "transparent" )
# 	mapfract <- mapfract + geom_polygon(data = map_df,aes(x = long, y = lat, group = group),  fill = 'transparent', colour = "black")
# 	print(mapfract)
#}
# } #End Cycle on var

	#- ------------------------------------------------------------------------------- -#
	#  Perform analysis on the different grids
	#- ------------------------------------------------------------------------------- -#

mapfracts = list()
plot_ind = 0
for (grid in 2:length(in_grid_dfs)) {

	in_grid_data = get(load(in_grid_dfs[grid]))
#	grid_res =
	is.na(in_grid_data) <- do.call(cbind,lapply(in_grid_data, is.infinite))
	is.na(in_grid_data) <- do.call(cbind,lapply(in_grid_data, is.nan))

		grid_dim = strsplit(strsplit(basename(in_grid_dfs[grid]),'_')[[1]][4],'.RData')[[1]]


		# Tabulate areas to compute cumulated/averaged values over the cells of the lower resolution grids
		aggregate_values = in_grid_data[, list(
						ref_rice_Area = sum(ref_ricearea, na.rm = T)/10000,			# cumulate area classified as rice
						tot_area = sum(tot_cellarea, na.rm = T)/10000,					# total non-nodata area
						mod_ricearea_q1 = sum(tot_cellarea*(is.finite(min_q1)), na.rm = T)/10000,	# area detected as rice in each of the 4 quarters
						mod_ricearea_q2 = sum(tot_cellarea*(min_q2 >0), na.rm = T)/10000,
						mod_ricearea_q3 = sum(tot_cellarea*(min_q3 >0), na.rm = T)/10000,
						mod_ricearea_q4 = sum(tot_cellarea*(min_q4 >0), na.rm = T)/10000,
						mod_ricearea_tot = sum(tot_cellarea*(n_seasons >0), na.rm = T)/10000, # total area detected as rice in the 4 quarters
						mean_min_q1 = mean(min_q1, na.rm = T),	# average doy of min in each of the 4 quarters
						mean_min_q2 = mean(min_q2, na.rm = T),
						mean_min_q3 = mean(min_q3, na.rm = T),
						mean_min_q4 = mean(min_q4, na.rm = T),
						mean_max_q1 = mean(max_q1, na.rm = T),	# average doy of max in each of the 4 quarters
						mean_max_q2 = mean(max_q2, na.rm = T),
						mean_max_q3 = mean(max_q3, na.rm = T),
						mean_max_q = mean(max_q4, na.rm = T),
						mean_length_q1 = mean(length_q1, na.rm = T),	# average max-min length in each of the 4 quarters
						mean_length_q2 = mean(length_q2, na.rm = T),
						mean_length_q3 = mean(length_q3, na.rm = T),
						mean_length_q4 = mean(length_q4, na.rm = T),
						stdev_min_q1 = sd(min_q1, na.rm = T),			# stdev of doys of min in each of the 4 quarters
						stdev_min_q2 = sd(min_q2, na.rm = T),
						stdev_min_q3 = sd(min_q3, na.rm = T),
						stdev_min_q4 = sd(min_q4, na.rm = T),
						stdev_max_q1 = sd(max_q1, na.rm = T),			# stdev of doys of min in each of the 4 quarters
						stdev_max_q2 = sd(max_q2, na.rm = T),
						stdev_max_q3 = sd(max_q3, na.rm = T),
						stdev_max_q4 = sd(max_q4, na.rm = T),
						stdev_length_q1 = sd(length_q1, na.rm = T),# stdev of doys of min in each of the 4 quarters
						stdev_length_q2 = sd(length_q2, na.rm = T),
						stdev_length_q3 = sd(length_q3, na.rm = T),
						stdev_length_q4 = sd(length_q4, na.rm = T))

				, by = id_big]


	# Create and print maps of rice-detected areas over the lower resolution cells

	centroids = getSpPPolygonsLabptSlots(readOGR(grid_folder,grid_shapes[grid]))	# get coordinates of centroids of each 250m cell
	centroids = SpatialPoints(centroids,proj4string =  CRS(in_proj)) # convert to spatialpoints
	centroids_coords = NULL
	centroids_coords$x = centroids@coords[,1]
	centroids_coords$y = centroids@coords[,2]
	centroids_coords$id = seq(1:length(centroids_coords$y))
	centroids_coords = data.frame(x = centroids_coords$x,y = centroids_coords$y,id_big = centroids_coords$id)

	data_plot = join(centroids_coords, aggregate_values, by = 'id_big',type = 'left')
	data_plot = subset(data_plot, (ref_rice_Area > 0  | mod_ricearea_tot >0))
	ext_grd = extent(centroids)


	legends = c("Doy of Min", "Doy of Min", "Doy of Min", "Doy of Min", "Doy of Max", "Doy of Max", "Doy of Max",  "Doy of Max", "Min-Max length",    "Min-Max length",    "Min-Max length",    "Min-Max length" )
	for (var_ind in seq(along =names(data_plot)[11:22])) {
		plot_ind = plot_ind +1
		var = names(data_plot)[11:22][var_ind]
		ylim = quantile((eval(parse(text = paste0('data_plot$',var)))), c(0.10, 0.90), na.rm = T)


		p <- ggplot(data = data_plot, aes(x = x, y = y))
		mapfract <- p + geom_tile(aes_string(fill = var))
		mapfract = mapfract +coord_fixed(xlim = c(ext_grd@xmin,ext_grd@xmax), ylim = c(ext_grd@ymin,ext_grd@ymax))
		mapfract = mapfract + theme_bw() + labs(title = paste("Map of ", var ,sep = ''), x = "Longitude", y = "Latitude")+ theme(plot.title = element_text(size = 14, vjust = 1))+
				labs(x = "Longitude") + theme(axis.text.x  = element_text(size = 8) ,axis.text.y  = element_text(size = 8))+ theme(legend.position="right")
#mapfract = mapfract + theme(plot.margin = unit(c(1,1,1,1), "cm"))
		mapfract <- mapfract + geom_polygon(data = map_df,aes(x = long, y = lat, group = group),  fill = 'transparent', colour = "black")
		mapfract <- mapfract + scale_fill_gradientn(var,colours = topo.colors(10), na.value = "transparent", limits = ylim )
		attributes(mapfract)$variable = var
		mapfracts[[plot_ind]] = mapfract
#		names(mapfracts[[plot_ind]]) = var

	} #End Cycle on var



	# Save results as RData file
#	save(aggregate_values, file = out_grid_df_files[cycle])

} #End Cycle on grid

save(avg_doys_provinces, p1_mins, p2_mins,p3_mins, p1_mins_pro,p2_mins_pro,p3_mins_pro,
		p1_maxs, p2_maxs,p3_maxs, p1_maxs_pro,p2_maxs_pro,p3_maxs_pro,
		p1_lgt, p2_lgt,p3_lgt, p1_lgt_pro,p2_lgt_pro,p3_lgt_pro, mapfracts, file = out_RData_file)


dev.off()
