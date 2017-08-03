# TODO: Add comment
#
# Author: LB
###############################################################################
savepdf <- function(file, width=29.7, height=21)
{ fname <- file
	pdf(fname, width=width/2.54, height=height/2.54,
			pointsize=10)
}


lm_eqn = function(df){

	eq <- substitute(italic(y) == a + b %.% italic(x)*""~~italic('r')~"="~r,
			list(a = format(df$Intercept, digits = 3),
					b = format(df$Slope, digits = 3),
					r = format(df$r, digits = 3)))
	as.character(as.expression(eq))
	out = data.frame(eq = as.character(as.expression(eq)))
	out
}

regress_stats = function( x= x, y = y) {

	sub_y = which(y > 0 & x > 0)
	y = y[sub_y]  		; 			x = x[sub_y]

	if (length(y[is.finite(y)] )>10 & length(x[is.finite(x)] )>10) {
		lin_model = lm(y~x, na.action = na.omit )				# Compute linear model
		sum_mod = summary(lin_model)
		ME = mean (y- x , na.rm = TRUE)  															 # mean error
		MAE = mean(abs((y-x)),na.rm = TRUE) 														 # MAE
		rMAE = 100*mean(abs((y-x)/x),na.rm = TRUE)											# relative MAE on original (%)
		RMSE = sqrt(mean((y-x)^2,na.rm = TRUE))												 # RMSE on original
		rRMSE = sqrt(mean(((y-x)/x)^2,na.rm = TRUE))  										#	relative RMSE on original(%)
		EF = NSE(y,x)
		sig = cor.test(x,y)$p.value
		# Build output data frame
		out = data.frame(N = (sum_mod$df[2]+1), Intercept = sum_mod$coef[[1]], Slope = sum_mod$coef[[2]], r = sum_mod$r.squared^0.5, sig = sig ,  R2 = sum_mod$r.squared, ME = ME, MAE = MAE, rMAE = rMAE, RMSE = RMSE, rRMSE = rRMSE ,EF = EF)
		out
	} else {
		out = data.frame(N = length(y[is.finite(y)] >1), Intercept = NA, Slope = NA, r = NA, R2 = NA, ME = NA, MAE = NA, rMAE = NA, RMSE = NA, rRMSE = NA )
	}
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
library(hydroGOF)
library(gridExtra)

memory.limit(14000)
in_proj = '+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs'

#field_data = read.csv2('W:/lorenzo/phenorice/philippines/GIS/Field_data_Leyte_point_adm_modis_info.csv',header = T)

main_folder = 'D:/Temp/Phenorice_temp/Lomb_Analysis/PostProc_Outputs' 															# Folder used to store results of " Phenorice_PostProcess_Aggregate

main_folder = 'Z:/Processing_v21/old_min_identification/Outputs/ndii_new' 
results_folder = file.path(main_folder,'postprocess')			# Where to put results
in_modgrid_df = get(load(file.path(results_folder,'out_df_modgrid.RData')))
in_grid_dfs = list.files(results_folder,'.RData', full.names = T)
in_grid_dfs = in_grid_dfs[c(5,3,4,1,2)]

grid_folder = file.path(results_folder,'GRIDs')   #save the grid here
grid_extent = data.frame(xmin = 13557688.37,ymin = 1188165.46, xmax = 13650350.93, ymax =  1280828.02)
grid_resolutions = c(231.656358,2084.907, 5096.432, 10192.864,20385.73 )
grid_shapes <- c('Grid_MODIS_250','Grid_2Km','Grid_5Km','Grid_10Km','Grid_20Km')		 #this is the grid name

out_pdf = file.path(results_folder,'PDFs','Output_postprocessing.pdf')
dir.create(dirname(out_pdf))
savepdf(out_pdf)

hans_fc_thresh = 50			# Threshold used to mask out "forest" pixels
mod_pix_area = 231.656358*231.656358
mask_hansen = F

doy_limit = 365

in_modgrid_df[which(in_modgrid_df$min_q3 > doy_limit),
		c('n_seasons','ncells',"min_q1","min_q2","min_q3","min_q4","max_q1",
				'max_q2', 'max_q3','max_q4','length_q1','length_q2','length_q3','length_q4')] = NA


if (mask_hansen == T) {
# "apply" the hansen forest cover map to MODIS results !
in_modgrid_df[which(in_modgrid_df$hans_forcov >hans_fc_thresh),
												c('n_seasons','ncells',"min_q1","min_q2","min_q3","min_q4","max_q1",
								     	'max_q2', 'max_q3','max_q4','length_q1','length_q2','length_q3','length_q4')] = NA
}
#in_modgrid_df = droplevels(subset(in_modgrid_df, hans_forcov < 50))
in_modgrid_df = droplevels(subset(in_modgrid_df, ncells > 0 & is.na( ref_ricearea) == FALSE))			# Remove "cells" with no "good data" pixels
#in_modgrid_df = droplevels(subset(in_modgrid_df, ncells > 3941))
#browser()
	#- ------------------------------------------------------------------------------- -#
	#  # Compute Accuracy on areas for the different provinces
	#- ------------------------------------------------------------------------------- -#
{{
		acc_areas_comp = function (data_in,Season){		# Accessory function used to Compute Accuracy (on areas)

			if (Season == 'All') {
				MOD_pixels = (is.finite(data_in$n_seasons))  # == TRUE if at least one season detected
			}
			if (Season == 'First') {
				MOD_pixels = is.finite(data_in$min_q1) | is.finite(data_in$min_q2)  # == TRUE if at least one season detected in 1st and 2nd quart
			}
			if (Season == 'Second') {
				MOD_pixels =  is.finite(data_in$min_q3)| is.finite(data_in$min_q4)  # == TRUE if at least one season detected in 3rdand 4th quart
			}
			MOD_Area =  sum(data_in$tot_cellarea[MOD_pixels], na.rm = T)		# total area recognized by MODIS (I use totcellarea, which already excludes area in nodata areas (e.g,
																																			# if I have a MODIS pixel with 1/4 area on the sea, I use only 3/4 of the area of the pixel
			Reference_Area = sum(data_in$ref_ricearea, na.rm = T)						# total area recognized by Reference

			# Compute areas required for accuracy matrix
			ones_ones = sum(data_in$ref_ricearea [MOD_pixels], na.rm = T)		# Areas at one in bot ref and MOD --> equal to reference area in pixels in which season was detected
			zeroes_zeroes = sum(data_in$tot_cellarea [!MOD_pixels], na.rm = T)-sum(data_in$ref_ricearea [!MOD_pixels], na.rm = T)  # Areas at 0 in both --> equal to total cell area in pixels
																																																														# without detections, minus the area detected by reference in the same pixels
			sar_1_mod_0 = sum(data_in$ref_ricearea [!MOD_pixels], na.rm = T)	# ones in reference, zeroes in MOD
			sar_0_mod_1 = MOD_Area-sum(data_in$ref_ricearea[MOD_pixels], na.rm = T)		# ones in MOD, zeroes in Reference --> Total area in MODIS (see above), minus area of SAR in the same pixels

			# Compute accuracy measures
			acc_mat = NULL
			acc_mat$OA = (ones_ones+zeroes_zeroes)/(ones_ones+zeroes_zeroes+sar_1_mod_0+sar_0_mod_1)*100
			acc_mat$Omission_Err = 100*(sar_1_mod_0/(sar_1_mod_0+ ones_ones))
			acc_mat$Commission_Err = 100*(sar_0_mod_1/(ones_ones+sar_0_mod_1))
			acc_mat$Prod_Accuracy = 100-acc_mat$Omission_Err
			acc_mat$User_Accuracy = 100-acc_mat$Commission_Err
			acc_mat$MOD_Area = MOD_Area
			acc_mat$Reference_Area = Reference_Area
			acc_mat$Season = Season
			return(acc_mat)
		}
		acc_mat_areas = NULL
		acc_mat_areas_tot =NULL
		for (Season in c('All','First','Second')) {
			acc_areas_seas = acc_areas_comp(in_modgrid_df,Season)		# Compute on all
			acc_areas_seas$Province  = 'All'
#			acc_mat_areas = rbind(acc_mat_areas, acc_areas_seas)
			acc_mat_areas_Admin = dlply(in_modgrid_df, .(Province), function(df)acc_areas_comp(df,Season))		# Compute separated by province
			provnames = names(acc_mat_areas_Admin)
			acc_mat_areas_Admin = rbindlist(acc_mat_areas_Admin)		# convert to data table
			acc_mat_areas_Admin$Province  = provnames
			acc_mat_areas_tot = rbind(acc_mat_areas_tot,acc_areas_seas, acc_mat_areas_Admin)   # rbinf "all" and "by province" results in a unique table
		}
		setcolorder(acc_mat_areas_tot, c('Province','Season',names(acc_mat_areas_tot)[1:7]))
		acc_mat_areas_tot = droplevels(subset(acc_mat_areas_tot, OA != 100))
	}}

# Add results to shapefile @data
#
#data_2_join = droplevels(subset(acc_mat_areas_tot, Season == 'All'))
#in_shape_administrative@data = join(in_shape_administrative@data,data_2_join, by = 'Province', type = 'left' )

{{# Plot the results
		data_melted = melt(acc_mat_areas_tot)
		data_melted$Season = as.factor(data_melted$Season)
		data_melted$Province = as.factor(data_melted$Province)
		data_melted = droplevels(subset(data_melted, (Province !='NA')))
		area_data <- droplevels(subset(data_melted,(variable %in% names(acc_mat_areas_tot)[c(1:2,8:9)])))
		names(area_data)[4] = 'Area'
		area_data$Area = area_data$Area/10000

# Plot Area Comparisons on all map
		p = ggplot (droplevels(subset(area_data,Province =='All')),aes(x = Season, y = Area, fill = variable)) + theme_bw()
		p = p + geom_bar(stat = 'identity', position = position_dodge()) + facet_wrap(~Province)
		print(p + ggtitle("MODIS vs Reference Area"))

# Plot Area Comparisons  by Provinces
		p = ggplot (droplevels(subset(area_data,Province !='All')),aes(x = Season, y = Area, fill = variable)) + theme_bw()
		p = p + geom_bar(stat = 'identity', position = position_dodge()) + facet_wrap(~Province)
		print(p + ggtitle("MODIS vs Reference Area - by Provinces"))

# Plot Accracies on all mop and then by province
		acc_data <- droplevels(subset(data_melted,(variable %in% names(acc_mat_areas_tot)[c(3,6,7)])))
		names(acc_data)[4] = 'Accuracy_Metric'

		p = ggplot (droplevels(subset(acc_data,Province =='All')),aes(x = Season, y = Accuracy_Metric, fill = variable)) + theme_bw()
		p = p + geom_bar(stat = 'identity', position = 'dodge') + facet_wrap(~Province)
		p = p + scale_fill_discrete('Accuracy Metric', labels = c('Overall Accuracy Map', 'Prod. Accuracy Rice', 'User Accuracy Rice'))
		print(p+ theme(legend.justification=c(1,0), legend.position="top")+ ggtitle("Detection Accuracies (Computed on Areas)"))

		acc_data2 = droplevels(subset(acc_data,(Province !='All' & variable  != 'OA')))
		p = ggplot (acc_data2,aes(x = Season, y = Accuracy_Metric, fill = variable)) + theme_bw()
		p = p + geom_bar(stat = 'identity', position = position_dodge(width = 1.0)) + facet_wrap(~Province)
		p = p + scale_fill_discrete('Accuracy_Metric', labels = c('Prod. Accuracy Rice', 'User Accuracy Rice'))
		print(p+ ggtitle("Detection Accuracies (Computed on Areas) - by Provinces"))
	}}

{{
		acc_pixels = function(data_in,Season){ # Accessory function to Compute accuracies as number of pixels correctly / incorrectly identified, as a function of rice fractional cover in
																					# MODIS cell
			threshs = seq(0.05,0.95,0.05)
			acc_out = list()

			if (Season == 'All') {
				mod_binary = 1*(is.finite(data_in$n_seasons))
			}
			if (Season == 'First') {
				mod_binary = 1*(is.finite(data_in$min_q1) | is.finite(data_in$min_q2))  # == TRUE if at least one season detected in 1st and 2nd quart
			}
			if (Season == 'Second') {
				mod_binary =  1*(is.finite(data_in$min_q3) | is.finite(data_in$min_q4))  # == TRUE if at least one season detected in 3rdand 4th quart
			}

			for(th in seq_along(threshs)){

				ref_binary = 1*(data_in$ref_rice_fc > threshs[th])
				conf_table = confusion.matrix(ref_binary,mod_binary)
				accuracy = accuracy(ref_binary,mod_binary)
				accuracy$Omission_Err_Rice = 100-100*accuracy$sensitivity
				accuracy$Commission_Err_Rice = 100*conf_table[2,1]/(conf_table[2,1]+conf_table[2,2])
				accuracy$threshold = threshs[th]*100
				acc_out[[th]] = accuracy
			}
			acc_out = rbindlist(acc_out)
			acc_out$Season = Season
			acc_out
		}
		acc_pix_all = NULL
		acc_pix_all_tot =NULL
		for (Season in c('All','First','Second')) {
			acc_pix_all = acc_pixels(in_modgrid_df,Season)			# On all
			acc_pix_all$Province = 'All'
			acc_pix_admin =  ddply(in_modgrid_df, .(Province), function(df)acc_pixels(df,Season))   # by province
			acc_pix_all_tot = rbind(acc_pix_all_tot,acc_pix_all,acc_pix_admin)
	}
	}}

# Add results to admin shapefil
#
#data_accpix_join = droplevels(subset(acc_pix_all, (threshold == '75' & Province != 'All')))
#data_accpix_join = data_accpix_join[, names(data_accpix_join) %in% c('Province','Kappa','Omission_Err_Rice','Commission_Err_Rice'), with=FALSE]
#in_shape_administrative@data = join(in_shape_administrative@data,data_accpix_join, by = 'Province', type = 'left' )

# Plot results
data_melted = melt(acc_pix_all_tot, id.vars = c(1,10,11))
plot_data <- droplevels(subset(data_melted,(variable %in% names(acc_pix_all)[c(1,8,9,10,11)])))

# Plot Accuracies on all mop
p = ggplot (droplevels(subset(plot_data,Province =='All')),aes(x = threshold, y = value, pch = variable, colour = Season, group = interaction(Season,variable))) + theme_bw()
p = p + geom_point()+geom_line(colour = 'black',linetype = 'dashed')
p = p +	scale_x_continuous('% of rice in MODIS pixel', limits = c(0,100), breaks = seq(0,100,10) )+
		scale_y_continuous('Metric (%)', limits = c(0,100),breaks = seq(0,100,10))+theme(plot.title = element_text(vjust = 1, hjust = 0))
print(p+ggtitle('Omission and Commision (as % of pixels) as a function of rice fc in MODIS pixels'))

# By provinces

p = ggplot (droplevels(subset(plot_data,Province !='All')),aes(x = threshold, y = value, pch = variable, colour = Season, group = interaction(Season,variable))) + theme_bw()
p = p + geom_point()+geom_line(colour = 'black',linetype = 'dashed')+facet_wrap(~Province)
p = p +	scale_x_continuous('% of rice in MODIS pixel', limits = c(0,100), breaks = seq(0,100,10) )+
		scale_y_continuous('Metric (%)', limits = c(0,100),breaks = seq(0,100,10))+theme(plot.title = element_text(vjust = 1, hjust = 0))
print(p+ggtitle('Omission and Commision (as % of pixels) as a function of rice fc in MODIS pixels - By Provinces'))

# Boxplots of dates

data = in_modgrid_df

# Compute average doys of minum/maximum

doys_provinces = ddply (data, .(Province), summarize, avg_q1 = mean(min_q1, na.rm = T)
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


# Join results to shapefile

is.na(doys_provinces) <- do.call(cbind,lapply(doys_provinces, is.nan))
is.na(data) <- do.call(cbind,lapply(data, is.nan))

#in_shape_administrative@data = join(in_shape_administrative@data ,doys_provinces, by = 'Province', type = 'left')

#writeOGR (in_shape_administrative,'W:/lorenzo/phenorice/philippines/GIS',outshape,driver="ESRI Shapefile",overwrite_layer=T)

# Print boxplots of minimums
pro = melt(data,value.name = "DOY", measure.vars = c('min_q1','min_q2','min_q3','min_q4'), na.rm = T )
names(pro)[length(names(pro))-1] = 'Quarter'
names(pro)[length(names(pro))] = 'DOY'
#pro$DOY = as.Date(pro$DOY - 1, origin = "2013-01-01")

give.n <- function(x){   # Accessory function to get N° of samples in each group
	return(c(y =-100, label = length(x)))
	}

p = ggplot(pro, aes(x = Quarter, y = DOY))+theme_bw()
p = p + geom_boxplot(outlier.colour = 'transparent')+geom_jitter(position = position_jitter(w = 0.1), size = 0.1)
p = p +theme(plot.title = element_text(vjust = 1, hjust = 0))+
		stat_summary(fun.data = give.n, geom = "text", fun.y = 0, colour = 'red')
print(p+ggtitle('Distribution of retrieved Minimum DOYs in the 4 quarters'))

p = ggplot(pro, aes(DOY, fill = Quarter))+theme_bw()
p = p + geom_density(alpha=.5, adjust  = 1)
p = p +theme(plot.title = element_text(vjust = 1, hjust = 0))
print(p+ggtitle('Distribution of retrieved Minimum DOYs in the 4 quarters'))

p = ggplot(pro, aes(x = Quarter, y = DOY))+theme_bw()
p = p + geom_boxplot(outlier.colour = 'transparent')+geom_jitter(position = position_jitter(w = 0.1), size = 0.1)+
		stat_summary(fun.data = give.n, geom = "text",size = 3, fun.y = 0, colour = 'red')
p = p + facet_wrap(~Province)+ggtitle('Distribution of retrieved Minimum DOYs in the 4 quarters - by Province')+theme(plot.title = element_text(vjust = 1, hjust = 0))
print(p)

# Print boxplots of maximums
pro = melt(data,value.name = "DOY", measure.vars = c('max_q1','max_q2','max_q3','max_q4'), na.rm = T )

names(pro)[length(names(pro))-1] = 'Quarter'
names(pro)[length(names(pro))] = 'DOY'

#pro$DOY = as.Date(pro$DOY - 1, origin = "2013-01-01")

p = ggplot(pro, aes(x = Quarter, y = DOY))+theme_bw()
p = p + geom_boxplot(outlier.colour = 'transparent')+geom_jitter(position = position_jitter(w = 0.1), size = 0.1)
p = p +theme(plot.title = element_text(vjust = 1, hjust = 0))+
		stat_summary(fun.data = give.n, geom = "text", fun.y = 0, colour = 'red')
print(p+ggtitle('Distribution of retrieved Maximums DOYs in the 4 quarters'))

p = ggplot(pro, aes(DOY, fill = Quarter))+theme_bw()
p = p + geom_density(alpha=.5, adjust  = 1)
p = p +theme(plot.title = element_text(vjust = 1, hjust = 0))
print(p+ggtitle('Distribution of retrieved Maximums DOYs in the 4 quarters'))

p = ggplot(pro, aes(x = Quarter, y = DOY))+theme_bw()
p = p + geom_boxplot(outlier.colour = 'transparent')+geom_jitter(position = position_jitter(w = 0.1), size = 0.1)+
		stat_summary(fun.data = give.n, geom = "text",size = 3, fun.y = 0, colour = 'red')
p = p + facet_wrap(~Province)+ggtitle('Distribution of retrieved Maximums DOYs in the 4 quarters - by Province')+theme(plot.title = element_text(vjust = 1, hjust = 0))
print(p)

# Print boxplots of lengths of season
pro = melt(data,value.name = "DOY", measure.vars = c('length_q1','length_q2','length_q3','length_q4'), na.rm = T )

names(pro)[length(names(pro))-1] = 'Quarter'
names(pro)[length(names(pro))] = 'DOY'
give.n <- function(x){
	return(c(y =0, label = length(x)))
}

p = ggplot(pro, aes(x = Quarter, y = DOY))+theme_bw()
p = p + geom_boxplot(outlier.colour = 'transparent')+geom_jitter(position = position_jitter(w = 0.1), size = 0.1)
p = p +theme(plot.title = element_text(vjust = 1, hjust = 0))+
		stat_summary(fun.data = give.n, geom = "text", fun.y = 0, colour = 'red')
print(p+ggtitle('Distribution of retrieved Lengths DOYs in the 4 quarters'))

p = ggplot(pro, aes(DOY, fill = Quarter))+theme_bw()
p = p + geom_density(alpha=.5, adjust  = 1)
p = p +theme(plot.title = element_text(vjust = 1, hjust = 0))
print(p+ggtitle('Distribution of retrieved Lengths DOYs in the 4 quarters'))

p = ggplot(pro, aes(x = Quarter, y = DOY))+theme_bw()
p = p + geom_boxplot(outlier.colour = 'transparent')+geom_jitter(position = position_jitter(w = 0.1), size = 0.1)+
		stat_summary(fun.data = give.n, geom = "text",size = 3, fun.y = 0, colour = 'red')
p = p + facet_wrap(~Province)+ggtitle('Distribution of retrieved Lengths DOYs in the 4 quarters - by Province')+theme(plot.title = element_text(vjust = 1, hjust = 0))
print(p)

# Section of code used to plot maps

 centroids = getSpPPolygonsLabptSlots(readOGR(grid_folder,grid_shapes[1]))	# get coordinates of centroids of each 250m cell
 centroids = SpatialPoints(centroids,proj4string =  CRS(in_proj)) # convert to spatialpoints
 centroids_coords = NULL
 centroids_coords$x = centroids@coords[,1]
 centroids_coords$y = centroids@coords[,2]
 centroids_coords$id = seq(1:length(centroids_coords$y))
 centroids_coords = data.frame(x = centroids_coords$x,y = centroids_coords$y,id = centroids_coords$id)
 data_plot = join(centroids_coords, data, by = 'id',type = 'left')
 ext_grd = extent(centroids)
#
#con <- url('http://biogeo.ucdavis.edu/data/gadm2/R/ITA_adm2.RData')
#load(con)
#map = gadm
#map = spTransform(map, CRS('+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs '))

mapit = readOGR('D:/Temp','Prov2011_WGS84')
mapit = spTransform(mapit, CRS(in_proj))
map_df = fortify(mapit, region = "COD_PRO")
#map_data = data.frame(id = unique(mapit_df$id), value = rnorm(1647))

 legends = c("Doy of Min", "Doy of Min", "Doy of Min", "Doy of Min", "Doy of Max", "Doy of Max", "Doy of Max",  "Doy of Max", "Min-Max length",    "Min-Max length",    "Min-Max length",    "Min-Max length" )
 for (var in names(data_plot)[7:18]) {

 #	for (cycle in 1:4) {
 ##	data_sub = melt(data_plot, id.vars = c(1,2,3),measure_vars = c(7:10))
 #	data_sub = data_sub[,c(1,2,3,(8+(cycle-1)*4:8+(cycle)*4))]
 	data_pp = subset(data_plot,ncells > 30  )
	
	if (length(which(is.finite(data_pp[,var]) == T))){
 #   data_pp = subset(data_pp, is.na(min_q3) == FALSE)
 	p <- ggplot(data = data_pp, aes(x = x, y = y))
 # 	p <- ggplot(aes_string( fill = var))
	
 	mapfract <- p + geom_tile(aes_string( fill = var))#+facet_wrap(~variable, scales = 'free_y')
  	mapfract = mapfract +coord_fixed(xlim = c(ext_grd@xmin,ext_grd@xmax), ylim = c(ext_grd@ymin,ext_grd@ymax))
 	mapfract = mapfract + theme_bw() + labs(title = paste("Map of ", var ,sep = ''), x = "Longitude", y = "Latitude")+ theme(plot.title = element_text(size = 14, vjust = 1))+
 			labs(x = "Longitude") + theme(axis.text.x  = element_text(size = 8) ,axis.text.y  = element_text(size = 8))+ theme(legend.position="right")
 #mapfract = mapfract + theme(plot.margin = unit(c(1,1,1,1), "cm"))
 	mapfract <- mapfract + scale_fill_gradientn(legends[var],colours = topo.colors(10), na.value = "transparent" )
 	mapfract <- mapfract + geom_polygon(data = map_df,aes(x = long, y = lat, group = group),  fill = 'transparent', colour = "black")
 	print(mapfract)
}
 } #End Cycle on var

	#- ------------------------------------------------------------------------------- -#
	#  Perform analysis on the different grids
	#- ------------------------------------------------------------------------------- -#
for (grid in 2:length(in_grid_dfs)) {

	in_grid_data = get(load(in_grid_dfs[grid]))
#	grid_res =
	is.na(in_grid_data) <- do.call(cbind,lapply(in_grid_data, is.infinite))
	is.na(in_grid_data) <- do.call(cbind,lapply(in_grid_data, is.nan))
	if (mask_hansen == T) {
	in_grid_data[which(in_grid_data$hans_forcov >hans_fc_thresh),
												c('n_seasons','ncells',"min_q1","min_q2","min_q3","min_q4","max_q1",
								     	'max_q2', 'max_q3','max_q4','length_q1','length_q2','length_q3','length_q4')] = NA

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
					mean_max_q4 = mean(max_q4, na.rm = T),
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
					stdev_length_q4 = sd(length_q4, na.rm = T),
					mean_hansforcov = mean(hans_forcov, na.rm = T),# mean hansen forest cover n each of the 4 quarters
					maj_province = names(table(Province)[table(Province)==max(table(Province))])) # majority province
#								maj_region = names(table(Region)[table(Region)==max(table(Region))]))  # majority region
			, by = key(in_grid_data)]
	} else {

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
						mean_max_q4 = mean(max_q4, na.rm = T),
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
#						maj_province = names(table(Province)[table(Province)==max(table(Province))])) # majority province
				, by = key(in_grid_data)]
#								maj_region = names(t
	}
	yvars = c('mod_ricearea_q1','mod_ricearea_q2','mod_ricearea_q3','mod_ricearea_q4','mod_ricearea_tot')
	plots = list()

	# Create and print plots of comparison between reference (SAR) and MODIS retrieved areas
	for (quart in 1:5) {

		yvar = yvars[quart]
		Regr_Stats =  regress_stats(x = aggregate_values$ref_rice_Area , y = aggregate_values[[yvar]])
		eqn_strings = lm_eqn(Regr_Stats)
#	eqn_strings$range_max = range_max
		p1 = ggplot(data = aggregate_values, aes_string(x = 'ref_rice_Area', y = yvar))
		p1 = p1 + geom_point(alpha = 0.3)+theme_bw()+xlim(0,max(max(aggregate_values$mod_ricearea_q1,aggregate_values$ref_rice_Area)))+ylim(0,max(max(aggregate_values$mod_ricearea_q1,aggregate_values$ref_rice_Area)))
		p1 = p1 + geom_abline(intercept = 0, slope = 1, linetype = 2)
		p1 = p1 + geom_abline(aes(intercept = Intercept, slope = Slope),data = Regr_Stats,linetype = 4, color = 'red', size = 0.9)
		p1 = p1 + geom_text(aes(x = 0, y = (max(aggregate_values$mod_ricearea_q3)-min(aggregate_values$mod_ricearea_q3))/2, label = eq), data = eqn_strings, parse = T, size = 4, hjust = 0, face = 'bold')
		p1 = p1 + xlab('Reference Rice Area')+ylab('MODIS detected rice area')
		p1 = p1 + ggtitle(paste(grid_dim,' Grid - Quarter = ', quart, sep = ''))
		plots[[quart]] = p1
	} #End Cycle on quart
# 	browser()
	print(grid.arrange(plots[[1]],plots[[2]],plots[[3]],plots[[4]],plots[[5]]))

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

	# Plot rice area derived from the reference map
	p <- ggplot(data = data_plot, aes(x = x, y = y))
	var = 'ref_rice_Area'
	mapfract <- p + geom_tile(aes_string(fill = var))
	mapfract = mapfract +coord_fixed(xlim = c(ext_grd@xmin,ext_grd@xmax), ylim = c(ext_grd@ymin,ext_grd@ymax))
	mapfract = mapfract + theme_bw() + labs(title = paste("Map of ", var ,sep = ''), x = "Longitude", y = "Latitude")+ theme(plot.title = element_text(size = 14, vjust = 1))+
			labs(x = "Longitude") + theme(axis.text.x  = element_text(size = 8) ,axis.text.y  = element_text(size = 8))+ theme(legend.position="right")
	#mapfract = mapfract + theme(plot.margin = unit(c(1,1,1,1), "cm"))
	mapfract <- mapfract + scale_fill_gradientn('Area (ha)',colours = topo.colors(10), na.value = "transparent" )
	mapfract <- mapfract + geom_polygon(data = map_df,aes(x = long, y = lat, group = group),  fill = 'transparent', colour = "black")


	# Add maps of areas derived from MODIS
	for (var in names(data_plot)[6:10]) {
		p <- ggplot(data = data_plot, aes(x = x, y = y))
		mapfract_mod <- p + geom_tile(aes_string(fill = var))
		mapfract_mod = mapfract_mod +coord_fixed(xlim = c(ext_grd@xmin,ext_grd@xmax), ylim = c(ext_grd@ymin,ext_grd@ymax))
		mapfract_mod = mapfract_mod + theme_bw() + labs(title = paste("Map of ", var ,sep = ''), x = "Longitude", y = "Latitude")+ theme(plot.title = element_text(size = 14, vjust = 1))+
				labs(x = "Longitude") + theme(axis.text.x  = element_text(size = 8) ,axis.text.y  = element_text(size = 8))+ theme(legend.position="right")
		#mapfract = mapfract + theme(plot.margin = unit(c(1,1,1,1), "cm"))
	mapfract_mod <- mapfract_mod + scale_fill_gradientn('Area (ha)',colours = topo.colors(10), na.value = "transparent" )
	mapfract_mod <- mapfract_mod + geom_polygon(data = map_df,aes(x = long, y = lat, group = group),  fill = 'transparent', colour = "black")
		print(grid.arrange(mapfract,mapfract_mod, ncol = 2))
	}

#	legends = c("Doy of Min", "Doy of Min", "Doy of Min", "Doy of Min", "Doy of Max", "Doy of Max", "Doy of Max",  "Doy of Max", "Min-Max length",    "Min-Max length",    "Min-Max length",    "Min-Max length" )
#	for (var in names(data_plot)[11:22]) {
#
#		p <- ggplot(data = data_plot, aes(x = x, y = y))
#		mapfract <- p + geom_tile(aes_string(fill = var))
#		mapfract = mapfract +coord_fixed(xlim = c(ext_grd@xmin,ext_grd@xmax), ylim = c(ext_grd@ymin,ext_grd@ymax))
#		mapfract = mapfract + theme_bw() + labs(title = paste("Map of ", var ,sep = ''), x = "Longitude", y = "Latitude")+ theme(plot.title = element_text(size = 14, vjust = 1))+
#				labs(x = "Longitude") + theme(axis.text.x  = element_text(size = 8) ,axis.text.y  = element_text(size = 8))+ theme(legend.position="right")
##mapfract = mapfract + theme(plot.margin = unit(c(1,1,1,1), "cm"))
#		mapfract <- mapfract + scale_fill_gradientn(legends[var],colours = topo.colors(10), na.value = "transparent" )
#		mapfract <- mapfract + geom_polygon(data = map_df,aes(x = long, y = lat, group = group),  fill = 'transparent', colour = "black")
#		print(mapfract)
#
#	} #End Cycle on var

#	Regr_Stats =  regress_stats(x = aggregate_values$ref_rice_Area , y = aggregate_values$mod_ricearea_q1)
#	eqn_strings = lm_eqn(Regr_Stats)
##	eqn_strings$range_max = range_max
#  p1 = ggplot(data = aggregate_values, aes(x = ref_rice_Area, y = mod_ricearea_q1))
#	p1 = p1 + geom_point(alpha = 0.3)+theme_bw()+xlim(0,max(max(aggregate_values$mod_ricearea_q1,aggregate_values$ref_rice_Area)))+ylim(0,max(max(aggregate_values$mod_ricearea_q1,aggregate_values$ref_rice_Area)))
#	p1 = p1 + geom_abline(intercept = 0, slope = 1, linetype = 2)
#	p1 = p1 + geom_abline(aes(intercept = Intercept, slope = Slope),data = Regr_Stats,linetype = 4, color = 'red', size = 0.9)
#	p1 = p1 + geom_text(aes(x = 30, y = 380, label = eq), data = eqn_strings, parse = T, size = 4, hjust = 0, face = 'bold')
#	p1 = p1 + xlab('Reference Rice Area')+ylab('MODIS detected rice area')
#	p1 = p1 + ggtitle(paste(grid_dim,' Grid - First Quarter', sep = ''))
##  print(p1)
#
#	Regr_Stats =  regress_stats(x = aggregate_values$ref_rice_Area , y = aggregate_values$mod_ricearea_q2)
#	eqn_strings = lm_eqn(Regr_Stats)
#
#	p2 = ggplot(data = aggregate_values, aes(x = ref_rice_Area, y = mod_ricearea_q2))
#	p2 = p2 + geom_point(alpha = 0.3)+theme_bw()+xlim(0,max(max(aggregate_values$mod_ricearea_q1,aggregate_values$ref_rice_Area)))+ylim(0,max(max(aggregate_values$mod_ricearea_q1,aggregate_values$ref_rice_Area)))
#	p2 = p2 + geom_abline(intercept = 0, slope = 1, linetype = 2)
#	p2 = p2 + geom_abline(aes(intercept = Intercept, slope = Slope),data = Regr_Stats,linetype = 4, color = 'red', size = 0.9)
#	p2 = p2 + geom_text(aes(x = 30, y = 380, label = eq), data = eqn_strings, parse = T, size = 4, hjust = 0, face = 'bold')
#	p2 = p2 + xlab('Reference Rice Area')+ylab('MODIS detected rice area')
#	p2 = p2 + ggtitle(paste(grid_dim,' Grid - Second Quarter', sep = ''))
##	print(p2)
#
#Regr_Stats =  regress_stats(x = aggregate_values$ref_rice_Area , y = aggregate_values$mod_ricearea_q3)
#eqn_strings = lm_eqn(Regr_Stats)
#	p3 = ggplot(data = aggregate_values, aes(x = ref_rice_Area, y = mod_ricearea_q3))
#	p3 = p3 + geom_point(alpha = 0.3)+theme_bw()+xlim(0,max(max(aggregate_values$mod_ricearea_q1,aggregate_values$ref_rice_Area)))+ylim(0,max(max(aggregate_values$mod_ricearea_q1,aggregate_values$ref_rice_Area)))
#	p3 = p3 + geom_abline(intercept = 0, slope = 1, linetype = 2)
#	p3 = p3 + geom_abline(aes(intercept = Intercept, slope = Slope),data = Regr_Stats,linetype = 4, color = 'red', size = 0.9)
#	p3 = p3 + geom_text(aes(x = 30, y = 380, label = eq), data = eqn_strings, parse = T, size = 4, hjust = 0, face = 'bold')
#	p3 = p3 + xlab('Reference Rice Area')+ylab('MODIS detected rice area')
#	p3 = p3 + ggtitle(paste(grid_dim,' Grid - Third Quarter', sep = ''))
##	print(p5)
#
#Regr_Stats =  regress_stats(x = aggregate_values$ref_rice_Area , y = aggregate_values$mod_ricearea_q4)
#eqn_strings = lm_eqn(Regr_Stats)
#	p4 = ggplot(data = aggregate_values, aes(x = ref_rice_Area, y = mod_ricearea_q4))
#	p4 = p4 + geom_point(alpha = 0.3)+theme_bw()+xlim(0,max(max(aggregate_values$mod_ricearea_q1,aggregate_values$ref_rice_Area)))+ylim(0,max(max(aggregate_values$mod_ricearea_q1,aggregate_values$ref_rice_Area)))
#	p4 = p4 + geom_abline(intercept = 0, slope = 1, linetype = 2)
#	p4 = p4 + geom_abline(aes(intercept = Intercept, slope = Slope),data = Regr_Stats,linetype = 4, color = 'red', size = 0.9)
#	p4 = p4 + geom_text(aes(x = 30, y = 380, label = eq), data = eqn_strings, parse = T, size = 4, hjust = 0, face = 'bold')
#	p4 = p4 + xlab('Reference Rice Area')+ylab('MODIS detected rice area')
#	p4 = p4 + ggtitle(paste(grid_dim,' Grid - Fourth Quarter', sep = ''))
##	print(p5)
#
#Regr_Stats =  regress_stats(x = aggregate_values$ref_rice_Area , y = aggregate_values$mod_ricearea_tot)
#eqn_strings = lm_eqn(Regr_Stats)
##eqn_strings$range_max = range_max
#	p5 = ggplot(data = aggregate_values, aes(x = ref_rice_Area, y = mod_ricearea_tot))
#	p5 = p5 + geom_point(alpha = 0.3)+theme_bw()+xlim(0,max(max(aggregate_values$mod_ricearea_q1,aggregate_values$ref_rice_Area)))+ylim(0,max(max(aggregate_values$mod_ricearea_q1,aggregate_values$ref_rice_Area)))
#	p5 = p5 + geom_abline(intercept = 0, slope = 1, linetype = 2)
#	p5 = p5 + geom_abline(aes(intercept = Intercept, slope = Slope),data = Regr_Stats,linetype = 4, color = 'red', size = 0.9)
#	p5 = p5 + geom_text(aes(x = 30, y = 380, label = eq), data = eqn_strings, parse = T, size = 4, hjust = 0, face = 'bold')
#	p5 = p5 + xlab('Reference Rice Area')+ylab('MODIS detected rice area')
#	p5 = p5 + ggtitle(paste(grid_dim,' Grid - All Detections', sep = ''))
#	print(p5)



	# Save results as RData file
#	save(aggregate_values, file = out_grid_df_files[cycle])

} #End Cycle on grid


dev.off()
