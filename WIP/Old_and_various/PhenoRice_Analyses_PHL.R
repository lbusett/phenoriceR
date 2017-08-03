# TODO: Add comment
# 
# Author: LB
###############################################################################
savepdf <- function(file, width=29.7, height=21)
{ fname <- file
	pdf(fname, width=width/2.54, height=height/2.54,
			pointsize=10)
}

out_pdf = file.path('W:/lorenzo/phenorice/philippines/Out9', 'hansenmask_out9.pdf')
outshape = 'shape_admin_data_hansen'
savepdf(out_pdf)    

library(ggplot2)
library(reshape)
library(data.table)
library(rgdal)
library(SDMTools)
library(plyr)



#in_data_db = 'W:/francesco/PhenoRice_processing/PHL/Analysis/DataBase/Out5/dB_PHL_2013_Out5_v2.RData'
#in_data_full = get(load(in_data_db))

in_data_db = 'W:/francesco/PhenoRice_processing/PHL/Analysis/DataBase/Out9/dB_PHL_2013_Out9.Rdata'
in_data_full = get(load(in_data_db))

#in_data_administrative = readOGR('W:/lorenzo/phenorice/philippines/GIS','Modis_grid_adm_info_point')
in_shape_administrative = readOGR('W:/lorenzo/phenorice/philippines/GIS','Andmin_SA')
in_shape_administrative@data = in_shape_administrative@data [,c("NAME_1","NAME_2")] 
names(in_shape_administrative@data) = c('Region','Province')

# To be used on other script
in_data_formap = read.table("W:/lorenzo/phenorice/philippines/GIS/Forest_Area_Hansen.txt", header = T)
names(in_data_formap)[3]='for_cover'
in_data_formap$area = NULL

names(in_data_administrative@data)[2] = 'ID'
in_data_full = join(in_data_full, in_data_administrative@data, by = 'ID', type = 'left')
in_data_full = join(in_data_full, in_data_formap, by = 'ID', type = 'left')
#save(in_data_full, file = file.choose(new = T))


in_data_masked = droplevels(subset(in_data_full, for_cover < 50))
#in_data_masked = droplevels(subset(in_data_full, mask_eviforest == 1))
#in_data_masked = droplevels(subset(in_data_full, (mask_eviforest == 1) & for_cover < 15))

{{in_data_masked$min_q1 [which(in_data_masked$min_q1 == 0 )] = NA
in_data_masked$min_q2 [which(in_data_masked$min_q2 == 0 )] = NA
in_data_masked$min_q3 [which(in_data_masked$min_q3 == 0 )] = NA
in_data_masked$min_q4 [which(in_data_masked$min_q4 == 0 )] = NA

in_data_masked$max_q1 [which(in_data_masked$max_q1 >= 400 )] = NA
in_data_masked$max_q2 [which(in_data_masked$max_q2  >= 400 )] = NA
in_data_masked$max_q3 [which(in_data_masked$max_q3  >= 400 )] = NA
in_data_masked$max_q4 [which(in_data_masked$max_q4  >= 400 )] = NA

in_data_masked$max_q1 [which(in_data_masked$max_q1 == 0 )] = NA
in_data_masked$max_q2 [which(in_data_masked$max_q2 == 0 )] = NA
in_data_masked$max_q3 [which(in_data_masked$max_q3 == 0 )] = NA
in_data_masked$max_q4 [which(in_data_masked$max_q4 == 0 )] = NA
}}

{{# Compute Accuracy on areas - all quarters
acc_areas_comp = function (data_in){
	
acc_areas = list()
MOD_Area = sum(data_in$MODIS_rice_area_m2)
SAR_Area = sum(data_in$SAR_rice_area_m2)

# Compute Accuracy on areas - 1st and second
MOD_pixels = which(data_in$MODIS_rice_area_m2 > 0)
MOD_pixels_norice = which(data_in$MODIS_rice_area_m2 == 0)
ones_ones = sum(data_in$SAR_rice_area_m2 [MOD_pixels])
zeroes_zeroes = sum(data_in$SAR_norice_area_m2 [MOD_pixels_norice])

sar_1_mod_0 = sum(data_in$SAR_rice_area_m2 [MOD_pixels_norice])
sar_0_mod_1 = sum(data_in$MODIS_rice_area_m2[MOD_pixels])-sum(data_in$SAR_rice_area_m2[MOD_pixels])

acc = NULL
acc$OA = (ones_ones+zeroes_zeroes)/(ones_ones+zeroes_zeroes+sar_1_mod_0+sar_0_mod_1)*100
acc$Omission_Err = 100*(sar_1_mod_0/(sar_1_mod_0+ ones_ones))
acc$Commission_Err = 100*(sar_0_mod_1/(ones_ones+sar_0_mod_1))
acc$MOD_Area = MOD_Area
acc$SAR_Area = SAR_Area
acc$quarter = 'All'
acc_areas [[1]] = acc

# Compute Accuracy on areas - 1std and 2ndth

MOD_pixels = which(is.finite(data_in$min_q1) | is.finite(data_in$min_q2))
MOD_Area = sum(data_in$MODIS_rice_area_m2[MOD_pixels])
MOD_pixels_norice = which(!(is.finite(data_in$min_q1) | is.finite(data_in$min_q2)))
ones_ones = sum(data_in$SAR_rice_area_m2 [MOD_pixels])
zeroes_zeroes = sum(data_in$SAR_norice_area_m2 [MOD_pixels_norice])

sar_1_mod_0 = sum(data_in$SAR_rice_area_m2 [MOD_pixels_norice])
sar_0_mod_1 = sum(data_in$MODIS_rice_area_m2[MOD_pixels])-sum(data_in$SAR_rice_area_m2[MOD_pixels])

acc = NULL
acc$OA = (ones_ones+zeroes_zeroes)/(ones_ones+zeroes_zeroes+sar_1_mod_0+sar_0_mod_1)*100
acc$Omission_Err = 100*(sar_1_mod_0/(sar_1_mod_0+ ones_ones))
acc$Commission_Err = 100*(sar_0_mod_1/(ones_ones+sar_0_mod_1))
acc$MOD_Area = MOD_Area
acc$SAR_Area = SAR_Area
acc$quarter = '1st_Season'
acc_areas [[3]] = acc

# Compute Accuracy on areas - 3rd and 4th
MOD_pixels = which(is.finite(data_in$min_q3) | is.finite(data_in$min_q4))
MOD_Area = sum(data_in$MODIS_rice_area_m2[MOD_pixels])
MOD_pixels_norice = which(!(is.finite(data_in$min_q3) | is.finite(data_in$min_q4)))
ones_ones = sum(data_in$SAR_rice_area_m2 [MOD_pixels])
zeroes_zeroes = sum(data_in$SAR_norice_area_m2 [MOD_pixels_norice])

sar_1_mod_0 = sum(data_in$SAR_rice_area_m2 [MOD_pixels_norice])
sar_0_mod_1 = sum(data_in$MODIS_rice_area_m2[MOD_pixels])-sum(data_in$SAR_rice_area_m2[MOD_pixels])

acc = NULL
acc$OA = (ones_ones+zeroes_zeroes)/(ones_ones+zeroes_zeroes+sar_1_mod_0+sar_0_mod_1)*100
acc$Omission_Err = 100*(sar_1_mod_0/(sar_1_mod_0+ ones_ones))
acc$Commission_Err = 100*(sar_0_mod_1/(ones_ones+sar_0_mod_1))
acc$MOD_Area = MOD_Area
acc$SAR_Area = SAR_Area
acc$quarter = '2nd_Season'
acc_areas [[2]] = acc
acc_areas =  rbindlist(acc_areas)

}

acc_areas_tot = acc_areas_comp(in_data_masked)
acc_areas_tot$NAME_2  = 'All'
acc_areas_Admin = ddply(in_data_masked, .(NAME_2), function(df)acc_areas_comp(df))
acc_areas_tot = rbind(acc_areas_tot,acc_areas_Admin)
names(acc_areas_tot)[7] = 'Province'
}}

# Add results to shapefile @data

data_2_join = droplevels(subset(acc_areas_tot, quarter == 'All'))
in_shape_administrative@data = join(in_shape_administrative@data,data_2_join, by = 'Province', type = 'left' )

{{# Plot the results
data_melted = melt(acc_areas_tot)
names(data_melted)[1] = 'Season'
data_melted$Season = as.factor(data_melted$Season)
levels(data_melted$Season) = c('1st','2nd','All')
area_data <- droplevels(subset(data_melted,(variable %in% names(acc_areas_tot)[4:7])))
names(area_data)[4] = 'Area'
area_data$Area = area_data$Area/10000

# Plot Area Comparisons on all map
p = ggplot (droplevels(subset(area_data,Province =='All')),aes(x = Season, y = Area, fill = variable)) + theme_bw()
p = p + geom_bar(stat = 'identity', position = position_dodge()) + facet_wrap(~Province)
print(p)

# Plot Area Comparisons  by Provinces
p = ggplot (droplevels(subset(area_data,Province !='All')),aes(x = Season, y = Area, fill = variable)) + theme_bw()
p = p + geom_bar(stat = 'identity', position = position_dodge()) + facet_wrap(~Province)
print(p)

# Plot Accracies on all mop
acc_data <- droplevels(subset(data_melted,(variable %in% names(acc_areas_tot)[1:3])))
names(acc_data)[4] = 'Metric'

p = ggplot (droplevels(subset(acc_data,Province =='All')),aes(x = Season, y = Metric, fill = variable)) + theme_bw()
p = p + geom_bar(stat = 'identity', position = position_dodge(width = 1.0)) + facet_wrap(~Province)
p = p + scale_fill_discrete('Metric', labels = c('Overall Accuracy', 'Omission Err. Rice', 'Commission Err. Rice'))
print(p)

#p = ggplot (droplevels(subset(acc_data,(Province !='All' & variable  != 'Overall Accuracy'))),aes(x = Season, y = Metric, fill = variable)) + theme_bw()
#p = p + geom_bar(stat = 'identity', position = position_dodge(width = 1.0)) + facet_wrap(~Province)
#p = p + scale_fill_discrete('Metric', labels = c('Overall Accuracy', 'Omission Err. Rice', 'Commission Err. Rice'))
#p

acc_data2 = droplevels(subset(acc_data,(Province !='All' & variable  != 'OA')))
p = ggplot (acc_data2,aes(x = Season, y = Metric, fill = variable)) + theme_bw()
p = p + geom_bar(stat = 'identity', position = position_dodge(width = 1.0)) + facet_wrap(~Province)
p = p + scale_fill_discrete('Metric', labels = c('Omission Err. Rice', 'Commission Err. Rice'))
print(p)
}}

{{# Compute accuracies as number of pixels
acc_pixels = function(data_in){
threshs = seq(0.05,0.95,0.05)
acc_out = list()

for(th in seq_along(threshs)){
	
	sar_binary = 1*(data_in$SAR_rice_area_percentage > threshs[th])
	mod_binary = 1*(data_in$MODIS_rice_area > 0)
	
	conf_table = confusion.matrix(sar_binary,mod_binary)
	accuracy = accuracy(sar_binary,mod_binary)
	accuracy$Omission_Err_Rice = 100-100*accuracy$sensitivity
	accuracy$Commission_Err_Rice = 100*conf_table[2,1]/(conf_table[2,1]+conf_table[2,2])
	accuracy$threshold = threshs[th]*100
	acc_out[[th]] = accuracy
}
acc_out = rbindlist(acc_out)
acc_out
}

acc_pix_all = acc_pixels(in_data_masked)
acc_pix_all$NAME_2 = 'All'
acc_pix_admin =  ddply(in_data_masked, .(NAME_2), function(df)acc_pixels(df))
acc_pix_all = rbind(acc_pix_all,acc_pix_admin)
names(acc_pix_all)[10] = 'Province'
}}

# Add results to admin shapefil

data_accpix_join = droplevels(subset(acc_pix_all, (threshold == '75' & Province != 'All')))
data_accpix_join = data_accpix_join[, names(data_accpix_join) %in% c('Province','Kappa','Omission_Err_Rice','Commission_Err_Rice'), with=FALSE]
in_shape_administrative@data = join(in_shape_administrative@data,data_accpix_join, by = 'Province', type = 'left' )

# Plot results
data_melted = melt(acc_pix_all, id.vars = c(1,10))
plot_data <- droplevels(subset(data_melted,(variable %in% names(acc_pix_all)[c(1,8,9,10)])))

# Plot Accracies on all mop
p = ggplot (droplevels(subset(plot_data,Province =='All')),aes(x = threshold, y = value, pch = variable, colour = variable)) + theme_bw()
p = p + geom_point()+geom_line(linetype = 'dashed') + facet_wrap(~Province)
p = p + scale_colour_discrete('Legend', labels = c( 'Omission Err. Rice', 'Commission Err. Rice'))+
				scale_shape_discrete('Legend', labels = c( 'Omission Err. Rice', 'Commission Err. Rice'))+
				scale_x_continuous('% of rice in MODIS pixel', limits = c(0,100), breaks = seq(0,100,10) )+
				scale_y_continuous('Metric (%)', limits = c(0,100),breaks = seq(0,100,10))+
				theme(legend.justification = c(0,1),legend.position=c(0,1))
print(p)

# By provinces

p = ggplot (droplevels(subset(plot_data,Province !='All')),aes(x = threshold, y = value, pch = variable, colour = variable)) + theme_bw()
p = p + geom_point()+geom_line(linetype = 'dashed') + facet_wrap(~Province)
p = p + scale_colour_discrete('Legend', labels = c( 'Omission Err. Rice', 'Commission Err. Rice'))+
		scale_shape_discrete('Legend', labels = c( 'Omission Err. Rice', 'Commission Err. Rice'))+
		scale_x_continuous('% of rice in MODIS pixel', limits = c(0,100), breaks = seq(0,100,10) )+
		scale_y_continuous('Metric (%)', limits = c(0,100),breaks = seq(0,100,10))+
		theme(legend.justification = c(1,0),legend.position=c(1,0))
print(p)

# Boxplots of dates

data = in_data_masked
data$length_q1 = 	(data$max_q1+365)-(data$min_q1+365)
data$length_q2 = 	(data$max_q2+365)-(data$min_q2+365)
data$length_q3 = 	(data$max_q3+365)-(data$min_q3+365)
data$length_q4 = 	(data$max_q4+365)-(data$min_q4+365)
# Compute average doys

doys_provinces = ddply (data, .(NAME_2), summarize, avg_q1 = mean(min_q1, na.rm = T)
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
										


names(doys_provinces)[1] = 'Province'

# Join results to shapefile

is.na(doys_provinces) <- do.call(cbind,lapply(doys_provinces, is.nan))
is.na(data) <- do.call(cbind,lapply(data, is.nan))


#a[,!is.finite(a[,])] = NA
in_shape_administrative@data = join(in_shape_administrative@data ,doys_provinces, by = 'Province', type = 'left')

#writeOGR (in_shape_administrative,'W:/lorenzo/phenorice/philippines/GIS',outshape,driver="ESRI Shapefile",overwrite_layer=T) 

# Print boxplots of minimums
pro = melt(data,value.name = "DOY", measure.vars = c('min_q1','min_q2','min_q3','min_q4'), na.rm = T )

#pro = melt(data,value.name = "DOY", measure.vars = c('min_q1','min_q3'), na.rm = T )

names(pro)[36] = 'Quarter'
names(pro)[37] = 'DOY'
#pro$DOY = as.Date(pro$DOY - 1, origin = "2013-01-01")
p = ggplot(pro, aes(x = Quarter, y = DOY))+theme_bw()
p = p + geom_boxplot(outlier.colour = 'transparent')+geom_jitter(position = position_jitter(w = 0.2), size = 0.6, alpha = 0.2)
p = p + facet_wrap(~NAME_2)
print(p)

p = ggplot(pro, aes(x = Quarter, y = DOY))+theme_bw()
p = p + geom_boxplot(outlier.colour = 'transparent')+geom_jitter(position = position_jitter(w = 0.2), size = 0.6, alpha = 0.2)
print(p)

# Print boxplots of maximums
pro = melt(data,value.name = "DOY", measure.vars = c('max_q1','max_q2','max_q3','max_q4'), na.rm = T )

#pro = melt(data,value.name = "DOY", measure.vars = c('max_q1','max_q3'), na.rm = T )

names(pro)[36] = 'Quarter'
names(pro)[37] = 'DOY'
#pro$DOY = as.Date(pro$DOY - 1, origin = "2013-01-01")

p = ggplot(pro, aes(x = Quarter, y = DOY))+theme_bw()
p = p + geom_boxplot(outlier.colour = 'transparent')+geom_jitter(position = position_jitter(w = 0.2), size = 0.6, alpha = 0.2)
print(p)

p = ggplot(pro, aes(x = Quarter, y = DOY))+theme_bw()
p = p + geom_boxplot(outlier.colour = 'transparent')+geom_jitter(position = position_jitter(w = 0.2), size = 0.6, alpha = 0.2)
p = p + facet_wrap(~NAME_2)
print(p)

# Print boxplots of maximums
pro = melt(data,value.name = "DOY", measure.vars = c('length_q1','length_q2','length_q3','length_q4'), na.rm = T )

names(pro)[36] = 'Quarter'
names(pro)[37] = 'DOY'
#pro$DOY = as.Date(pro$DOY - 1, origin = "2013-01-01")



p = ggplot(pro, aes(x = Quarter, y = DOY))+theme_bw()
p = p + geom_boxplot(outlier.colour = 'transparent')+geom_jitter(position = position_jitter(w = 0.2), size = 0.6, alpha = 0.2)
print(p)

p = ggplot(pro, aes(x = Quarter, y = DOY))+theme_bw()
p = p + geom_boxplot(outlier.colour = 'transparent')+geom_jitter(position = position_jitter(w = 0.2), size = 0.6, alpha = 0.2)
p = p + facet_wrap(~NAME_2)
print(p)


dev.off()


#
#pro = melt(data,value.name = "DOY", measure.vars = c('min_1st','min_2nd'), na.rm = T )
#
#names(pro)[43] = 'Season'
#names(pro)[44] = 'DOY'
#pro$DOY = as.Date(pro$DOY - 1, origin = "2013-01-01")
#p = ggplot(pro, aes(x = Season, y = DOY))+theme_bw()
#p = p + geom_boxplot(outlier.colour = 'transparent')+geom_jitter(position = position_jitter(w = 0.1), size = 0.3)
#p = p + facet_wrap(~NAME_2)
#print(p)
#
#p = ggplot(pro, aes(x = Season, y = DOY))+theme_bw()
#p = p + geom_boxplot(outlier.colour = 'transparent')+geom_jitter(position = position_jitter(w = 0.1), size = 0.3)
#print(p)
