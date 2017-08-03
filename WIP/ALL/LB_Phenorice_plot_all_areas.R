lm_eqn = function(df){

  eq <- substitute(italic(y) == a + b %.% italic(x)*""~~italic('r')~"="~r~~italic('MAE')~"="~MAE~~italic('RMSE')~"="~RMSE,
                   list(a = format(df$Intercept, digits = 3),
                        b = format(df$Slope, digits = 3),
                        r = format(df$r, digits = 3),
                        MAE = format(df$MAE, digits = 3),
                        RMSE= format(df$RMSE, digits = 3)
                   ))
  as.character(as.expression(eq))
  out = data.frame(eq = as.character(as.expression(eq)))
  out
}

regress_stats = function( x= x, y = y) {

  # 	sub_y = which(y > 0 & x > 0)
  # 	y = y[sub_y]  		; 			x = x[sub_y]

  if (length(y[is.finite(y)] )>10 & length(x[is.finite(x)] )>10) {
    lin_model = lm(y~x, na.action = na.omit )				# Compute linear model
    sum_mod = summary(lin_model)
    ME = mean (y- x , na.rm = TRUE)  															 # mean error
    MAE = mean(abs((y-x)),na.rm = TRUE) 														 # MAE
    rMAE = 100*mean(abs((y-x)/(x)),na.rm = TRUE)											# relative MAE on original (%)
    RMSE = sqrt(mean((y-x)^2,na.rm = TRUE))												 # RMSE on original
    rRMSE = sqrt(mean(((y-x)/(x+0.001))^2,na.rm = TRUE))  										#	relative RMSE on original(%)
    EF = NSE(y,x)
    sig = cor.test(x,y)$p.value
    rmse2 = rmse(y, x, na.rm = T)
    # Build output data frame
    out = data.frame(N = (sum_mod$df[2]+1), Intercept = sum_mod$coef[[1]], Slope = sum_mod$coef[[2]], r = sum_mod$r.squared^0.5, sig = sig ,  R2 = sum_mod$r.squared, ME = ME, MAE = MAE, rMAE = rMAE, RMSE = RMSE, rRMSE = rRMSE ,EF = EF, rmse2 = rmse2)
    out
  } else {
    out = data.frame(N = length(y[is.finite(y)] >1), Intercept = NA, Slope = NA, r = NA, R2 = NA, ME = NA, MAE = NA, rMAE = NA, RMSE = NA, rRMSE = NA )
  }
}

give.n <- function(x){   # Accessory function to get N? of samples in each group
  return(c(y =-100, label = length(x)))
}

check_seasons = 3
only_nueva =  0
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

only_nueva = 0
# Retrieve data for IND ----

results_folder = "/home/lb/phenorice/PhenoRice/Processing/IND/Outputs/Final/Validate/"
country_code = 'IND'
out_RData_file = file.path(results_folder, 'Plots_Data.RData')
load(out_RData_file)
data_plot_dates_ind = data_plot_dates
data_plot_dates_ind$country = 'IND'
data_DS_Melted_ind = data_DS_Melted
data_DS_Melted_ind$country = 'IND'
data_DS_Melted_2Km_ind = data_DS_Melted_2Km
data_DS_Melted_2Km_ind$country = 'IND'
data_DS_Melted_2Km_ind = subset(data_DS_Melted_2Km_ind,quarter %in% c('1','2'))
data_DS_Melted_2Km_ind$Sow_Date_MODIS[which(data_DS_Melted_2Km_ind$Sow_Date_MODIS > -10)] = NA
data_DS_Melted_2Km_ind$Sow_Date_MODIS[which(data_DS_Melted_2Km_ind$Sow_Date_MODIS < -150)] = NA
data_DS_Melted_2Km_ind$nquarters = 2
# Retrieve data for PHL ----


results_folder ="/home/lb/phenorice/PhenoRice/Processing/PHL/Outputs/Final/Validate/"
country_code = 'PHL'
out_RData_file = file.path(results_folder, 'Plots_Data.RData')
load(out_RData_file)
data_plot_dates_phl = data_plot_dates
data_plot_dates_phl$country = 'PHL'
data_DS_Melted_phl = data_DS_Melted_2Km
data_DS_Melted_phl$country = 'PHL - DS'
data_WS_Melted_phl = data_WS_Melted_2Km
data_WS_Melted_phl$country = 'PHL - WS'
data_DS_Melted_2Km_phl = data_DS_Melted_phl
data_DS_Melted_2Km_phl$country = 'PHL - DS'
data_WS_Melted_2Km_phl = data_WS_Melted_phl
data_WS_Melted_2Km_phl$country = 'PHL - WS'

data_DS_Melted_2Km_phl = droplevels(subset(data_DS_Melted_2Km_phl, quarter %in% c('1','2') ))
data_WS_Melted_2Km_phl = droplevels(subset(data_WS_Melted_2Km_phl, quarter %in% c('3','4') ))

data_DS_Melted_2Km_phl$Sow_Date_MODIS[which(data_DS_Melted_2Km_phl$Sow_Date_MODIS > 50)] = NA
data_DS_Melted_2Km_phl$Sow_Date_MODIS[which(data_DS_Melted_2Km_phl$Sow_Date_MODIS < -50)] = NA
data_DS_Melted_2Km_phl$Sow_Date_MODIS[which(data_DS_Melted_2Km_phl$Sow_Date_MODIS < -24)] = -24

data_WS_Melted_2Km_phl$Sow_Date_MODIS [which(data_WS_Melted_2Km_phl$Sow_Date_MODIS < 125)] = NA
data_WS_Melted_2Km_phl$Sow_Date_MODIS [which(data_WS_Melted_2Km_phl$Sow_Date_MODIS > 280)] = NA
data_WS_Melted_2Km_phl$Sow_Date_MODIS[which(data_WS_Melted_2Km_phl$Sow_Date_MODIS < 145)] = 145

if (only_nueva ==1) {data_DS_Melted_2Km_phl = droplevels(subset(data_DS_Melted_2Km_phl, NAME_1 == 'Nueva Ecija'))}
if (only_nueva ==1) {data_WS_Melted_2Km_phl = droplevels(subset(data_WS_Melted_2Km_phl, NAME_1 == 'Nueva Ecija'))}
# data_WS_Melted_2Km_phl = droplevels(subset(data_WS_Melted_2Km_phl, NAME_1 =='Nueva Ecija'))
# data_DS_Melted_2Km_phl = droplevels(subset(data_DS_Melted_2Km_phl, NAME_1 =='Nueva Ecija'))

data_DS_Melted_2Km_phl$nquarters = 2
data_WS_Melted_2Km_phl$nquarters = 2
#Retrieve data for Italy ----

results_folder = "/home/lb/phenorice/PhenoRice/Processing/ITA/Outputs/Final/Validate/"
country_code = 'ITA'
out_RData_file = file.path(results_folder, 'Plots_Data.RData')
load(out_RData_file)

data_DS_Melted_2Km_it = data_DS_Melted_2Km
data_DS_Melted_2Km_it$country = 'ITA'
data_DS_Melted_2Km_it = subset(data_DS_Melted_2Km_it,quarter==c('3'))
data_DS_Melted_2Km_it$Sow_Date_MODIS[which(data_DS_Melted_2Km_it$Sow_Date_MODIS < 0)] = NA
data_DS_Melted_2Km_it$Sow_Date_MODIS[which(data_DS_Melted_2Km_it$Sow_Date_MODIS > 365)] = NA
data_DS_Melted_2Km_it$nquarters = 1


# Put the  4 datasets together ----

data_DS_Melted_2Km_tot = rbindlist(list(data_DS_Melted_2Km_it,
                               data_DS_Melted_2Km_phl,
                               data_WS_Melted_2Km_phl,
                               data_DS_Melted_2Km_ind))

aggregate_n_seasons = data_DS_Melted_2Km_tot[, list(    # Find out which pixels are detected as biseasonal - needed to avoid counting
                                                n_valid = sum (is.finite(Sow_Date_MODIS)))                           # the area twice !!
                                         , by = c('country','Season','id')]
data_DS_Melted_2Km_tot = join(data_DS_Melted_2Km_tot,aggregate_n_seasons, by =  c('country','Season','id'), match = 'all')

aggregate_values_DS = data_DS_Melted_2Km_tot[, list(
  ref_ricearea_DS = sum(ref_ricearea_DS/nquarters, na.rm = T)/10000,			# cumulate area classified as rice
  tot_area = sum(tot_cellarea_DS/nquarters, na.rm = T)/10000,					# total non-nodata area
  refrice_fc = sum(ref_ricearea_DS, na.rm = T)/sum(tot_cellarea_DS, na.rm = T),					# total non-nodata area
  mod_ricearea = (sum((tot_cellarea_DS*is.finite(Sow_Date_MODIS)/n_valid), na.rm = T))/10000,	# area detected as rice in each of the 4 quarters
  mean_sow_mod = mean(Sow_Date_MODIS, na.rm = T),	# average doy of min in each of the 4 quarters
  mean_min_sar = mean(Sow_Date_SAR, na.rm = T))

  , by = c('country','Season','id_big')]


# data_DS_Melted_grd = data_DS_Melted_2Km_it


# Create and print plots of rice-detected areas MODIS vs SAR ----

data_country = droplevels(subset(aggregate_values_DS, country == 'PHL' & Season == 'WS'))
data_country = aggregate_values_DS

Regr_Stats =  ddply(data_country, c('country', 'Season'), function(df) regress_stats(df$ref_ricearea_DS, df$mod_ricearea ))
eqn_strings =  ddply(Regr_Stats, c('country', 'Season'), function(df) lm_eqn(df))
# data_country$gridder = factor(paste0(data_country$country, data_country$Season))
data_country$gridder = data_country$country
levels (data_country$gridder)=  c('IND - samba',"ITA","PHL - DS","PHL - WS")
# levels (data_country$gridder)= c('Tamil Nadu - samba',"Italy","Nueva Ecjia - Dry Season","Nueva Ecjia - Wet Season")
eqn_strings$gridder = as.factor(paste0(eqn_strings$country, eqn_strings$Season))
levels (eqn_strings$gridder)= levels (data_country$gridder)
Regr_Stats$gridder = as.factor(paste0(Regr_Stats$country, Regr_Stats$Season))
levels (Regr_Stats$gridder)= levels (data_country$gridder)

eqn_strings$gridder = factor(eqn_strings$gridder, levels = c("ITA", "IND - samba", "PHL - DS", "PHL - WS"), labels = c("ITA", "IND - samba", "PHL - Dry Season", "PHL - Wet Season"))
eqn_strings$country = eqn_strings$gridder


Regr_Stats$gridder = factor(Regr_Stats$gridder, levels = c("ITA", "IND", "PHL - DS", "PHL - WS"), labels = c("ITA", "IND - samba", "PHL - Dry Season", "PHL - Wet Season"))
Regr_Stats$country = Regr_Stats$gridder



data_country$country = as.factor(data_country$country)
# levels(data_country$country) = c("IND", "ITA", "PHL")
data_country$country = factor(data_country$country, levels = c("ITA", "IND", "PHL - DS", "PHL - WS"), labels = c("ITA", "IND - samba", "PHL - Dry Season", "PHL - Wet Season"))

data_country$country = as.factor(data_country$country)

# levels(data_country$country) = data_country$country = factor(data_country$country, levels = c("ITA", "IND", "PHL - DS", "PHL - WS"))


data_country$gridder = factor(data_country$gridder, levels = c("ITA", "IND - samba", "PHL - Dry Season", "PHL - Wet Season"))
max_used <- 450
p1 = ggplot(data = droplevels(data_country), aes(x = ref_ricearea_DS, y = mod_ricearea))
p1 = p1 + geom_point(alpha = 0.3)+theme_bw() +xlim(0,max_used) + ylim(0,max_used)
p1 = p1 + geom_abline(intercept = 0, slope = 1, linetype = 2)
p1 = p1 + geom_abline(aes(intercept = Intercept, slope = Slope),data = Regr_Stats,linetype = 4, color = 'red', size = 1.2)
p1 = p1 + geom_text(aes(x = 0, y = (max_used), label = eq), data = eqn_strings, parse = T, size = 4.0, hjust = 0)
p1 = p1 + xlab('Reference rice area [ha]')+ylab('PhenoRice detected rice area [ha]')
p1 = p1 + facet_wrap(~country, drop = TRUE)
p1 = p1 + ggtitle('PhenoRice vs. reference rice detected area - aggregation on 2 x 2 km grid')
p1 = p1 + theme(plot.title = element_text(vjust = 2) )
p1 = p1 + theme(axis.title = element_text(size = 14),
    plot.title = element_text(size = 15))
p1 = p1 + theme(axis.title = element_text(size = 13))+theme(strip.text.x = element_text(size = 13))

p_comparea_2km_DS  = p1 + coord_flip()


# Plots of accuracies and detection rates ----
only_nueva = 0
# Retrieve data for IND
results_folder = "/home/lb/phenorice/PhenoRice/Processing/IND/Outputs/Final/Validate/"
country_code = 'IND'
out_RData_file = file.path(results_folder, 'Plots_Data.RData')
load(out_RData_file)
acc_mat_areas_tot_DS_IND = acc_mat_areas_tot_DS
acc_mat_areas_tot_DS_IND$Country = 'IND'
acc_mat_areas_tot_DS_IND$Season  = 'DS'
acc_pix_all_tot_DS_IND = acc_pix_all_tot_DS
acc_pix_all_tot_DS_IND$Country = 'IND'
acc_pix_all_tot_DS_IND$Season  = 'DS'

# Retrieve data for PHL


results_folder ="/home/lb/phenorice/PhenoRice/Processing/PHL/Outputs/Final/Validate/"
country_code = 'PHL'
out_RData_file = file.path(results_folder, 'Plots_Data.RData')
load(out_RData_file)
acc_mat_areas_tot_DS_PHL = acc_mat_areas_tot_DS
acc_mat_areas_tot_DS_PHL$Country = 'PHL'
acc_mat_areas_tot_DS_PHL$Season  = 'DS'
acc_pix_all_tot_DS_PHL = acc_pix_all_tot_DS
acc_pix_all_tot_DS_PHL$Country = 'PHL'
acc_pix_all_tot_DS_PHL$Season  = 'DS'

acc_mat_areas_tot_WS_PHL = acc_mat_areas_tot_WS
acc_mat_areas_tot_WS_PHL$Country = 'PHL'
acc_mat_areas_tot_WS_PHL$Season  = 'WS'
acc_pix_all_tot_WS_PHL = acc_pix_all_tot_WS
acc_pix_all_tot_WS_PHL$Country = 'PHL'
acc_pix_all_tot_WS_PHL$Season  = 'WS'
if (only_nueva ==1) {acc_mat_areas_tot_DS_PHL = droplevels(subset(acc_mat_areas_tot_DS_PHL, Province == 'Nueva Ecija'))
                     acc_pix_all_tot_DS_PHL = droplevels(subset(acc_pix_all_tot_DS_PHL, NAME_1 == 'Nueva Ecija'))
                      acc_mat_areas_tot_DS_PHL$Province = 'All'
                      acc_pix_all_tot_DS_PHL$NAME_1 = 'All'}
if (only_nueva ==1) {acc_mat_areas_tot_WS_PHL = droplevels(subset(acc_mat_areas_tot_WS_PHL, Province == 'Nueva Ecija'))
                        acc_pix_all_tot_WS_PHL = droplevels(subset(acc_pix_all_tot_WS_PHL, NAME_1 == 'Nueva Ecija'))
                                    acc_mat_areas_tot_WS_PHL$Province = 'All'
                                    acc_pix_all_tot_WS_PHL$NAME_1 = 'All'}
# Retrieve data for PHL


results_folder = "/home/lb/phenorice/PhenoRice/Processing/ITA/Outputs/Final/Validate/"
country_code = 'ITA'
out_RData_file = file.path(results_folder, 'Plots_Data.RData')
load(out_RData_file)
acc_mat_areas_tot_DS_IT = acc_mat_areas_tot_DS
acc_mat_areas_tot_DS_IT$Country = 'ITA'
acc_mat_areas_tot_DS_IT$Season  = 'DS'
acc_pix_all_tot_DS_IT = acc_pix_all_tot_DS
acc_pix_all_tot_DS_IT$Country = 'ITA'
acc_pix_all_tot_DS_IT$Season  = 'DS'

# Bind the countries

acc_mat_areas_tot = rbindlist(list(acc_mat_areas_tot_DS_IT,acc_mat_areas_tot_DS_IND,acc_mat_areas_tot_DS_PHL,acc_mat_areas_tot_WS_PHL))
acc_pix_all_tot = rbindlist(list(acc_pix_all_tot_DS_IT,acc_pix_all_tot_DS_IND,acc_pix_all_tot_DS_PHL,acc_pix_all_tot_WS_PHL))

data_melted = melt(acc_mat_areas_tot)
data_melted$Season = as.factor(data_melted$Season)
data_melted$Province = as.factor(data_melted$Province)
data_melted = droplevels(subset(data_melted, (Province !='NA')))
area_data <- droplevels(subset(data_melted,(variable %in% names(acc_mat_areas_tot)[c(1:2,8:9)])))
names(area_data)[5] = 'Area'
area_data$Area = area_data$Area/10000
area_data$gridder = factor(paste0(area_data$Country, area_data$Season))
levels (area_data$gridder)= c('IND - samba',"ITA","PHL - Dry","PHL - Wet")
levels (area_data$variable)= c('Phenorice',"Reference")

# Plot Area Comparisons on all map
p = ggplot (droplevels(subset(area_data,Province =='All')),aes(x = gridder, y = Area, fill = variable)) + theme_bw()
p = p + geom_bar(stat = 'identity', position = position_dodge()) #+ facet_wrap(~Province)
p_area_comp_all_DS = p + ggtitle("PhenoRice vs Reference Area") + theme_bw() + scale_fill_grey('Area (ha)')# + facet_wrap(~gridder)

# Plot Area Comparisons  by Provinces
# p = ggplot (droplevels(subset(area_data,Province !='All')),aes(x = Season, y = Area, fill = variable)) + theme_bw()
# p = p + geom_bar(stat = 'identity', position = position_dodge()) + facet_wrap(~Country)
# p_area_comp_prov = p + ggtitle("MODIS vs Reference Area - by Provinces")

# Plot Accracies on all mop and then by province
acc_data <- droplevels(subset(data_melted,(variable %in% names(acc_mat_areas_tot)[c(3,6,7)])))
names(acc_data)[4] = 'Accuracy_Metric'

p = ggplot (droplevels(subset(acc_data,Province =='All')),aes(x = Season, y = value, fill = Accuracy_Metric)) + theme_bw()
p = p + geom_bar(stat = 'identity', position = 'dodge') + facet_wrap(~Country)
p = p + scale_fill_discrete('Accuracy Metric', labels = c('Overall Accuracy Map', 'Prod. Accuracy Rice', 'User Accuracy Rice'))
p_accuracy_DS = p+ theme(legend.justification=c(1,0), legend.position="top")+ ggtitle("Detection Accuracies (Computed on Areas)")

# acc_data2 = droplevels(subset(acc_data,(Province !='All' & variable  != 'OA')))
# p = ggplot (acc_data2,aes(x = Season, y = Accuracy_Metric, fill = variable)) + theme_bw()
# p = p + geom_bar(stat = 'identity', position = position_dodge(width = 1.0)) + facet_wrap(~Province)
# p = p + scale_fill_discrete('Accuracy_Metric', labels = c('Prod. Accuracy Rice', 'User Accuracy Rice'))
# p_accuracy_pro = p+ ggtitle("Detection Accuracies (Computed on Areas) - by Provinces")


# Plot detection rates -----
# Plot results
data_melted = melt(acc_pix_all_tot, id.vars = c(1,9,10,11,12))
plot_data <- droplevels(subset(data_melted,(variable %in% names(acc_pix_all_tot)[c(1,8,9,10,11,12)]) & NAME_2 =='All' ))
plot_data$gridder = factor(paste0(plot_data$Country, plot_data$Season))
levels (plot_data$gridder)= c('IND - samba',"ITA","PHL - Dry","PHL - Wet")
# Plot Accuracies on all mop
p = ggplot (droplevels(subset(plot_data, variable == 'Omission_Err_Rice')),aes(x = threshold, y = 100-value, pch = gridder, colour = gridder, group = gridder)) + theme_bw()
p = p + geom_point(size = 2)+geom_line(colour = 'grey60',linetype = 'dashed')
p = p +	scale_x_continuous('% of rice in MODIS pixels', limits = c(0,100), breaks = seq(0,100,10) )+
  scale_y_continuous('Detection rate (%)', limits = c(0,100),breaks = seq(0,100,10))+theme(plot.title = element_text(vjust = 1, hjust = 0))
p_detection_DS = p+ggtitle('Detection rate as a function of rice fractional cover in MODIS pixels') +
  scale_colour_discrete('Legend')+
  scale_shape_discrete('Legend')+
  theme(plot.title = element_text(hjust = 0.5), legend.justification=c(1,0), legend.position=c(1,0), legend.background = element_rect(colour = 'black'))+ theme(axis.title = element_text(size = 17),
                                                                                                                        axis.text.x = element_text(size = 12),
                                                                                                                        axis.text.y = element_text(size = 12),
                                                                                                                        plot.title = element_text(size = 15),
                                                                                                                        legend.text = element_text(size = 12),
                                                                                                                        legend.title = element_text(size = 12))

+facet_wrap(~gridder)






#End -----
# By provinces

# p = ggplot (droplevels(subset(plot_data,NAME_1 !='All')),aes(x = threshold, y = 100-value, pch = variable, colour = Season, group = interaction(Season,variable))) + theme_bw()
# p = p + geom_point()+geom_line(colour = 'black',linetype = 'dashed')+facet_wrap(~NAME_1)
# p = p +	scale_x_continuous('% of rice in MODIS pixel', limits = c(0,100), breaks = seq(0,100,10) )+
#   scale_y_continuous('Metric (%)', limits = c(0,100),breaks = seq(0,100,10))+theme(plot.title = element_text(vjust = 1, hjust = 0))
# p_detection_pro_DS = p+ggtitle('Omission and Commision (as % of pixels) as a function of rice fc in MODIS pixels - By Provinces')
#
#
#



# Subset the datasets if needed
# data_DS_Melted_2Km_ind$Sow_Date_MODIS[which(data_DS_Melted_2Km_ind$Sow_Date_MODIS > -50)] = NA
# data_DS_Melted_2Km_ind$Sow_Date_MODIS[which(data_DS_Melted_2Km_ind$Sow_Date_MODIS > 20)] = NA
# data_DS_Melted_2Km_ind$Sow_Date_MODIS[which(data_DS_Melted_2Km_ind$Sow_Date_MODIS < -145)] = NA

# Retrieve Data - PHL

results_folder ="/home/lb/phenorice/PhenoRice/Processing/PHL/Outputs/Final/Validate/"
country_code = 'PHL'
out_RData_file = file.path(results_folder, 'Plots_Data.RData')
load(out_RData_file)
data_plot_dates_phl = data_plot_dates
data_plot_dates_phl$country = 'PHL'
data_DS_Melted_phl = data_DS_Melted
data_DS_Melted_phl$country = 'PHL'
data_WS_Melted_phl = data_WS_Melted
data_WS_Melted_phl$country = 'PHL'
data_DS_Melted_2Km_phl = data_DS_Melted_2Km
data_DS_Melted_2Km_phl$country = 'PHL'
data_DS_Melted_5Km_phl = data_DS_Melted_2Km
data_DS_Melted_5Km_phl$country = 'PHL'
data_WS_Melted_2Km_phl = data_WS_Melted_2Km
data_WS_Melted_2Km_phl$country = 'PHL'
data_WS_Melted_5Km_phl = data_WS_Melted_2Km
data_WS_Melted_5Km_phl$country = 'PHL'


data_DS_Melted_2Km_phl = droplevels(subset(data_DS_Melted_2Km_phl, quarter %in% c('1','2') & n_seasons !=3))
data_WS_Melted_2Km_phl = droplevels(subset(data_WS_Melted_2Km_phl, quarter %in% c('3','4') & n_seasons != 5))

data_DS_Melted_5Km_phl = droplevels(subset(data_DS_Melted_5Km_phl, quarter %in% c('1','2') & n_seasons < check_seasons))
data_WS_Melted_5Km_phl = droplevels(subset(data_DS_Melted_5Km_phl, quarter %in% c('3','4') & n_seasons < check_seasons))

data_DS_Melted_2Km_phl$Sow_Date_MODIS[which(data_DS_Melted_2Km_phl$Sow_Date_MODIS > 50)] = NA
data_DS_Melted_2Km_phl$Sow_Date_MODIS[which(data_DS_Melted_2Km_phl$Sow_Date_MODIS < -50)] = NA
data_DS_Melted_2Km_phl$Sow_Date_MODIS[which(data_DS_Melted_2Km_phl$Sow_Date_MODIS < -24)] = -24
data_DS_Melted_2Km_phl$Sow_Date_MODIS [which(data_DS_Melted_2Km_phl$Sow_Date_MODIS < 125)] = NA
data_DS_Melted_2Km_phl$Sow_Date_MODIS [which(data_DS_Melted_2Km_phl$Sow_Date_MODIS > 220)] = NA
data_DS_Melted_2Km_phl$Sow_Date_MODIS[which(data_DS_Melted_2Km_phl$Sow_Date_MODIS < 145)] = 145
data_DS_Melted_2Km_phl = droplevels(subset(data_DS_Melted_2Km_phl, NAME_1 =='Nueva Ecija'))
data_WS_Melted_2Km_phl = droplevels(subset(data_WS_Melted_2Km_phl, NAME_1 =='Nueva Ecija'))


# Retrieve Data - IND
#
results_folder = "/home/lb/phenorice/PhenoRice/Processing/IND/Outputs/Final_Elab/newcomp/Validate/"
country_code = 'IND'
out_RData_file = file.path(results_folder, 'Plots_Data.RData')
load(out_RData_file)
data_plot_dates_ind = data_plot_dates
data_plot_dates_ind$country = 'IND'
data_DS_Melted_ind = data_DS_Melted
data_DS_Melted_ind$country = 'IND'
data_DS_Melted_2Km_ind = data_DS_Melted_2Km
data_DS_Melted_2Km_ind$country = 'IND'
data_DS_Melted_5Km_ind = data_DS_Melted_2Km
data_DS_Melted_5Km_ind$country = 'IND'


data_DS_Melted_2Km_ind = subset(data_DS_Melted_2Km_ind,quarter %in% c('1','2'))
data_DS_Melted_2Km_ind$Sow_Date_MODIS[which(data_DS_Melted_2Km_ind$n_seasons > check_seasons)] = NA





data_DS_Melted_grd = data_DS_Melted_2Km_phl
data_WS_Melted_grd = data_WS_Melted_2Km_phl
data_DS_Melted_grd$Sow_Date_MODIS[which(data_DS_Melted_grd$Sow_Date_MODIS > 50)] = NA
data_DS_Melted_grd$Sow_Date_MODIS[which(data_DS_Melted_grd$Sow_Date_MODIS < -50)] = NA
data_DS_Melted_grd$Sow_Date_MODIS[which(data_DS_Melted_grd$Sow_Date_MODIS < -24)] = -24
data_WS_Melted_grd$Sow_Date_MODIS [which(data_WS_Melted_grd$Sow_Date_MODIS < 125)] = NA
data_WS_Melted_grd$Sow_Date_MODIS [which(data_WS_Melted_grd$Sow_Date_MODIS > 220)] = NA
data_WS_Melted_grd$Sow_Date_MODIS[which(data_WS_Melted_grd$Sow_Date_MODIS < 145)] = 145

data_WS_Melted_grd$Sow_Date_SAR[which(data_WS_Melted_grd$Sow_Date_SAR < 155)] = NA

aggregate_n_seasons = data_DS_Melted_grd[, list(aggregate_n_seasons    # Find out which pixels are detected as biseasonal - needed to avoid counting
                                                n_valid = sum (is.finite(Sow_Date_MODIS)))                           # the area twice !!

                                         , by = id]

data_WS_Melted_grd = join(data_DS_Melted_grd,aggregate_n_seasons, by = 'id', match = 'all')

aggregate_values_DS = data_DS_Melted_grd[, list(
  ref_ricearea_DS = sum(ref_ricearea_DS/2, na.rm = T)/10000,			# cumulate area classified as rice
  tot_area = sum(tot_cellarea_DS/2, na.rm = T)/10000,					# total non-nodata area
  refrice_fc = sum(ref_ricearea_DS, na.rm = T)/sum(tot_cellarea_DS, na.rm = T),					# total non-nodata area
  mod_ricearea = sum((tot_cellarea_DS*is.finite(Sow_Date_MODIS)), na.rm = T)/10000,	# area detected as rice in each of the 4 quarters
  mean_sow_mod = mean(Sow_Date_MODIS, na.rm = T),	# average doy of min in each of the 4 quarters
  mean_min_sar = mean(Sow_Date_SAR, na.rm = T))

  , by = id_big]

# Create and print plots of rice-detected areas MODIS vs SAR

x = aggregate_values_DS$ref_ricearea_DS
y = aggregate_values_DS$mod_ricearea
Regr_Stats =  regress_stats( x, y )
eqn_strings = lm_eqn(Regr_Stats)
ref_rice_Area = 'ref_rice_Area'

temp_df = data.frame(x = x, y = y)
#	eqn_strings$range_max = range_max
max_x = max(x,y)
max_y = max(x,y)
max_used = max(max_x, max_y)
eqn_strings$max_used = max_used

p1 = ggplot(data = temp_df, aes(x = x, y = y))
p1 = p1 + geom_point(alpha = 0.3)+theme_bw() +xlim(0,max_used)+ylim(0,max_used)
p1 = p1 + geom_abline(intercept = 0, slope = 1, linetype = 2)
p1 = p1 + geom_abline(aes(intercept = Intercept, slope = Slope),data = Regr_Stats,linetype = 4, color = 'red', size = 0.9)
p1 = p1 + geom_text(aes(x = 0, y = (max_used), label = eq), data = eqn_strings, parse = T, size = 4, hjust = 0, face = 'bold')
p1 = p1 + xlab('Reference Rice Area [ha]')+ylab('MODIS detected rice area [ha]')
p1 = p1 + ggtitle(paste0(grid_dim,' Grid'))
p_comparea_2km_DS = p1

aggregate_n_seasons = data_WS_Melted_grd[, list(aggregate_n_seasons    # Find out which pixels are detected as biseasonal - needed to avoid counting
  n_valid = sum (is.finite(Sow_Date_MODIS)))                           # the area twice !!

  , by = id]

data_WS_Melted_grd = join(data_WS_Melted_grd,aggregate_n_seasons, by = 'id', match = 'all')

aggregate_values_WS = data_WS_Melted_grd[, list(
  ref_ricearea_DS = sum(ref_ricearea_DS/2, na.rm = T)/10000,			# cumulate area classified as rice
  tot_area = sum(tot_cellarea_DS/2, na.rm = T)/10000,					# total non-nodata area
  refrice_fc = sum(ref_ricearea_DS, na.rm = T)/sum(tot_cellarea_DS, na.rm = T),					# total non-nodata area
  mod_ricearea = (sum((tot_cellarea_DS*is.finite(Sow_Date_MODIS)/n_valid), na.rm = T))/10000,	# area detected as rice in each of the 4 quarters
  mean_sow_mod = mean(Sow_Date_MODIS, na.rm = T),	# average doy of min in each of the 4 quarters
  mean_min_sar = mean(Sow_Date_SAR, na.rm = T))

  , by = id_big]

# Create and print plots of rice-detected areas MODIS vs SAR

x = aggregate_values_WS$ref_ricearea_DS
y = aggregate_values_WS$mod_ricearea
Regr_Stats =  regress_stats( x, y )
eqn_strings = lm_eqn(Regr_Stats)
ref_rice_Area = 'ref_rice_Area'

temp_df = data.frame(x = x, y = y)
#	eqn_strings$range_max = range_max
max_x = max(x,y)
max_y = max(x,y)
max_used = max(max_x, max_y)
eqn_strings$max_used = max_used

p1 = ggplot(data = temp_df, aes(x = x, y = y))
p1 = p1 + geom_point(alpha = 0.3)+theme_bw() +xlim(0,max_used)+ylim(0,max_used)
p1 = p1 + geom_abline(intercept = 0, slope = 1, linetype = 2)
p1 = p1 + geom_abline(aes(intercept = Intercept, slope = Slope),data = Regr_Stats,linetype = 4, color = 'red', size = 0.9)
p1 = p1 + geom_text(aes(x = 0, y = (max_used), label = eq), data = eqn_strings, parse = T, size = 4, hjust = 0, face = 'bold')
p1 = p1 + xlab('Reference Rice Area [ha]')+ylab('MODIS detected rice area [ha]')
p1 = p1 + ggtitle(paste0('PHL - Wet Season ', grid_dim,' Grid'))
p_comparea_2km_WS = p1



#
# data_DS_Melted_2Km_phl$Sow_Date_MODIS[which(data_DS_Melted_2Km_phl$n_seasons != 2)] = NA
# data_WS_Melted_2Km_phl$Sow_Date_MODIS[which(data_WS_Melted_2Km_phl$n_seasons != 2)] = NA

# data_DS_Melted_2Km_phl$Sow_Date_MODIS[which(data_DS_Melted_2Km_phl$Sow_Date_MODIS > 50)] = NA
# data_DS_Melted_2Km_phl$Sow_Date_MODIS[which(data_DS_Melted_2Km_phl$Sow_Date_MODIS < -50)] = NA
# # data_DS_Melted_2Km_phl$Sow_Date_MODIS[which(data_DS_Melted_2Km_phl$Sow_Date_MODIS < -24)] = -24
# data_WS_Melted_2Km_phl$Sow_Date_MODIS [which(data_WS_Melted_2Km_phl$Sow_Date_MODIS < 150)] = NA
# data_WS_Melted_2Km_phl$Sow_Date_MODIS [which(data_WS_Melted_2Km_phl$Sow_Date_MODIS > 220)] = NA
# # data_DS_Melted_2Km_phl$Sow_Date_MODIS[which(data_DS_Melted_2Km_phl$Sow_Date_MODIS < 145)] = 145

data_WS_Melted_2Km_phl$Sow_Date_SAR[which(data_WS_Melted_2Km_phl$Sow_Date_SAR < 155)] = NA

 data_DS_Melted_2Km_phl = droplevels(subset(data_DS_Melted_2Km_phl, NAME_1 =='Nueva Ecija'))
 data_WS_Melted_2Km_phl = droplevels(subset(data_WS_Melted_2Km_phl, NAME_1 =='Nueva Ecija'))

data_melted_2km = rbind(data_DS_Melted_2Km_phl,data_WS_Melted_2Km_phl,data_DS_Melted_2Km_ind,data_DS_Melted_2Km_it)

data_melted_2km = droplevels(subset(data_melted_2km, ref_rice_fc> 0.75))
aggregate_values = data_melted_2km[, list(
  ref_ricearea_DS = sum(ref_ricearea_DS/4, na.rm = T)/10000,			# cumulate area classified as rice
  tot_area = sum(tot_cellarea_DS/4, na.rm = T)/10000,					# total non-nodata area
  refrice_fc = sum(ref_ricearea_DS, na.rm = T)/sum(tot_cellarea_DS, na.rm = T),					# total non-nodata area
  mod_ricearea = sum((tot_cellarea_DS*is.finite(Sow_Date_MODIS)), na.rm = T)/10000,	# area detected as rice in each of the 4 quarters
  mean_sow_mod = mean(Sow_Date_MODIS, na.rm = T),	# average doy of min in each of the 4 quarters
  mean_min_sar = mean(Sow_Date_SAR, na.rm = T), country = country[1],
  std_sow_mod = sd(Sow_Date_MODIS, na.rm = T),
  std_sow_sar = sd(Sow_Date_SAR, na.rm = T))

  , by =keycols]

summary(aggregate_values$std_sow_mod)
summary(aggregate_values$std_sow_sar)

aggregate_values_sub = subset(aggregate_values, std_sow_sar < 100 & std_sow_mod < 200 )

Regr_Stats_DS =  regress_stats(aggregate_values_sub$mean_sow_mod, aggregate_values_sub$mean_min_sar )
eqn_strings_WS = lm_eqn(Regr_Stats_DS)

area_limit = 0.75
subdata = subset(aggregate_values_sub, refrice_fc > area_limit)
x = subdata$mean_min_sar
y = subdata$mean_sow_mod
z = subdata$country
w = subdata$Season

Regr_Stats =  regress_stats( x, y )
eqn_strings = lm_eqn(Regr_Stats)
ref_rice_Area = 'ref_rice_Area'

temp_df = data.frame(x = x, y = y, z = z, w = w)
#	eqn_strings$range_max = range_max
max_x = max(x,y, na.rm = T)
max_y = max(x,y, na.rm = T)
max_used = max(max_x, max_y)
eqn_strings$max_used = max_used
eqn_strings$country = 'max_used'

p1 = ggplot(data = aggregate_values, aes(x = mean_min_sar, y = mean_sow_mod, color = country))
p1 = p1 + geom_point(alpha = 0.3)+theme_bw()# +xlim(100,180)+ylim(100,180)
p1 = p1 + geom_abline(intercept = 0, slope = 1, linetype = 2)
p1 = p1 + geom_abline(aes(intercept = Intercept, slope = Slope),data = Regr_Stats,linetype = 4, color = 'red', size = 0.75)
p1 = p1 + geom_text(aes(x = -100, y = (200), label = eq), data = eqn_strings, parse = T, size = 4, hjust = 0, face = 'bold')
p1 = p1 + xlab('Reference Sowing Date [DOY]')+ylab('MODIS detected Sowing Date [DOY]')
p1 = p1 + ggtitle(paste0('2Km',' Grid'))
p_compsow_2km_DS = p1

sub_it = droplevels(subset(aggregate_values, country =='ITA'))
Regr_Stats_it_ds =  regress_stats(sub_it$mean_min_sar, sub_it$mean_sow_mod )
Regr_Stats_it_ds$country = 'ITA'
Regr_Stats_it_ds$Season  = 'DS'

sub_ind = droplevels(subset(aggregate_values, country =='IND'))
Regr_Stats_ind_ds =  regress_stats(sub_ind$mean_min_sar, sub_ind$mean_sow_mod)
Regr_Stats_ind_ds$country = 'IND'
Regr_Stats_ind_ds$Season  = 'DS'

sub_phl_DS = droplevels(subset(aggregate_values, country =='PHL' & Season == 'DS'))
Regr_Stats_phl_ds =  regress_stats(sub_phl_DS$mean_min_sar, sub_phl_DS$mean_sow_mod )

sub_phl_WS = droplevels(subset(aggregate_values, country =='PHL' & Season == 'WS'))
Regr_Stats_phl_ws =  regress_stats(sub_phl_WS$mean_min_sar, sub_phl_WS$mean_sow_mod )

Regr_Stats_phl_ds$country = 'PHL'
Regr_Stats_phl_ds$Season  = 'DS'

Regr_Stats_phl_ws$country = 'PHL'
Regr_Stats_phl_ws$Season  = 'WS'

Regr_Stats$country ='All'
Regr_Stats$Season ='All'

regr_stats_all=rbind(Regr_Stats,Regr_Stats_it_ds,Regr_Stats_ind_ds,Regr_Stats_phl_ds,Regr_Stats_phl_ws)

qplot(data = aggregate_values, mean_min_sar,mean_sow_mod, color = country)+theme_bw()

aggregate_values_WS = data_WS_Melted_grd[, list(
  ref_ricearea_DS = sum(ref_ricearea_DS/4, na.rm = T)/10000,			# cumulate area classified as rice
  tot_area = sum(tot_cellarea_DS/4, na.rm = T)/10000,					# total non-nodata area
  refrice_fc = sum(ref_ricearea_DS, na.rm = T)/sum(tot_cellarea_DS, na.rm = T),					# total non-nodata area
  mod_ricearea = sum((tot_cellarea_DS*is.finite(Sow_Date_MODIS)), na.rm = T)/10000,	# area detected as rice in each of the 4 quarters
  mean_sow_mod = mean(Sow_Date_MODIS, na.rm = T),	# average doy of min in each of the 4 quarters
  mean_min_sar = mean(Sow_Date_SAR, na.rm = T))

  , by = id_big]



data_DS_Melted_5Km_it = subset(data_DS_Melted_5Km_it,ref_rice_fc > 0.75 & quarter %in% c('3'))
data_DS_Melted_5Km_it$Sow_Date_MODIS[which(data_DS_Melted_5Km_it$Sow_Date_MODIS < 80)] = NA
data_DS_Melted_5Km_it$Sow_Date_MODIS[which(data_DS_Melted_5Km_it$Sow_Date_MODIS > 180)] = NA
data_DS_Melted_5Km_phl = droplevels(subset(data_DS_Melted_5Km_phl,ref_rice_fc >0.75 & quarter %in% c('1','2')))
data_WS_Melted_5Km_phl = droplevels(subset(data_WS_Melted_5Km_phl,ref_rice_fc > 0.75  & quarter %in% c('3','4')))

data_DS_Melted_5Km_phl = droplevels(subset(data_DS_Melted_5Km_phl,ref_rice_fc >0.75 & quarter %in% c('1','2')))
data_WS_Melted_5Km_phl = droplevels(subset(data_DS_Melted_5Km_phl,ref_rice_fc >0.75  & quarter %in% c('3','4')))

data_DS_Melted_5Km_phl$Sow_Date_MODIS[which(data_DS_Melted_5Km_phl$Sow_Date_MODIS > 50)] = NA
data_DS_Melted_5Km_phl$Sow_Date_MODIS[which(data_DS_Melted_5Km_phl$Sow_Date_MODIS < -50)] = NA
data_DS_Melted_5Km_phl$Sow_Date_MODIS[which(data_DS_Melted_5Km_phl$Sow_Date_MODIS < -24)] = -24
data_WS_Melted_5Km_phl$Sow_Date_MODIS [which(data_WS_Melted_5Km_phl$Sow_Date_MODIS < 150)] = NA
data_WS_Melted_5Km_phl$Sow_Date_MODIS [which(data_WS_Melted_5Km_phl$Sow_Date_MODIS > 220)] = NA
# data_DS_Melted_5Km_phl$Sow_Date_MODIS[which(data_DS_Melted_5Km_phl$Sow_Date_MODIS < 145)] = 145

data_WS_Melted_5Km_phl$Sow_Date_SAR[which(data_WS_Melted_5Km_phl$Sow_Date_SAR < 155)] = NA

data_DS_Melted_5Km_phl = droplevels(subset(data_DS_Melted_5Km_phl, NAME_1 =='Nueva Ecija'))

data_DS_Melted_5Km_it = subset(data_DS_Melted_5Km_it,ref_rice_fc > 0.75 & quarter %in% c('3'))
data_DS_Melted_5Km_it$Sow_Date_MODIS[which(data_DS_Melted_5Km_it$Sow_Date_MODIS < 80)] = NA
data_DS_Melted_5Km_it$Sow_Date_MODIS[which(data_DS_Melted_5Km_it$Sow_Date_MODIS > 180)] = NA

data_melted_5Km = rbind(data_DS_Melted_5Km_phl,data_WS_Melted_5Km_phl,data_DS_Melted_5Km_ind,data_DS_Melted_5Km_it)

data_melted_5Km = droplevels(subset(data_melted_5Km, ref_rice_fc> 0.75))
aggregate_values = data_melted_5Km[, list(
  ref_ricearea_DS = sum(ref_ricearea_DS/4, na.rm = T)/10000,			# cumulate area classified as rice
  tot_area = sum(tot_cellarea_DS/4, na.rm = T)/10000,					# total non-nodata area
  refrice_fc = sum(ref_ricearea_DS, na.rm = T)/sum(tot_cellarea_DS, na.rm = T),					# total non-nodata area
  mod_ricearea = sum((tot_cellarea_DS*is.finite(Sow_Date_MODIS)), na.rm = T)/10000,	# area detected as rice in each of the 4 quarters
  mean_sow_mod = mean(Sow_Date_MODIS, na.rm = T),	# average doy of min in each of the 4 quarters
  mean_min_sar = mean(Sow_Date_SAR, na.rm = T), country = country[1])

  , by =keycols]

Regr_Stats_DS =  regress_stats(aggregate_values$mean_sow_mod, aggregate_values$mean_min_sar )
eqn_strings_WS = lm_eqn(Regr_Stats_DS)

area_limit = 0.75
subdata = subset(aggregate_values, refrice_fc > area_limit)
x = subdata$mean_min_sar
y = subdata$mean_sow_mod
z = subdata$country
w = subdata$Season

Regr_Stats =  regress_stats( x, y )
eqn_strings = lm_eqn(Regr_Stats)
ref_rice_Area = 'ref_rice_Area'

temp_df = data.frame(x = x, y = y, z = z, w = w)
#	eqn_strings$range_max = range_max
max_x = max(x,y, na.rm = T)
max_y = max(x,y, na.rm = T)
max_used = max(max_x, max_y)
eqn_strings$max_used = max_used
eqn_strings$country = 'max_used'

p1 = ggplot(data = aggregate_values, aes(x = mean_min_sar, y = mean_sow_mod, color = country))
p1 = p1 + geom_point(alpha = 0.3)+theme_bw()# +xlim(100,180)+ylim(100,180)
p1 = p1 + geom_abline(intercept = 0, slope = 1, linetype = 2)
p1 = p1 + geom_abline(aes(intercept = Intercept, slope = Slope),data = Regr_Stats,linetype = 4, color = 'red', size = 0.75)
p1 = p1 + geom_text(aes(x = -100, y = (200), label = eq), data = eqn_strings, parse = T, size = 4, hjust = 0, face = 'bold')
p1 = p1 + xlab('Reference Sowing Date [DOY]')+ylab('MODIS detected Sowing Date [DOY]')
p1 = p1 + ggtitle(paste0('5Km',' Grid'))
p_compsow_5Km_DS = p1




results_folder = "/home/lb/phenorice/PhenoRice/Processing/IT/Outputs/Validate/sos/subset"
country_code = 'ITA'
out_RData_file = file.path(results_folder, 'Plots_Data_sow.RData')
load(out_RData_file)

data_DS_Melted_2Km_it = data_DS_Melted_2Km
data_DS_Melted_2Km_it$country = 'ITA'
# data_DS_Melted_5Km_it = data_DS_Melted_2Km
# data_DS_Melted_5Km_it$country = 'IT'
# data_DS_Melted_2Km_it$Sow_Date_SAR = data_DS_Melted_2Km_it$Sow_Date_SAR-10

data_DS_Melted_2Km_it = subset(data_DS_Melted_2Km_it,quarter==c('3'))

grid_dim = '2Km'
data_DS_Melted_grd = data_DS_Melted_2Km_it  #get((paste0('data_DS_Melted_',grid_dim)))
# data_DS_Melted_grd = subset(data_DS_Melted_grd, !is.na(Sow_Date_SAR ))
# data_DS_Melted_grd = subset(data_DS_Melted_grd, ref_rice_fc > 0.1)

# Subset the datasets if needed
#data_DS_Melted_grd$Sow_Date_MODIS[which(data_DS_Melted_grd$Sow_Date_MODIS > 0)] = NA
data_DS_Melted_grd$Sow_Date_MODIS[which(data_DS_Melted_grd$Sow_Date_MODIS < 80)] = NA
data_DS_Melted_grd$Sow_Date_MODIS[which(data_DS_Melted_grd$Sow_Date_MODIS > 180)] = NA

# Tabulate areas to compute cumulated/averaged values over the cells of the lower resolution grids
aggregate_values_DS = data_DS_Melted_grd[, list(
  ref_ricearea_DS = sum(ref_ricearea_DS, na.rm = T)/10000,			# cumulate area classified as rice
  tot_area = sum(tot_cellarea_DS, na.rm = T)/10000,					# total non-nodata area
  refrice_fc = sum(ref_ricearea_DS, na.rm = T)/sum(tot_cellarea_DS, na.rm = T),					# total non-nodata area
  mod_ricearea = sum((tot_cellarea_DS*is.finite(Sow_Date_MODIS)), na.rm = T)/10000,	# area detected as rice in each of the 4 quarters
  mean_sow_mod = mean(Sow_Date_MODIS, na.rm = T),	# average doy of min in each of the 4 quarters
  mean_min_sar = mean(Sow_Date_SAR, na.rm = T))

  , by = id_big]

# Create and print plots of rice-detected areas MODIS vs SAR

x = aggregate_values_DS$ref_ricearea_DS
y = aggregate_values_DS$mod_ricearea
Regr_Stats =  regress_stats( x, y )
eqn_strings = lm_eqn(Regr_Stats)
ref_rice_Area = 'ref_rice_Area'

temp_df = data.frame(x = x, y = y)
#	eqn_strings$range_max = range_max
max_x = max(x,y)
max_y = max(x,y)
max_used = max(max_x, max_y)
eqn_strings$max_used = max_used

p1 = ggplot(data = temp_df, aes(x = x, y = y))
p1 = p1 + geom_point(alpha = 0.3)+theme_bw() +xlim(0,max_used)+ylim(0,max_used)
p1 = p1 + geom_abline(intercept = 0, slope = 1, linetype = 2)
p1 = p1 + geom_abline(aes(intercept = Intercept, slope = Slope),data = Regr_Stats,linetype = 4, color = 'red', size = 0.9)
p1 = p1 + geom_text(aes(x = 0, y = (max_used), label = eq), data = eqn_strings, parse = T, size = 4, hjust = 0, face = 'bold')
p1 = p1 + xlab('Reference Rice Area [ha]')+ylab('MODIS detected rice area [ha]')
p1 = p1 + ggtitle(paste0(grid_dim,' Grid'))
p_comparea_2km_DS = p1

