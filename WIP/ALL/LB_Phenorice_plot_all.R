check_seasons = 2

results_folder = "D:/Temp/PhenoRice/Processing/IT/Outputs/Validate/sos"
country_code = 'ITA'
out_RData_file = file.path(results_folder, 'Plots_Data_sow.RData')
load(out_RData_file)
data_plot_dates_it = data_plot_dates
data_plot_dates_it$country = 'IT'
data_DS_Melted_it = data_DS_Melted
data_DS_Melted_it$country = 'IT'
data_DS_Melted_2Km_it = data_DS_Melted_2Km
data_DS_Melted_2Km_it$country = 'IT'
data_DS_Melted_5Km_it = data_DS_Melted_2Km
data_DS_Melted_5Km_it$country = 'IT'
data_DS_Melted_2Km_it$Sow_Date_SAR = data_DS_Melted_2Km_it$Sow_Date_SAR-10

data_DS_Melted_2Km_it = subset(data_DS_Melted_2Km_it,ref_rice_fc > 0.75 & quarter %in% c('3'))
# data_DS_Melted_2Km_it$Sow_Date_MODIS[which(data_DS_Melted_2Km_it$Sow_Date_MODIS < 80)] = NA
# data_DS_Melted_2Km_it$Sow_Date_MODIS[which(data_DS_Melted_2Km_it$Sow_Date_MODIS > 180)] = NA

results_folder = "D:/Temp/PhenoRice/Processing/IND/Outputs/Final_Elab/Validate/"
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


data_DS_Melted_2Km_ind = subset(data_DS_Melted_2Km_ind,ref_rice_fc > 0.75 & quarter %in% c('1','2'))
data_DS_Melted_2Km_ind$Sow_Date_MODIS[which(data_DS_Melted_2Km_ind$n_seasons > check_seasons)] = NA
# Subset the datasets if needed
# data_DS_Melted_2Km_ind$Sow_Date_MODIS[which(data_DS_Melted_2Km_ind$Sow_Date_MODIS > -50)] = NA
# data_DS_Melted_2Km_ind$Sow_Date_MODIS[which(data_DS_Melted_2Km_ind$Sow_Date_MODIS > 20)] = NA
# data_DS_Melted_2Km_ind$Sow_Date_MODIS[which(data_DS_Melted_2Km_ind$Sow_Date_MODIS < -145)] = NA

results_folder ="D:/Temp/PhenoRice/Processing/PHL/Outputs/new_elab/16days_lst/Validate/"
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


data_DS_Melted_2Km_phl = droplevels(subset(data_DS_Melted_2Km_phl,ref_rice_fc >0.75 & quarter %in% c('1','2') & n_seasons != 5))
data_WS_Melted_2Km_phl = droplevels(subset(data_WS_Melted_2Km_phl,ref_rice_fc > 0.75  & quarter %in% c('3','4') & n_seasons != 5))

data_DS_Melted_5Km_phl = droplevels(subset(data_DS_Melted_5Km_phl,ref_rice_fc >0.75 & quarter %in% c('1','2') & n_seasons < check_seasons))
data_WS_Melted_5Km_phl = droplevels(subset(data_DS_Melted_5Km_phl,ref_rice_fc >0.75  & quarter %in% c('3','4') & n_seasons < check_seasons))


data_DS_Melted_2Km_phl$Sow_Date_MODIS[which(data_DS_Melted_2Km_phl$n_seasons != 2)] = NA
data_WS_Melted_2Km_phl$Sow_Date_MODIS[which(data_WS_Melted_2Km_phl$n_seasons != 2)] = NA

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

sub_it = droplevels(subset(aggregate_values, country =='IT'))
Regr_Stats_it_ds =  regress_stats(sub_it$mean_min_sar, sub_it$mean_sow_mod )
Regr_Stats_it_ds$country = 'IT'
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
