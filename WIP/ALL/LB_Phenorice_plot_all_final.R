lm_eqn = function(df){

  eq <- substitute(italic(y) == a + b %.% italic(x)*""~~italic('r')~"="~r~~italic('ME')~"="~ME~~italic('MAE')~"="~MAE,
                   list(a = format(df$Intercept, digits = 3),
                        b = format(df$Slope, digits = 3),
                        r = format(df$r, digits = 3),
                        ME = format(df$ME, digits = 3),
                        MAE = format(df$MAE, digits = 3)#,
                        # RMSE= format(df$RMSE, digits = 3)
                   ))
  as.character(as.expression(eq))
  out = data.frame(eq = as.character(as.expression(eq)))
  out
}

regress_stats = function( x= x, y = y) {

  # 	sub_y = which(y > 0 & x > 0)
  # 	y = y[sub_y]  		; 			x = x[sub_y]

  if (length(y[is.finite(y)])>10 & length(x[is.finite(x)] )>10) {
    browser()
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


# Plotting of Sowing Dates Comparison ----

check_seasons = 3
only_nueva =  0
results_folder = "/home/lb/phenorice/PhenoRice/Processing/IT/Outputs/Validate/sos"
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
data_DS_Melted_2Km_it$Sow_Date_MODIS_dd = doytodate(data_DS_Melted_2Km_it$Sow_Date_MODIS,2013)
data_DS_Melted_2Km_it$Sow_Date_SAR_dd = doytodate(data_DS_Melted_2Km_it$Sow_Date_SAR,2013)
 # data_DS_Melted_2Km_it$Sow_Date_MODIS[which(data_DS_Melted_2Km_it$Sow_Date_MODIS < 80)] = NA
 # data_DS_Melted_2Km_it$Sow_Date_MODIS[which(data_DS_Melted_2Km_it$Sow_Date_MODIS > 180)] = NA

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
data_DS_Melted_5Km_ind = data_DS_Melted_2Km
data_DS_Melted_5Km_ind$country = 'IND'


data_DS_Melted_2Km_ind = subset(data_DS_Melted_2Km_ind,ref_rice_fc > 0.75 & quarter %in% c('1','2'))
data_DS_Melted_2Km_ind$Sow_Date_MODIS[which(data_DS_Melted_2Km_ind$n_seasons > check_seasons)] = NA
data_DS_Melted_2Km_ind$Sow_Date_MODIS[which(data_DS_Melted_2Km_ind$Sow_Date_MODIS > -10)] = NA
data_DS_Melted_2Km_ind$Sow_Date_MODIS[which(data_DS_Melted_2Km_ind$Sow_Date_MODIS < -150)] = NA
data_DS_Melted_2Km_ind$Sow_Date_MODIS_dd = doytodate(data_DS_Melted_2Km_ind$Sow_Date_MODIS,2014)
data_DS_Melted_2Km_ind$Sow_Date_SAR_dd = doytodate(data_DS_Melted_2Km_ind$Sow_Date_SAR,2014)
# Subset the datasets if needed
# data_DS_Melted_2Km_ind$Sow_Date_MODIS[which(data_DS_Melted_2Km_ind$Sow_Date_MODIS > -50)] = NA
# data_DS_Melted_2Km_ind$Sow_Date_MODIS[which(data_DS_Melted_2Km_ind$Sow_Date_MODIS > 20)] = NA
# data_DS_Melted_2Km_ind$Sow_Date_MODIS[which(data_DS_Melted_2Km_ind$Sow_Date_MODIS < -145)] = NA

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


data_DS_Melted_2Km_phl = droplevels(subset(data_DS_Melted_2Km_phl,ref_rice_fc >0.75 & quarter %in% c('1','2') & n_seasons != 5))
data_WS_Melted_2Km_phl = droplevels(subset(data_WS_Melted_2Km_phl,ref_rice_fc > 0.75  & quarter %in% c('3','4') & n_seasons != 5))

data_DS_Melted_5Km_phl = droplevels(subset(data_DS_Melted_5Km_phl,ref_rice_fc >0.75 & quarter %in% c('1','2') & n_seasons < check_seasons))
data_WS_Melted_5Km_phl = droplevels(subset(data_DS_Melted_5Km_phl,ref_rice_fc >0.75  & quarter %in% c('3','4') & n_seasons < check_seasons))

data_DS_Melted_2Km_phl$Sow_Date_MODIS[which(data_DS_Melted_2Km_phl$Sow_Date_MODIS < -50)] = NA
data_DS_Melted_2Km_phl$Sow_Date_MODIS[which(data_DS_Melted_2Km_phl$Sow_Date_MODIS >  50)] = NA
# data_DS_Melted_2Km_phl$Sow_Date_MODIS[which(data_DS_Melted_2Km_phl$Sow_Date_MODIS < -24)] = -24

data_WS_Melted_2Km_phl$Sow_Date_MODIS [which(data_WS_Melted_2Km_phl$Sow_Date_MODIS < 125)] = NA
data_WS_Melted_2Km_phl$Sow_Date_MODIS [which(data_WS_Melted_2Km_phl$Sow_Date_MODIS > 225)] = NA
# # data_WS_Melted_2Km_phl$Sow_Date_MODIS[which(data_WS_Melted_2Km_phl$Sow_Date_MODIS < 145)] = 145


# data_DS_Melted_2Km_phl$Sow_Date_MODIS[which(data_DS_Melted_2Km_phl$n_seasons != 2)] = NA
# data_WS_Melted_2Km_phl$Sow_Date_MODIS[which(data_WS_Melted_2Km_phl$n_seasons != 2)] = NA

# data_DS_Melted_2Km_phl$Sow_Date_MODIS[which(data_DS_Melted_2Km_phl$Sow_Date_MODIS > 50)] = NA
# data_DS_Melted_2Km_phl$Sow_Date_MODIS[which(data_DS_Melted_2Km_phl$Sow_Date_MODIS < -50)] = NA
# # data_DS_Melted_2Km_phl$Sow_Date_MODIS[which(data_DS_Melted_2Km_phl$Sow_Date_MODIS < -24)] = -24
# data_WS_Melted_2Km_phl$Sow_Date_MODIS [which(data_WS_Melted_2Km_phl$Sow_Date_MODIS < 150)] = NA
# data_WS_Melted_2Km_phl$Sow_Date_MODIS [which(data_WS_Melted_2Km_phl$Sow_Date_MODIS > 220)] = NA
# # data_DS_Melted_2Km_phl$Sow_Date_MODIS[which(data_DS_Melted_2Km_phl$Sow_Date_MODIS < 145)] = 145

data_WS_Melted_2Km_phl$Sow_Date_SAR[which(data_WS_Melted_2Km_phl$Sow_Date_SAR < 155)] = NA
#

if (only_nueva ==1) {data_DS_Melted_2Km_phl = droplevels(subset(data_DS_Melted_2Km_phl, NAME_1 == 'Nueva Ecija'))}
if (only_nueva ==1) {data_WS_Melted_2Km_phl = droplevels(subset(data_WS_Melted_2Km_phl, NAME_1 == 'Nueva Ecija'))}

data_DS_Melted_2Km_phl$Sow_Date_MODIS_dd = doytodate(data_DS_Melted_2Km_phl$Sow_Date_MODIS,2013)
data_WS_Melted_2Km_phl$Sow_Date_MODIS_dd = doytodate(data_WS_Melted_2Km_phl$Sow_Date_MODIS,2013)

data_DS_Melted_2Km_phl$Sow_Date_SAR_dd = doytodate(data_DS_Melted_2Km_phl$Sow_Date_SAR,2013)
data_WS_Melted_2Km_phl$Sow_Date_SAR_dd = doytodate(data_WS_Melted_2Km_phl$Sow_Date_SAR,2013)
#
# data_DS_Melted_2Km_phl = droplevels(subset(data_DS_Melted_2Km_phl, NAME_1 =='Nueva Ecija'))
# data_WS_Melted_2Km_phl = droplevels(subset(data_WS_Melted_2Km_phl, NAME_1 =='Nueva Ecija'))

data_melted_2km = rbindlist(list(data_DS_Melted_2Km_phl,data_WS_Melted_2Km_phl,data_DS_Melted_2Km_ind,data_DS_Melted_2Km_it))
#
# aggregate_n_seasons = data_melted_2km[, list(    # Find out which pixels are detected as biseasonal - needed to avoid counting
#   n_valid = sum (is.finite(Sow_Date_MODIS)))                           # the area twice !!
#   , by = c('country','Season','id')]
# data_melted_2km = join(data_melted_2km,aggregate_n_seasons, by =  c('country','Season','id'), match = 'all')
# data_melted_2km = droplevels(subset(data_melted_2km,n_valid <2 ))

keycols = c("id_big","Season","country")
data_melted_2km = droplevels(subset(data_melted_2km, ref_rice_fc> 0.75))
aggregate_values = data_melted_2km[, list(
  ref_ricearea_DS = sum(ref_ricearea_DS/4, na.rm = T)/10000,			# cumulate area classified as rice
  tot_area = sum(tot_cellarea_DS/4, na.rm = T)/10000,					# total non-nodata area
  refrice_fc = sum(ref_ricearea_DS, na.rm = T)/sum(tot_cellarea_DS, na.rm = T),					# total non-nodata area
  mod_ricearea = sum((tot_cellarea_DS*is.finite(Sow_Date_MODIS)), na.rm = T)/10000,	# area detected as rice in each of the 4 quarters
  mean_sow_mod = mean(Sow_Date_MODIS, na.rm = T),	# average doy of min in each of the 4 quarters
  mean_min_sar = mean(Sow_Date_SAR, na.rm = T), country = country[1],
  mean_sow_mod_dd = mean(Sow_Date_MODIS_dd, na.rm = T),	# average doy of min in each of the 4 quarters
  mean_min_sar_dd = mean(Sow_Date_SAR_dd, na.rm = T), country = country[1],
  std_sow_mod = sd(Sow_Date_MODIS_dd, na.rm = T),
  std_sow_sar = sd(Sow_Date_SAR_dd, na.rm = T))

  , by =keycols]

summary(aggregate_values$std_sow_mod)
summary(aggregate_values$std_sow_sar)

aggregate_values$mean_sow_mod_dd [aggregate_values$country != 'IND']= doytodate(aggregate_values$mean_sow_mod[aggregate_values$country != 'IND'],2013)
aggregate_values$mean_sow_mod_dd [aggregate_values$country == 'IND']= doytodate(aggregate_values$mean_sow_mod[aggregate_values$country == 'IND'],2014)

# aggregate_values_sub = subset(aggregate_values, std_sow_sar < 15 & std_sow_mod < 10 )
aggregate_values_sub = aggregate_values
Regr_Stats_DS =  regress_stats(aggregate_values_sub$mean_sow_mod, aggregate_values_sub$mean_min_sar)
eqn_strings_WS = lm_eqn(Regr_Stats_DS)

area_limit = 0.75
subdata = subset(aggregate_values_sub, refrice_fc > area_limit)
x = subdata$mean_min_sar_dd
y = subdata$mean_sow_mod_dd
z = subdata$country
w = subdata$Season

Regr_Stats =  regress_stats( y, x )
eqn_strings = lm_eqn(Regr_Stats)
ref_rice_Area = 'ref_rice_Area'

temp_df = data.frame(x = x, y = y, z = z, w = w)
#	eqn_strings$range_max = range_max
max_x = max(x,y, na.rm = T)
max_y = max(x,y, na.rm = T)
max_used = max(max_x, max_y)
# eqn_strings$max_used = max_used
# eqn_strings$country = 'PHL'

aggregate_values$gridder = factor(paste0(aggregate_values$country, aggregate_values$Season))
levels (aggregate_values$gridder)= c('IND - Samba',"IT","PHL - DS","PHL - WS ")

subdata$gridder = factor(paste0(subdata$country, subdata$Season))
levels (subdata$gridder)= c('IND - Samba',"IT","PHL - DS","PHL - WS ")

# p1 = ggplot(data = aggregate_values, aes(x = mean_min_sar, y = mean_sow_mod))
#
# p1 = p1 + geom_point(aes(color = gridder, pch = gridder),alpha = 0.3, size = 2)+theme_bw()# +xlim(100,180)+ylim(100,180)
# p1 = p1 + xlab('Reference Sowing Date [DOY]')+ylab('PhenoRice Estimated Sowing Date [DOY]')
# p1 = p1 + geom_abline(intercept = 0, slope = 1, linetype = 2)
# p1 = p1 + geom_abline(aes(intercept = Intercept, slope = Slope),data = Regr_Stats,linetype = 4, color = 'red', size = 0.75)
# p1 = p1 + geom_text(data = eqn_strings, aes(x = -100, y = 220, label = eq), parse = T, size = 4, hjust = 0, face = 'bold')
# p1 = p1 + ggtitle(paste0('PhenoRice vs Reference Sowing Dates - 2x2 Km grid'))+scale_color_grey('Legend')+scale_shape('Legend')
# p_compsow_2km_DS = p1 + guides(colour = guide_legend(override.aes = list(alpha = 1)))+ theme(legend.justification=c(1,0), legend.position=c(1,0),legend.background = element_rect(color = 'black'))

p1 = ggplot(data = subdata, aes(x = mean_min_sar_dd, y = mean_sow_mod_dd))
p1 = p1 + geom_point(aes(color = gridder, pch = gridder),alpha = 0.2, size = 2)+theme_bw()# +xlim(100,180)+ylim(100,180)
p1 = p1 + geom_smooth(aes(color = gridder), method="lm", fill = NA, lty = 'dashed')
p1 = p1 + xlab('Reference crop establishment date')+ylab('PhenoRice crop establishment date')
p1 = p1 + geom_abline(intercept = 0, slope = 1, linetype = 2)
 p1 = p1 + geom_abline(aes(intercept = datetodoy(as.Date("2013-05-09") + 1 ), slope = Slope),
                       data = Regr_Stats,linetype = 4, color = 'red', size = 0.75)
p1 = p1 + geom_text(data = eqn_strings, aes(x = as.Date('2012-11-01'),y = as.Date('2013-11-01'), label = eq), 
                    parse = T, size = 4, hjust = 0, fontface = 'bold')
p1 = p1 + ggtitle(paste0('PhenoRice vs reference establishment dates - 2 x 2 Km grid'))+ 
  scale_color_grey('Legend', start = 0, end = 0)+scale_shape('Legend')
p1 = p1 + guides(colour = guide_legend(override.aes = list(alpha = 1))) + 
  theme(legend.justification=c(1,0), legend.position=c(1,0),legend.background = element_rect(color = 'black'))
p1 = p1 + scale_x_date(date_breaks = '2 month', date_labels = "%b %Y",  
                       limits = c(as.Date('2012-11-01'),as.Date ('2013-11-01'))) +
  scale_y_date(date_breaks = '2 month', date_labels = "%b %Y", limits = c(as.Date('2012-11-01'),as.Date ('2013-11-01')))
p1 = p1 + theme(axis.title = element_text(size = 17),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    plot.title = element_text(size = 15),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.key = element_rect(fill = "white",
        colour = "black", linetype = "solid"),
    legend.background = element_rect(fill = "white",
        size = 0.1, linetype = "solid"),
    legend.position = c(1.010, -0.015))
p1
p1 + geom_smooth(method="lm", fill = NA, color = 'red', lty = 'dashed')

p1 + geom_smooth(data = aggregate_values,method='lm',formula=mean_sow_mod_dd~mean_min_sar_dd)


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
  mean_sow_mod = mean(Sow_Date_MODIS_dd, na.rm = T),	# average doy of min in each of the 4 quarters
  mean_min_sar = mean(Sow_Date_SAR_dd, na.rm = T))

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
