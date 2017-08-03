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

library(data.table)
library(ggplot2)
library(scales)
library(ireaRscripts)
library(gridExtra)
library(raster)
library(rWBclimate)
library(grid)
library(gtable)
# Load datasets  ----

only_nueva = 0
setwd("/home/lb/phenorice/")
results_folder = file.path(getwd(),"PhenoRice/Processing/ITA/Outputs/Final/Validate/")
country_code = 'ITA'
out_RData_file = file.path(results_folder, 'Plots_Data.RData')
load(out_RData_file)
data_plot_dates_it = data_plot_dates
data_plot_dates_it$country = 'ITA'
data_plot_dates_it$Date = as.Date(data_plot_dates_it$DOY - 1, origin = "2013-01-01")

results_folder = file.path(getwd(),"PhenoRice/Processing/IND/Outputs/Final/Validate/")
country_code = 'IND'
out_RData_file = file.path(results_folder, 'Plots_Data.RData')
load(out_RData_file)
data_plot_dates_ind = data_plot_dates
data_plot_dates_ind$country = 'IND'
data_plot_dates_ind$Date = as.Date(data_plot_dates_ind$DOY - 1, origin = "2013-01-01")

results_folder =file.path(getwd(),"PhenoRice/Processing/PHL/Outputs/Final/Validate/")
country_code = 'PHL'
out_RData_file = file.path(results_folder, 'Plots_Data.RData')
load(out_RData_file)
data_plot_dates_phl = data_plot_dates
data_plot_dates_phl$country = 'PHL'
data_plot_dates_phl$Date = as.Date(data_plot_dates_phl$DOY - 1, origin = "2013-01-01")
if (only_nueva == 1) {data_plot_dates_phl = droplevels(subset(data_plot_dates_phl, NAME_1 == 'Nueva Ecija'))}


data_plot_dates_it_sub = data_plot_dates_it[,c("variable", "DOY", "Quarter", "country","Date", "n_seasons")]
data_plot_dates_ind_sub = data_plot_dates_ind[,c("variable", "DOY", "Quarter", "country","Date", "n_seasons")]
data_plot_dates_phl_sub = data_plot_dates_phl[,c("variable", "DOY", "Quarter", "country","Date", "n_seasons")]

data_plot_dates_it_sub$Season = 'First'
data_plot_dates_phl_sub$Season [data_plot_dates_phl_sub$Date < '2013-03-01'] = 'First'
data_plot_dates_phl_sub$Season [data_plot_dates_phl_sub$Date > '2013-03-01'] = 'Second'

data_plot_dates_ind_sub$Season [data_plot_dates_ind_sub$Date < '2013-01-01'] = 'First'
data_plot_dates_ind_sub$Season [data_plot_dates_ind_sub$Date < '2013-06-01' & data_plot_dates_ind_sub$Date > '2013-01-01'] = 'Second'
data_plot_dates_ind_sub$Season [data_plot_dates_ind_sub$Date > '2013-06-01' ] = 'Third'

data_plot_dates_tot = rbindlist(list(data_plot_dates_it_sub,data_plot_dates_ind_sub,data_plot_dates_phl_sub))
data_plot_dates_tot$country = factor(data_plot_dates_tot$country)
data_plot_dates_tot$country = factor(data_plot_dates_tot$country,levels = levels(data_plot_dates_tot$country)[c(2,3,1)], labels = c('ITA',"PHL","IND"))

# plto sowing -----

data = subset(data_plot_dates_tot, variable == 'Sowing' & DOY <1000 & !is.na(DOY) & n_seasons <4)
data$country = factor(data$country, levels = c("ITA", "IND", "PHL"  ))
data$Season = as.factor(data$Season)
# p = ggplot(data, aes(x = Date, fill = Season))+theme_bw()+facet_wrap(~country, nrow = 3, scales = 'free_y')
p = ggplot(data, aes(x = Date))+theme_bw()+facet_wrap(~country, nrow = 3)
p = p + geom_histogram(aes(y=100*..count../tapply(..count..,..PANEL..,sum)[..PANEL..]),alpha=.5, position = 'identity', breaks = as.numeric(seq(as.Date("2012-09-06"), as.Date("2013-10-16"),'8 day')), color = 'black')
p = p +theme(plot.title = element_text(vjust = 1, hjust = 0))
p3_mins = p +ylab('Frequency [%]')+   scale_x_date('Establishment date',labels = date_format("%b"),limits = c(as.Date("2012-09-01"), as.Date("2013-10-30")), breaks = date_breaks(width = "1 month"))+
   lb_theme_bw(x_ang = 45)+ theme(axis.text.x = element_text( hjust  = 1 , vjust = 1))+ylim(0,43)+ theme(axis.title = element_text(size = 17),
                                                                                                         axis.text.x = element_text(size = 12),
                                                                                                         axis.text.y = element_text(size = 12),
                                                                                                         plot.title = element_text(size = 15),
                                                                                                         legend.text = element_text(size = 12),
                                                                                                         legend.title = element_text(size = 12))

# plto Flowering -----


data_flow = subset(data_plot_dates_tot, variable == 'Heading')
data_flow$country = factor(data_flow$country, levels = c("ITA", "IND", "PHL"  ))
data_flow$Season = as.factor(data_flow$Season)

p = ggplot(data_flow, aes(x = Date))+theme_bw()+facet_wrap(~country, nrow = 3)
p = p + geom_histogram(aes(y=100*..count../tapply(..count..,..PANEL..,sum)[..PANEL..]),alpha=.5, position = 'identity', breaks = as.numeric(seq(as.Date("2012-09-06"), as.Date("2013-10-16"),'8 day')), color = 'black')
p = p +theme(plot.title = element_text(vjust = 1, hjust = 0))+theme(axis.title.y = element_text(vjust  = 1 ))
p3_maxs = p +ylab('Frequency [%]')+   scale_x_date('Flowering date',labels = date_format("%b"),limits = c(as.Date("2012-09-01"), as.Date("2013-10-30")), breaks = date_breaks(width = "1 month"))+
  lb_theme_bw(x_ang = 45)+ theme(axis.text.x = element_text( hjust  = 1 , vjust = 1))+ theme(axis.title = element_text(size = 17),
                                                                                             axis.text.x = element_text(size = 12),
                                                                                             axis.text.y = element_text(size = 12),
                                                                                             plot.title = element_text(size = 15),
                                                                                             legend.text = element_text(size = 12),
                                                                                             legend.title = element_text(size = 12))


# plto Lengths  -----
data_length = subset(data_plot_dates_tot, variable == 'Length' & DOY != 0& n_seasons <4)
data_length$country = factor(data_length$country, levels = c("ITA", "IND", "PHL"  ))
data_length$Season = as.factor(data_flow$Season)
p = ggplot(data_length, aes (x = DOY))+theme_bw()+theme(axis.title.y = element_text(vjust  = 1 ))
p = p + geom_histogram(aes(fill = n_seasons,y=100*..count../tapply(..count..,..PANEL..,sum)[..PANEL..]),binwidth = 8, color = 'black', fill = 'grey75')+
  facet_wrap(~country, nrow = 3)
p = p + lb_theme_bw(x_ang = 45)+xlab('Length of season')+ylab('Frequency [%]')
p4_lengths = p + xlim(0,160)+ theme(axis.text.x = element_text( hjust  = 1 , vjust = 1))+ theme(axis.title = element_text(size = 17),
                                                                                                axis.text.x = element_text(size = 12),
                                                                                                axis.text.y = element_text(size = 12),
                                                                                                plot.title = element_text(size = 15),
                                                                                                legend.text = element_text(size = 12),
                                                                                                legend.title = element_text(size = 12))


# p = ggplot(subset(data_plot_dates_tot, variable == 'Length' & DOY != 0), aes (x = DOY))+theme_bw()+theme(axis.title.y = element_text(vjust  = 1 ))
# p = p + geom_histogram(aes(y=100*..count../tapply(..count..,..PANEL..,sum)[..PANEL..]),binwidth = 8, color = 'black', fill = 'grey75')+facet_wrap(~country, nrow = 3, scales = 'free_y')
# # p = p +theme(plot.title = element_text(vjust = 1, hjust = 0))
# # p = p + ggtitle('Distribution of retrieved length of Season (Sow to Heading)')
# p4_lengths = p +xlim(1,150)+ylab('Frequency [%]')+xlab('Number of Days')#+facet_wrap(~Quarter)
#
# p = ggplot(subset(data_plot_dates_tot, variable == 'Length' & DOY != 0 & n_seasons <4), aes (y = DOY, x = as.factor(n_seasons)))+theme_bw()+theme(axis.title.y = element_text(vjust  = 1 ))
# p = p+ geom_histogram()
# p
#
# p = ggplot(subset(data_plot_dates_tot, variable == 'Length' & DOY != 0 & n_seasons <4), aes (x = variable,y = DOY))
# p = p + geom_boxplot(outlier.colour = 'transparent')+ylab('Days Betwwen Sowing and Flowering')+facet_wrap(~country, nrow = 3)
# p  = p + lb_theme_bw()
# p4_lengths_box = p
#
#
#
# p = ggplot(subset(data_plot_dates_tot, variable == 'Length' & DOY != 0 & n_seasons < 4), aes (x = DOY, color = n_seasons))
# p = p + geom_freqpoly(aes(y=100*..count../tapply(..count..,..PANEL..,sum)[..PANEL..]),binwidth = 8)+
#   facet_grid(n_seasons~country, scales = 'free_y')
# p+theme_bw()+theme(axis.title.y = element_text(vjust  = 1 ))

# p = p + geom_boxplot
# #   geom_histogram(aes(y=100*..count../tapply(..count..,..PANEL..,sum)[..PANEL..]),binwidth = 8, color = 'black', fill = 'grey75')+facet_wrap(~country, nrow = 3, scales = 'free_y')
# p = ggplot(subset(data_plot_dates_tot, variable == 'Length' & DOY != 0 & country == 'Nueva Ecjia'), aes (y = DOY, x = Quarter))+theme_bw()+theme(axis.title.y = element_text(vjust  = 1 ))
# p = p + geom_boxplot(outlier.colour = 'transparent')+ylab('Days Betwwen Sowing and Flowering')+ylim(0,150)#+geom_jitter(alpha = 0.01)



#Get  and plot Climate Data ----

dates = rep(as.Date('2013-01-01'),12)+cumsum(c(0,31,28,31,30,31,30,31,31,30,30,31))

lb_plotclim = function(clim_data){

  p = ggplot(clim_data, aes(x = date+3)) + lb_theme_bw()
  p = p + geom_bar(aes(y = prec), stat = 'identity', width = 5, color = 'black')
  p = p + geom_line(aes(y = tmean), color = 'red', size = 1.1)
  p = p + ylab('Temperature [°C] / Rainfall [mm*10-1]')
  p = p + scale_x_date('Month',labels = date_format("%b"),limits = c(as.Date("2013-01-01"), as.Date("2013-12-10")), breaks = date_breaks(width = "1 month"))
  p = p + facet_wrap(~cy, nrow = 3)
  p
}

min_it = getData('worldclim', var=c('tmin'), res=0.5, lon=5, lat=45)
max_it = getData('worldclim', var=c('tmax'), res=0.5, lon=5, lat=45)
prec_it = getData('worldclim', var=c('prec'), res=0.5, lon=5, lat=45)
sp_it <- SpatialPoints(data.frame(Site_lon = 8.5, Site_lat = 45.14))
tmin_it = extract(min_it, sp_it, method='bilinear')
tmax_it = extract(max_it, sp_it, method='bilinear')
prec_it = extract(prec_it, sp_it, method='bilinear')
tmean_it = (tmin_it+tmax_it)/2
clim_it = data.frame(date = dates, tmean = 0.1*as.numeric(tmean_it), prec = 0.1*as.numeric(prec_it), cy = 'ITA')
clim_it = rbind(clim_it[9:12,],clim_it, clim_it[1:5,])
clim_it$date[1:4] = clim_it$date[1:4]-365
clim_it$date[17:21] = clim_it$date[17:21]+365

min_ph = getData('worldclim', var=c('tmin'), res=0.5, lon=120, lat=15)
max_ph = getData('worldclim', var=c('tmax'), res=0.5, lon=120, lat=15)
prec_ph = getData('worldclim', var=c('prec'), res=0.5, lon=120, lat=15)

sp_ph <- SpatialPoints(data.frame(Site_lon = 120.830, Site_lat =15.610))
tmin_ph = extract(min_ph, sp_ph, method='bilinear')
tmax_ph = extract(max_ph, sp_ph, method='bilinear')
prec_ph = extract(prec_ph, sp_ph, method='bilinear')
tmean_ph = (tmin_ph+tmax_ph)/2
clim_ph = data.frame(date = dates, tmean = 0.1*as.numeric(tmean_ph), prec = 0.1*as.numeric(prec_ph), cy = 'PHL')
clim_ph = rbind(clim_ph[9:12,],clim_ph, clim_ph[1:5,])
clim_ph$date[1:4] = clim_ph$date[1:4]-365
clim_ph$date[17:21] = clim_ph$date[17:21]+365

min_ind = getData('worldclim', var=c('tmin'), res=0.5, lon=79, lat=11)
max_ind = getData('worldclim', var=c('tmax'), res=0.5, lon=79, lat=11)
prec_ind = getData('worldclim', var=c('prec'), res=0.5, lon=79, lat=11)
sp_ind <- SpatialPoints(data.frame(Site_lon = 78.670, Site_lat =10.995))
tmin_ind = extract(min_ind, sp_ind, method='bilinear')
tmax_ind = extract(max_ind, sp_ind, method='bilinear')
prec_ind = extract(prec_ind, sp_ind, method='bilinear')
tmean_ind = (tmin_ind+tmax_ind)/2
clim_ind = data.frame(date = dates, tmean = 0.1*as.numeric(tmean_ind), prec = 0.1*as.numeric(prec_ind), cy = 'IND')
clim_ind = rbind(clim_ind[9:12,],clim_ind, clim_ind[1:5,])
clim_ind$date[1:4] = clim_ind$date[1:4]-365
clim_ind$date[17:21] = clim_ind$date[17:21]+365

clim_data = rbind(clim_it, clim_ph, clim_ind)
names(clim_data)[4] = 'country'
clim_data$country = factor(clim_data$country, levels = c("ITA", "IND", "PHL"  ))

# p_clim_it = lb_plotclim(clim_data)+lb_theme_bw(x_ang = 45)+ theme(axis.text.x = element_text( hjust  = 1 , vjust = 1))
# p_clim_ph = lb_plotclim(clim_ph)
# p_clim_ind = lb_plotclim(clim_ind)


# Arrange panels and plot ----
# p3_mins_clim = p3_mins +geom_line(data = clim_data, aes(x = date, y = tmean, group = country), color = 'red', lty = 2, size = 1, alpha = 0.7)+
#   geom_line(data = clim_data,aes(x = date,y = prec, group = country),  color = 'blue', lty = 2, size = 1, alpha = 0.7)
#
# # p3_maxs = p3_maxs +geom_line(data = clim_data, aes(x = date, y = tmean, group = country), color = 'red', lty = 2, size = 1)+
# #   geom_line(data = clim_data,aes(x = date,y = prec, group = country),  color = 'blue', lty = 2, size = 1)

p1 = p3_mins
p2 = ggplot(data = clim_data, aes(x = date, y = tmean, group = country))
p2 = p2 + geom_line(color = 'red', lty = 3, size = 1, alpha = 0.7)+
  geom_line(data = clim_data,aes(x = date,y = prec, group = country),  color = 'blue', lty = 3, size = 1, alpha = 0.7)
p2 = p2 + facet_wrap(~country, nrow =3)+   scale_x_date('Sowing date',labels = date_format("%b"),limits = c(as.Date("2012-09-01"), as.Date("2013-10-30")), breaks = date_breaks(width = "1 month"))+
  lb_theme_bw(x_ang = 45)+ theme(axis.text.x = element_text( hjust  = 1 , vjust = 1))+ylim(0,43)

  # two plots

 p2 <- p2 +  lb_theme_bw(x_ang = 45)+ theme(axis.text.x = element_text( hjust  = 1 , vjust = 1)) %+replace%  theme(panel.background = element_rect(fill = NA))
 p2 = p2 + theme(panel.grid.major = element_line(color = 'transparent'))   + theme(panel.grid.minor = element_line(color = 'transparent'))

  g1 <- ggplot_gtable(ggplot_build(p1))
  g2 <- ggplot_gtable(ggplot_build(p2))


  panels = g1$layout$name[ grepl("panel", g1$layout$name)]
  axis = g1$layout$name[ grepl("axis_l", g1$layout$name)]
  # overlap the panel of 2nd plot on that of 1st plot
  pp <- c(subset(g1$layout, name %in% panels, se = t:r))
  ia <- which(g2$layout$name %in% axis)
  indpan = 1
  g = g1
  a= g2$widths[g2$layout[ia, ]$l]
  a [] = unit(0.13, "cm")

  g <- gtable_add_cols(g,  a, pos = -1 )
  for (pan in which(g2$layout$name %in% panels)) {

    g <- gtable_add_grob(g, g2$grobs[[pan]], pp$t[[indpan]], pp$l[[indpan]], pp$b[[indpan]], pp$l[[indpan]])

    for (ias in ia)
    {
      ga <- g2$grobs[[ias]]
      ax <- ga$children[[2]]
      ax$widths <- rev(ax$widths)
      ax$grobs <- rev(ax$grobs)
      ax$grobs[[1]]$x <- ax$grobs[[1]]$x - unit(1, "npc") + unit(0.0, "cm")
      # ax$grobs[[2]]$x <- ax$grobs[[1]]$x - unit(1, "npc") + unit(0.0, "cm")
      g <- gtable_add_grob(g, ax, pp$t[[indpan]], length(g$widths) - 1, pp$b[[indpan]])
      # g <- gtable_add_grob(g, ax, pp$t[[indpan]], length(g$widths) - 1, pp$b[[indpan]])
    }
    indpan= indpan+1

  }
  a= g2$widths[g2$layout[ia, ]$l]
  a [] = unit(0.35, "cm")
   text.grob1 = textGrob("Avg. T [°C] / Cum. Rainfall [mm/10]", rot = 90, y = unit(.5, "npc"), gp = gpar(fontsize = 9), vjust = 0.2)
  g <- gtable_add_cols(g,  a, pos = -1 )
  g <-  gtable_add_grob(g, text.grob1, pp$t[[2]]-5, length(g$widths) - 1, pp$b[[2]]+5)
  pmins_meteo = g

  # Add meteo data to max plot ----

  p1 = p3_maxs
  p2 = ggplot(data = clim_data, aes(x = date, y = tmean, group = country))
  p2 = p2 + geom_line(color = 'red', lty = 3, size = 1, alpha = 0.7)+
    geom_line(data = clim_data,aes(x = date,y = prec, group = country),  color = 'blue', lty = 3, size = 1, alpha = 0.7)
  p2 = p2 + facet_wrap(~country, nrow =3)+   scale_x_date('Sowing date',labels = date_format("%b"),limits = c(as.Date("2012-09-01"), as.Date("2013-10-30")), breaks = date_breaks(width = "1 month"))+
    lb_theme_bw(x_ang = 45)+ theme(axis.text.x = element_text( hjust  = 1 , vjust = 1))+ylim(0,43)

  # two plots

  p2 <- p2 +  lb_theme_bw(x_ang = 45)+ theme(axis.text.x = element_text( hjust  = 1 , vjust = 1)) %+replace%  theme(panel.background = element_rect(fill = NA))
  p2 = p2 + theme(panel.grid.major = element_line(color = 'transparent'))   + theme(panel.grid.minor = element_line(color = 'transparent'))

  g1 <- ggplot_gtable(ggplot_build(p1))
  g2 <- ggplot_gtable(ggplot_build(p2))


  panels = g1$layout$name[ grepl("panel", g1$layout$name)]
  axis = g1$layout$name[ grepl("axis_l", g1$layout$name)]
  # overlap the panel of 2nd plot on that of 1st plot
  pp <- c(subset(g1$layout, name %in% panels, se = t:r))
  ia <- which(g2$layout$name %in% axis)
  indpan = 1
  g = g1
  a= g2$widths[g2$layout[ia, ]$l]
  a [] = unit(0.13, "cm")

  g <- gtable_add_cols(g,  a, pos = -1 )
  for (pan in which(g2$layout$name %in% panels)) {

    g <- gtable_add_grob(g, g2$grobs[[pan]], pp$t[[indpan]], pp$l[[indpan]], pp$b[[indpan]], pp$l[[indpan]])

    for (ias in ia)
    {
      ga <- g2$grobs[[ias]]
      ax <- ga$children[[2]]
      ax$widths <- rev(ax$widths)
      ax$grobs <- rev(ax$grobs)
      ax$grobs[[1]]$x <- ax$grobs[[1]]$x - unit(1, "npc") + unit(0.0, "cm")
      # ax$grobs[[2]]$x <- ax$grobs[[1]]$x - unit(1, "npc") + unit(0.0, "cm")
      g <- gtable_add_grob(g, ax, pp$t[[indpan]], length(g$widths) - 1, pp$b[[indpan]])
      # g <- gtable_add_grob(g, ax, pp$t[[indpan]], length(g$widths) - 1, pp$b[[indpan]])
    }
    indpan= indpan+1

  }
  a= g2$widths[g2$layout[ia, ]$l]
  a [] = unit(0.35, "cm")
  text.grob1 = textGrob("Avg. T [°C] / Cum. Rainfall [mm/10]", rot = 90, y = unit(.5, "npc"), gp = gpar(fontsize = 9), vjust = 0.2)
  g <- gtable_add_cols(g,  a, pos = -1 )
  g <-  gtable_add_grob(g, text.grob1, pp$t[[2]]-5, length(g$widths) - 1, pp$b[[2]]+5)
  pmaxs_meteo = g




grid.arrange(pmins_meteo,pmaxs_meteo,p4_lengths,widths = c(9.3,9.3,5.5),ncol = 3)


# plots 0f sos vs pos and sos vs length ----
data1= subset(data_plot_dates_tot, variable == 'Sowing'   & n_seasons <4)
data2= subset(data_plot_dates_tot, variable == 'Heading' & n_seasons <4)


ggplot(faithfuld, aes(waiting, eruptions)) +
  geom_hex(aes(stat = density))

data = cbind(data1, data2$DOY)
data$diff = data$V2 - data$DOY
# p = ggplot(data,aes(x = DOY,y = diff,fill = ..density..)))+ facet_wrap(~country)+
#   stat_summary_hex()
#   stat_density_2d(geom = "hex", aes(fill = 100*..density..), contour = FALSE, binwidth = c(16,16))+
#   facet_wrap(~country)
# p
#
#   stat_density2d( aes(fill = ..density..), geom="raster")
#
#
#
# +facet_wrap(~country)
#
#              stat_binhex(aes(alpha = ..count..))
#   geom_hex(binwidth = c(8,8) )
#
p = ggplot(data)+ geom_hex(binwidth = c(12,12),aes(Date,V2-DOY))+facet_grid(~country)+scale_fill_gradient("N° of pixels", high= 'black', low = 'grey97', limits = c(50, 7000), na.value = 'transparent')+lb_theme_bw()
p = p + lb_theme_bw()
p  = p + xlab('Establishment date')+ylab('Length of vegetative phase')
p_sow_lgt = p +
  scale_x_date('Establishment date',labels = date_format("%b"),limits = c(as.Date("2012-09-01"), as.Date("2013-10-30")), breaks = date_breaks(width = "2 month"))+ ylim(30,150)+ theme(axis.title = element_text(size = 17),
                                                                                                                                                                                       axis.text.x = element_text(size = 12),
                                                                                                                                                                                       axis.text.y = element_text(size = 12),
                                                                                                                                                                                       plot.title = element_text(size = 15),
                                                                                                                                                                                       legend.text = element_text(size = 12),
                                                                                                                                                                                       legend.title = element_text(size = 12))



p = ggplot(data)+ geom_hex(binwidth = c(12,12),aes(Date,lb_doytodate(V2,2013)))+facet_grid(~country)+scale_fill_gradient("N° of pixels",high= 'black', low = 'grey97', limits = c(50, 7000), na.value = 'transparent')+lb_theme_bw()
p  = p + xlab('Establishment date')+ylab('Flowering date')
p_sow_pos = p +
  scale_x_date('Establishment date',labels = date_format("%b"),limits = c(as.Date("2012-09-01"), as.Date("2013-10-30")), breaks = date_breaks(width = "2 month"))+
  scale_y_date('Flowering date',labels = date_format("%b"),limits = c(as.Date("2012-09-01"), as.Date("2013-10-30")), breaks = date_breaks(width = "2 month"))+ theme(axis.title = element_text(size = 17),
                                                                                                                                                                     axis.text.x = element_text(size = 12),
                                                                                                                                                                     axis.text.y = element_text(size = 12),
                                                                                                                                                                     plot.title = element_text(size = 15),
                                                                                                                                                                     legend.text = element_text(size = 12),
                                                                                                                                                                     legend.title = element_text(size = 12))

grid.arrange (p_sow_pos, p_sow_lgt)


p = ggplot(data)+  stat_density_2d(aes(Date,lb_doytodate(V2,2013)))+lb_theme_bw()+facet_grid(~country)
p  = p + xlab('Sowing date')+ylab('Flowering date')
p_sow_pos = p +
	scale_x_date('Sowing date',labels = date_format("%b"),limits = c(as.Date("2012-09-01"), as.Date("2013-10-30")), breaks = date_breaks(width = "2 month"))+
	scale_y_date('Flowering date',labels = date_format("%b"),limits = c(as.Date("2012-09-01"), as.Date("2013-10-30")), breaks = date_breaks(width = "2 month"))

grid.arrange (p_sow_pos, p_sow_lgt)


p = ggplot(data, aes(Date,V2-DOY))+  geom_density_2d() +facet_grid(~country)
# +scale_fill_gradient(high= 'black', low = 'grey97', limits = c(50, 7000), na.value = 'transparent')+lb_theme_bw()
p = p + lb_theme_bw()
p  = p + xlab('Sowing date')+ylab('Length of Vegetative Season')
p_sow_lgt = p +
	scale_x_date('Sowing date',labels = date_format("%b"),limits = c(as.Date("2012-09-01"), as.Date("2013-10-30")), breaks = date_breaks(width = "2 month"))


grups = group_by(data_flow, country, Season)
stats = summarise(grups, avg = mean(DOY, na.rm = T))

ordered = sort(p$DOY)
n = sum(!is.na(p$DOY))
plot(ordered, (1:n)/n, type = 's', ylim = c(0, 1), xlab = 'Sample Quantiles of Ozone', ylab = '', main = 'Empirical Cumluative Distribution\nOzone Pollution in New York')
abline(v = 233, h = 0.90)


a = p %>%
  filter(DOY > 50)%>%
  group_by(country,Season) %>%
  do(tidy(t(quantile(.$DOY, na.rm = T, probs = seq(0, 1, 0.05) ,type = 5))) )

stat2 = function(data){
  a = regress_stats(data$DOY, data$V2-data$DOY)
  a
}
a = regress_stats(data$DOY, data$V2)
pro = ddply(data, .(country, Season),.fun =stat)
pro2 = ddply(data, .(country, Season),.fun =stat2)
