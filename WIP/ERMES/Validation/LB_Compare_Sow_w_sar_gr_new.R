gr_shape_file_15 = 'D:/Temp/PhenoRice/Processing/Validation_Ermes/Input/GR_Static_info_2015_buf.shp'
gr_shape_15 = readOGR (file.path(dirname(gr_shape_file_15)), file_path_sans_ext(basename(gr_shape_file_15)))
types = c("ASC","DSC","Both")
# type = 'ASC'
for (type in types) {
  print(type)

if (type == "Both" ){
  in_raster = raster('D:/Temp/PhenoRice/Processing/Validation_Ermes/GR/SAR/S1A2015/A+D_MSOS')
  in_raster2 = raster('D:/Temp/PhenoRice/Processing/Validation_Ermes/GR/SAR/S1A2015/A+D_MPOS')
  out_shp = 'D:/Temp/PhenoRice/Processing/Validation_Ermes/Input/GR_Static_info_2015_w_sar_All.shp'
  out_rdata = 'd:/temp/phenorice/processing/Validation_Ermes/GR_15_SAR_All.RData'
  values(in_raster) = (values(in_raster)-2)*6+136
  values(in_raster)[which(values(in_raster) < 136)] =NA
  values(in_raster2) = (values(in_raster2)-2)*6+136
  values(in_raster2)[which(values(in_raster2) < 136)] =NA
  small_15 = crop(in_raster, gr_shape_15)
  small_15_pos = crop(in_raster2, gr_shape_15)
  dir.create('D:/Temp/PhenoRice/Processing/GR/ancillary/validation_maps/SAR/All/')
  writeRaster(small_15,'D:/Temp/PhenoRice/Processing/GR/ancillary/validation_maps/SAR/All/SOS_Small_All.tif', overwrite = TRUE)
  writeRaster(small_15_pos,'D:/Temp/PhenoRice/Processing/GR/ancillary/validation_maps/SAR/All/POS_Small_All.tif', overwrite = TRUE)
}

if (type == "ASC" ){
  in_raster = raster('D:/Temp/PhenoRice/Processing/Validation_Ermes/GR/SAR/S1A2015/Greece-Asc_SoS')
  in_raster2 = raster('D:/Temp/PhenoRice/Processing/Validation_Ermes/GR/SAR/S1A2015/Greece-Asc_PoS')
  out_shp = 'D:/Temp/PhenoRice/Processing/Validation_Ermes/Input/GR_Static_info_2015_w_sar_Asc.shp'
  out_rdata = 'd:/temp/phenorice/processing/Validation_Ermes/GR_15_SAR_Asc.RData'
  values(in_raster) = (values(in_raster)-2)*12+136
  values(in_raster)[which(values(in_raster) < 136)] =NA
  values(in_raster2) = (values(in_raster2)-2)*12+136
  values(in_raster2)[which(values(in_raster2) < 136)] =NA
  small_15 = crop(in_raster, gr_shape_15)
  small_15_pos = crop(in_raster2, gr_shape_15)
  dir.create('D:/Temp/PhenoRice/Processing/GR/ancillary/validation_maps/SAR/ASC/')
  writeRaster(small_15,    'D:/Temp/PhenoRice/Processing/GR/ancillary/validation_maps/SAR/ASC/SOS_Small_ASC.tif', overwrite = TRUE)
  writeRaster(small_15_pos,'D:/Temp/PhenoRice/Processing/GR/ancillary/validation_maps/SAR/ASC/POS_Small_ASC.tif', overwrite = TRUE)
}

if (type == "DSC" ){
  in_raster = raster('D:/Temp/PhenoRice/Processing/Validation_Ermes/GR/SAR/S1A2015/Greece-Desc_SoS')
  in_raster2 =raster( 'D:/Temp/PhenoRice/Processing/Validation_Ermes/GR/SAR/S1A2015/Greece-Desc_PoS')
  out_shp = 'D:/Temp/PhenoRice/Processing/Validation_Ermes/Input/GR_Static_info_2015_w_sar_DSC.shp'
  out_rdata = 'd:/temp/phenorice/processing/Validation_Ermes/GR_15_SAR_Dsc.RData'
  values(in_raster) = (values(in_raster)-2)*12+142
  values(in_raster)[which(values(in_raster) < 142)] =NA
  values(in_raster2) = (values(in_raster2)-2)*12+142
  values(in_raster2)[which(values(in_raster2) < 142)] =NA
  small_15 = crop(in_raster, gr_shape_15)
  small_15_pos = crop(in_raster2, gr_shape_15)
  dir.create('D:/Temp/PhenoRice/Processing/GR/ancillary/validation_maps/SAR/DSC/')
  writeRaster(small_15,'D:/Temp/PhenoRice/Processing/GR/ancillary/validation_maps/SAR/DSC/SOS_Small_DSC.tif', overwrite = TRUE)
  writeRaster(small_15_pos,'D:/Temp/PhenoRice/Processing/GR/ancillary/validation_maps/SAR/DSC/POS_Small_DSC.tif', overwrite = TRUE)
}

myZonal <- function (x, z, stat, digits = 0, na.rm = TRUE,...) {
  library(data.table)
  # maj = apply(x, 1, function(idx) {
  #   which(tabulate(idx) == max(tabulate(idx)))
  # })
  fun <- match.fun(stat)
  vals <- getValues(x)
  zones <- round(getValues(z), digits = digits)
  rDT <- data.table(vals, z=zones)
  setkey(rDT, z)

  # fun = function(x, na.rm) {
  #   as.numeric(names(which.max(table(x))))
  # }
  # rDT[,list(mostFreqCat=as.numeric(names(which.max(table(vals))))),by=z]
  rDT[, lapply(.SD, fun, na.rm = T), by=z]
}

myZonal_length <- function (x, z, stat, digits = 0, na.rm = TRUE,...) {
  library(data.table)
  # maj = apply(x, 1, function(idx) {
  #   which(tabulate(idx) == max(tabulate(idx)))
  # })
  fun <- match.fun(stat)
  vals <- getValues(x)
  zones <- round(getValues(z), digits = digits)
  rDT <- data.table(vals, z=zones)
  setkey(rDT, z)

  fun = function(x, na.rm) {
    # browser()
    length(which(!is.na(x)==T))
    # as.numeric(names(which.max(table(x))))
  }
  # rDT[,list(mostFreqCat=as.numeric(names(which.max(table(vals))))),by=z]
  rDT[, lapply(.SD, fun, na.rm = T), by=z]
}


myZonal_median <- function (x, z, stat, digits = 0, na.rm = TRUE,...) {
  library(data.table)
  # maj = apply(x, 1, function(idx) {
  #   which(tabulate(idx) == max(tabulate(idx)))
  # })
  fun <- match.fun(stat)
  vals <- getValues(x)
  zones <- round(getValues(z), digits = digits)
  rDT <- data.table(vals, z=zones)
  setkey(rDT, z)

  fun = function(x, na.rm) {
    as.numeric(names(which.max(table(x))))
  }
  # rDT[,list(mostFreqCat=as.numeric(names(which.max(table(vals))))),by=z]
  rDT[, lapply(.SD, fun, na.rm = T), by=z]
}

ZonalPipe <- function (path.in.shp, path.in.r, path.out.r, path.out.shp, zone.attribute, stat,N){

  # 1/ Rasterize using GDAL

  #Ingriate parameter
  r<-stack(path.in.r)

  ext<-extent(r)
  ext<-paste(ext[1], ext[3], ext[2], ext[4])

  res<-paste(res(r)[1], res(r)[2])

  #Gdal_rasterize
  command<-'gdal_rasterize'
  command<-paste(command, "--config GDAL_CACHEMAX 2000") #Speed-up wgrh more cache (avice: max 1/3 of your total RAM)
  command<-paste(command, "-a", zone.attribute) #Identifies an attribute field on the features to be used for a burn in value. The value will be burned into all output bands.
  command<-paste(command, "-te", as.character(ext)) #(GDAL >= 1.8.0) set georeferenced extents. The values must be expressed in georeferenced ungrs. If not specified, the extent of the output file will be the extent of the vector layers.
  command<-paste(command, "-tr", res) #(GDAL >= 1.8.0) set target resolution. The values must be expressed in georeferenced ungrs. Both must be posgrive values.
  # command<-paste(command, "-overwrgre") #(GDAL >= 1.8.0) set target resolution. The values must be expressed in georeferenced ungrs. Both must be posgrive values.
  command<-paste(command, path.in.shp)
  command<-paste(command, path.out.r)

  system(command)

  # 2/ Zonal Stat using myZonal function
  zone<-raster(path.out.r)

  Zstat<-data.frame(myZonal(r, zone, mean))
  names(Zstat)[2] = paste0("mean")
  Zstat = droplevels(subset(Zstat, mean !=0))
  # colnames(Zstat)[2:length(Zstat)]<-paste0("B", c(1:(length(Zstat)-1)), "_",stat)

  Zstat2<-data.frame(myZonal_length(r, zone, mean))
   # browser()
  names(Zstat2)[2] = paste0("count")
  Zstat2 = droplevels(subset(Zstat2, count !=0))

  # colnames(Zstat)[2:length(Zstat)]<-paste0("B", c(1:(length(Zstat)-1)), "_",stat)

  Zstat3<-data.frame(myZonal_median(r, zone, mean))
  names(Zstat3)[2] = paste0("maj")
  # colnames(Zstat)[2:length(Zstat)]<-paste0("B", c(1:(length(Zstat)-1)), "_",stat)

  Zstat = cbind(Zstat,Zstat2$count,Zstat3$maj)
  # browser()
  names(Zstat)[2] =paste0("mean", N)
  names(Zstat)[3] =paste0("count", N)
  names(Zstat)[4] = paste0("maj", N)

  # 3/ Merge data in the shapefile and wrgre gr
  shp<-readOGR(path.in.shp, layer= sub("^([^.]*).*", "\\1", basename(path.in.shp)))

  shp@data <- data.frame(shp@data, Zstat[match(shp@data[,zone.attribute], Zstat[, "z"]),])
  # browser()
  writeOGR(shp, dirname(file.path(path.out.shp)), layer= sub("^([^.]*).*", "\\1", basename(path.out.shp)), driver="ESRI Shapefile", overwrite_layer = T)
  return(shp)

}

shp = ZonalPipe(gr_shape_file_15,
           small_15,
          'D:/Temp/PhenoRice/Processing/GR/ancillary/validation_maps/SAR/shape_rasterized.tiff.tif',
          'D:/Temp/PhenoRice/Processing/Validation_Ermes/Input/GR_Static_info_2015_w_sar_temp.shp',
          'int_id', stat="mean",1)

shp = ZonalPipe('D:/Temp/PhenoRice/Processing/Validation_Ermes/Input/GR_Static_info_2015_w_sar_temp.shp',
           small_15_pos,
          'D:/Temp/PhenoRice/Processing/GR/ancillary/validation_maps/SAR/shape_rasterized2.tiff.tif',
          'D:/Temp/PhenoRice/Processing/Validation_Ermes/Input/GR_Static_info_2015_w_sar_final.shp',
          'int_id', stat="mean",2)

shp@data$diffsow <- shp@data$sowing_doy - shp@data$mean1
shp@data$diff <- shp@data$mean2 - shp@data$mean1
shp@data$diffmaj <- shp@data$sowing_doy - shp@data$maj1

names(shp@data)[17:26] = c("avg_sos","pixcnt1","maj_sos","z1","avg_pos","pixcnt","maj_pos","erravg","psss_diff","errmaj")

writeOGR(shp, dirname(out_shp), layer= sub("^([^.]*).*", "\\1", basename(out_shp)), driver="ESRI Shapefile", overwrite_layer = T)
}

 path.in.shp =  'D:/Temp/PhenoRice/Processing/Validation_Ermes/Input/GR_Static_info_2015_w_sar_All.shp'
 shp<-readOGR(path.in.shp, layer= sub("^([^.]*).*", "\\1", basename(path.in.shp)))
 names(shp@data)[17:26] = c("avg_sos","pixcnt1","maj_sos","z1","avg_pos","pixcnt","maj_pos","erravg","psss_diff","errmaj")
 data = shp@data

 data$type = 'All'

 path.in.shp =  'D:/Temp/PhenoRice/Processing/Validation_Ermes/Input/GR_Static_info_2015_w_sar_Asc.shp'
 shp<-readOGR(path.in.shp, layer= sub("^([^.]*).*", "\\1", basename(path.in.shp)))
 shp@data$type = 'ASC'
 names(shp@data)[17:26] = c("avg_sos","pixcnt1","maj_sos","z1","avg_pos","pixcnt","maj_pos","erravg","psss_diff","errmaj")
 data = rbind(data, shp@data)
 path.in.shp =  'D:/Temp/PhenoRice/Processing/Validation_Ermes/Input/GR_Static_info_2015_w_sar_DSC.shp'
 shp<-readOGR(path.in.shp, layer= sub("^([^.]*).*", "\\1", basename(path.in.shp)))
 shp@data$type = 'DSC'
 names(shp@data)[17:26] = c("avg_sos","pixcnt1","maj_sos","z1","avg_pos","pixcnt","maj_pos","erravg","psss_diff","errmaj")
 data = rbind(data, shp@data)
 data$type = as.factor(data$type)
 data$avg_sos =  data$avg_sos


  data$avg_sos [which(data$type != 'All')]=  data$avg_sos[which(data$type != 'All')] - 5.5
  data$maj_sos [which(data$type != 'All')]=  data$maj_sos[which(data$type != 'All')] - 5.5
  data$avg_pos [which(data$type != 'All')]=  data$avg_pos[which(data$type != 'All')] - 5.5
  data$maj_pos [which(data$type != 'All')]=  data$maj_pos[which(data$type != 'All')] - 5.5
  data$error_1_mean =  data$avg_sos-5.5
  data$error_2_mean =  data$avg_sos+5.5
  data$error_1_maj =  data$maj_sos-5.5
  data$error_2_maj =  data$maj_sos+5.5

  data$error_1_mean [which(data$type == 'All')]=  data$avg_sos [which(data$type == 'All')] -3
  data$error_2_mean [which(data$type == 'All')] =  data$avg_sos [which(data$type == 'All')] + 3
  data$error_1_maj [which(data$type == 'All')]=  data$maj_sos [which(data$type == 'All')] - 3
  data$error_2_maj [which(data$type == 'All')] =  data$maj_sos [which(data$type == 'All')]+ 3

  data$erravg <- data$sowing_doy - data$avg_sos
  data$psss_diff <- data$avg_pos - data$avg_sos
  data$diffmaj <- data$sowing_doy - data$maj_sos

  methods = c('Water',"Dry")

  diff_ps = 0
  stop_doy = 111

  data = droplevels(subset(data, crop_type =='Rice'))
  subdata = (subset(data, crop_type =='Rice' &
                      sowing_doy > 90 &
                      area > 0 &
                      pixcnt > 0 &
                      psss_diff >= diff_ps &
                      sowing_doy > stop_doy)) # &


                    #
                    # variety != 'Carnaroli' &
                    # variety != 'Mare CL'))
                    #
 Regr_Stats =  ddply(subdata, c('type'), function(df) regress_stats(df$sowing_doy, df$avg_sos ))
 eqn_strings =  ddply(Regr_Stats, c('type'), function(df) lm_eqn(df))

 p = ggplot(subdata,aes(x = sowing_doy, y = (avg_sos)))+facet_wrap(~type)
 p = p + geom_count(alpha = 0.3, position = position_dodge(width =0.9))+geom_errorbar(data = subdata,aes(ymin = error_1_mean, ymax =error_2_mean ))+theme_bw() + xlim(80, 180)+ylim(80,180)   #+geom_hex(binwidth = c(16,16))
 p = p + geom_abline(intercept = 0, slope = 1) + scale_size_continuous(range = c(2,8)) #+ scale_color_discrete(drop=FALSE)
 p = p + geom_abline(aes(intercept = Intercept, slope = Slope),data = Regr_Stats,linetype = 4, color = 'red', size = 0.75)
 p = p + geom_text(data = eqn_strings, aes(x = 87.5, y = 162.5, label = eq), parse = T, size = 4, hjust = 0)
 p = p + scale_y_continuous('SAR SOS - average',breaks = round(seq(87.5,162.5, by = 12.5),1),limits=c(87.5,162.5))+
   scale_x_continuous('Sowing Date',breaks = round(seq(87.5,162.5, by = 12.5),1),limits=c(87.5,162.5))
 p


 diff_ps = 24
 stop_doy = 90

 data = droplevels(subset(data, crop_type =='Rice'))
 subdata = (subset(data, crop_type =='Rice' &
                     sowing_doy > 90 &
                     area >= 0 &
                     pixcnt > 5 &
                     psss_diff > diff_ps &
                     sowing_doy > stop_doy)) # &
 #
 # variety != 'Carnaroli' &
 # variety != 'Mare CL'))
 #
 Regr_Stats =  ddply(subdata, c('type','sowing_met'), function(df) regress_stats(df$sowing_doy, df$avg_sos ))
 eqn_strings =  ddply(Regr_Stats, c('type','sowing_met'), function(df) lm_eqn(df))

 p = ggplot(subdata,aes(x = sowing_doy, y = (avg_sos)))+   facet_grid(type~sowing_met)
 p = p + geom_count(alpha = 0.2, posgrion = posgrion_dodge(width =0.9))+geom_errorbar(data = subdata,aes(ymin = error_1_mean, ymax =error_2_mean ))+theme_bw() + xlim(80, 180)+ylim(80,180)   #+geom_hex(binwidth = c(16,16))
 p = p + geom_abline(intercept = 0, slope = 1) + scale_size_continuous(range = c(2,8)) #+ scale_color_discrete(drop=FALSE)
 p = p + geom_abline(aes(intercept = Intercept, slope = Slope),data = Regr_Stats,linetype = 4, color = 'red', size = 0.75)
  p = p + geom_text(data = eqn_strings, aes(x = 87.5, y = 162.5, label = eq), parse = T, size = 4, hjust = 0) + xlim(80,162.5)+ ylim(80,162.5)
  p = p + scale_y_continuous('SAR SOS - average',breaks = round(seq(87.5,162.5, by = 12.5),1),limgrs=c(87.5,162.5))+
    scale_x_continuous('Sowing Date',breaks = round(seq(87.5,175, by = 12.5),1),limgrs=c(87.5,162.5))
  p



  diff_ps = 30
  stop_doy = 116.5

  data = droplevels(subset(data, crop_type =='Rice'))
  subdata = (subset(data, crop_type =='Rice' &
                      sowing_doy > 90 &
                      area >= 0 &
                      pixcnt > 15 &
                      psss_diff > diff_ps &
                      sowing_doy > stop_doy)) # &
#
Regr_Stats =  ddply(subdata, c('type','sowing_met'), function(df) regress_stats(df$sowing_doy, df$maj_sos ))
eqn_strings =  ddply(Regr_Stats, c('type','sowing_met'), function(df) lm_eqn(df))

p = ggplot(subdata,aes(x = sowing_doy, y = (maj_sos)))+   facet_grid(type~sowing_met)
p = p + geom_count(alpha = 0.2, position = position_dodge(width =0.9))+geom_errorbar(data = subdata,aes(ymin = error_1_maj, ymax =error_2_maj ))+theme_bw() + xlim(80, 180)+ylim(80,180)   #+geom_hex(binwidth = c(16,16))
p = p + geom_abline(intercept = 0, slope = 1) + scale_size_continuous(range = c(2,8)) #+ scale_color_discrete(drop=FALSE)
p = p + geom_abline(aes(intercept = Intercept, slope = Slope),data = Regr_Stats,linetype = 4, color = 'red', size = 0.75)
p = p + geom_text(data = eqn_strings, aes(x = 87.5, y = 175, label = eq), parse = T, size = 4, hjust = 0) + xlim(80,160)+ ylim(80,160)
p = p + scale_y_continuous('SAR SOS',breaks = round(seq(87.5,175, by = 12.5),1),limits=c(87.5,175))+
  scale_x_continuous('Sowing Date',breaks = round(seq(87.5,175, by = 12.5),1),limits=c(87.5,175))
p

#
lm_eqn = function(df){

 # eq <- substgrute(gralic(y) == a + b %.% gralic(x)*""~~gralic('r')~"="~r~~gralic('MAE')~"="~MAE~~gralic('RMSE')~"="~RMSE,
 #                  list(a = format(df$Intercept, diggrs = 3),
 #                       b = format(df$Slope, diggrs = 3),
 #                       r = format(df$r, diggrs = 3),
 #                       MAE = format(df$MAE, diggrs = 3),
 #                       RMSE= format(df$RMSE, diggrs = 3)
 #                  ))
 eq <- substitute(~~italic('N')~"="~N~~italic('r')~"="~r~~italic('ME')~"="~ME~~italic('MAE')~"="~MAE~~italic('RMSE')~"="~RMSE,
                  list(a = format(df$Intercept, digits = 3),
                       b = format(df$Slope, digits = 3),
                       r = format(df$r, digits = 3),
                       ME = format(df$ME, digits = 3),
                       MAE = format(df$MAE, digits = 3),
                       RMSE= format(df$RMSE, digits = 3),
                       N= format(df$N, digits = 3)
                  ))
 as.character(as.expression(eq))
 out = data.frame(eq = as.character(as.expression(eq)))
 out
 }

regress_stats = function( x= x, y = y) {

  # 	sub_y = which(y > 0 & x > 0)
  # 	y = y[sub_y]  		; 			x = x[sub_y]

  if (length(y[is.finite(y)] )>0 & length(x[is.finite(x)] )>0) {
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

#
# data = shp@data
# data$error_1_mean =  data$mean1
# data$error_2_mean =  data$mean1-12
#   data$error_1_maj =  data$maj1+0
# data$error_2_maj =  data$maj1-12
#
# subdata = subset(data, crop_type =='Rice' &
#                    sowing_doy > 90 &
#                    area >= 0 &
#                    diff > 0 &
#                    count1 > 5 &
#                    diff > 0 & sowing_met == 'Water')
# p = ggplot(subdata,aes(x = sowing_doy, y = (mean1-6), color = sowing_met))
# p = p + geom_point(alpha = 0.9)+geom_errorbar(aes(ymin = error_1_mean, ymax =error_2_mean ))+theme_bw() + xlim(80, 180)+ylim(80,180)   #+geom_hex(binwidth = c(16,16))
# p = p + geom_abline(intercept = 0, slope = 1)
# p


# D:/Temp/PhenoRice/Processing/Validation_Ermes/Input/GR_Static_info_2015_w_sar_final.shp
