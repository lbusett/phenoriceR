it_shape_file_15 = 'D:/Temp/PhenoRice/Processing/Validation_Ermes/Input/IT_Static_info_2015.shp'
it_shape_15 = readOGR (dirname(it_shape_file_15), file_path_sans_ext(basename(it_shape_file_15)))
type = "ASC"

if (type == "Both" ){
  in_raster = 'D:/Temp/PhenoRice/Processing/IT/ancillary/validation_maps/SAR/A+D_first6_MSOS'
  in_raster2 = 'D:/Temp/PhenoRice/Processing/IT/ancillary/validation_maps/SAR/A+D_first6_MPOS'
  out_shp = 'D:/Temp/PhenoRice/Processing/Validation_Ermes/Input/IT_Static_info_2015_w_sar_All.shp'
  out_rdata = 'd:/temp/phenorice/processing/Validation_Ermes/IT_15_SAR_All.RData'
  values(in_raster) = (values(in_raster)-2)*6+78
  values(in_raster)[which(values(in_raster) < 85)] =NA
  values(in_raster2) = (values(in_raster2)-2)*6+78
  values(in_raster2)[which(values(in_raster2) < 85)] =NA
  small_15 = crop(in_raster, it_shape_15)
  small_15_pos = crop(in_raster2, it_shape_15)
  dir.create('D:/Temp/PhenoRice/Processing/IT/ancillary/validation_maps/SAR/All/')
  writeRaster(small_15,'D:/Temp/PhenoRice/Processing/IT/ancillary/validation_maps/SAR/All/SOS_Small_All.tif', overwrite = TRUE)
  writeRaster(small_15_pos,'D:/Temp/PhenoRice/Processing/IT/ancillary/validation_maps/SAR/All/POS_Small_All.tif', overwrite = TRUE)
}

if (type == "ASC" ){
  in_raster = 'F:/sarmap_12_2015/A2_SoS'
  in_raster2 = 'F:/sarmap_12_2015/A2_PoS'
  out_shp = 'D:/Temp/PhenoRice/Processing/Validation_Ermes/Input/IT_Static_info_2015_w_sar_Asc.shp'
  out_rdata = 'd:/temp/phenorice/processing/Validation_Ermes/IT_15_SAR_Asc.RData'
  values(in_raster) = (values(in_raster)-2)*12+83
  values(in_raster)[which(values(in_raster) < 83)] =NA
  values(in_raster2) = (values(in_raster2)-2)*6+83
  values(in_raster2)[which(values(in_raster2) < 85)] =NA
  small_15 = crop(in_raster, it_shape_15)
  small_15_pos = crop(in_raster2, it_shape_15)
  dir.create('D:/Temp/PhenoRice/Processing/IT/ancillary/validation_maps/SAR/ASC/')
  writeRaster(small_15,'D:/Temp/PhenoRice/Processing/IT/ancillary/validation_maps/SAR/ASC/SOS_Small_ASC.tif', overwrite = TRUE)
  writeRaster(small_15_pos,'D:/Temp/PhenoRice/Processing/IT/ancillary/validation_maps/ASC/SAR/POS_Small_ASC.tif', overwrite = TRUE)
}

if (type == "DSC" ){
  in_raster = 'F:/sarmap_12_2015/D2_SoS'
  in_raster2 = 'F:/sarmap_12_2015/D2_PoS'
  out_shp = 'D:/Temp/PhenoRice/Processing/Validation_Ermes/Input/IT_Static_info_2015_w_sar_DSC.shp'
  out_rdata = 'd:/temp/phenorice/processing/Validation_Ermes/IT_15_SAR_Dsc.RData'
  values(in_raster) = (values(in_raster)-2)*12+110
  values(in_raster)[which(values(in_raster) < 110)] =NA
  values(in_raster2) = (values(in_raster2)-2)*6+110
  values(in_raster2)[which(values(in_raster2) < 110)] =NA
  small_15 = crop(in_raster, it_shape_15)
  small_15_pos = crop(in_raster2, it_shape_15)
  dir.create('D:/Temp/PhenoRice/Processing/IT/ancillary/validation_maps/SAR/DSC/')
  writeRaster(small_15,'D:/Temp/PhenoRice/Processing/IT/ancillary/validation_maps/SAR/DSC/SOS_Small_DSC.tif', overwrite = TRUE)
  writeRaster(small_15_pos,'D:/Temp/PhenoRice/Processing/IT/ancillary/validation_maps/SAR/DSC/POS_Small_DSC.tif', overwrite = TRUE)
}


myZonal <- function (x, z, stat, digits = 0, na.rm = TRUE,...) {
  library(data.table)
  fun <- match.fun(stat)
  vals <- getValues(x)
  zones <- round(getValues(z), digits = digits)
  rDT <- data.table(vals, z=zones)
  setkey(rDT, z)
  rDT[, lapply(.SD, fun, na.rm = T), by=z]
}

ZonalPipe <- function (path.in.shp, path.in.r, path.out.r, path.out.shp, zone.attribute, stat){

  # 1/ Rasterize using GDAL

  #Initiate parameter
  r<-stack(path.in.r)

  ext<-extent(r)
  ext<-paste(ext[1], ext[3], ext[2], ext[4])

  res<-paste(res(r)[1], res(r)[2])

  #Gdal_rasterize
  command<-'gdal_rasterize'
  command<-paste(command, "--config GDAL_CACHEMAX 2000") #Speed-up with more cache (avice: max 1/3 of your total RAM)
  command<-paste(command, "-a", zone.attribute) #Identifies an attribute field on the features to be used for a burn in value. The value will be burned into all output bands.
  command<-paste(command, "-te", as.character(ext)) #(GDAL >= 1.8.0) set georeferenced extents. The values must be expressed in georeferenced units. If not specified, the extent of the output file will be the extent of the vector layers.
  command<-paste(command, "-tr", res) #(GDAL >= 1.8.0) set target resolution. The values must be expressed in georeferenced units. Both must be positive values.
  # command<-paste(command, "-overwrite") #(GDAL >= 1.8.0) set target resolution. The values must be expressed in georeferenced units. Both must be positive values.
  command<-paste(command, path.in.shp)
  command<-paste(command, path.out.r)

  system(command)

  # 2/ Zonal Stat using myZonal function
  zone<-raster(path.out.r)

  Zstat<-data.frame(myZonal(r, zone, stat))
  colnames(Zstat)[2:length(Zstat)]<-paste0("B", c(1:(length(Zstat)-1)), "_",stat)

  # 3/ Merge data in the shapefile and write it
  shp<-readOGR(path.in.shp, layer= sub("^([^.]*).*", "\\1", basename(path.in.shp)))

  shp@data <- data.frame(shp@data, Zstat[match(shp@data[,zone.attribute], Zstat[, "z"]),])
  writeOGR(shp, path.out.shp, layer= sub("^([^.]*).*", "\\1", basename(path.in.shp)), driver="ESRI Shapefile", overwrite_layer = T)
  return(shp)

}

shp = ZonalPipe(it_shape_file_15,
          'D:/Temp/PhenoRice/Processing/IT/ancillary/validation_maps/SAR/SOS_Small.tif',
          'D:/Temp/PhenoRice/Processing/IT/ancillary/validation_maps/SAR/shape_rasterized.tiff.tif',
          'D:/Temp/PhenoRice/Processing/Validation_Ermes/Input/IT_Static_info_2015_w_sar_temp.shp',
          'int_id', stat="mean")

shp = ZonalPipe('D:/Temp/PhenoRice/Processing/Validation_Ermes/Input/IT_Static_info_2015_w_sar_temp.shp',
          'D:/Temp/PhenoRice/Processing/IT/ancillary/validation_maps/SAR/POS_Small.tif',
          'D:/Temp/PhenoRice/Processing/IT/ancillary/validation_maps/SAR/shape_rasterized2.tiff.tif',
          'D:/Temp/PhenoRice/Processing/Validation_Ermes/Input/IT_Static_info_2015_w_sar_final.shp',
          'int_id', stat="mean")

shp@data$diffsow <- shp@data$sowing_doy - shp@data$B1_mean
shp@data$diff <- shp@data$B1_mean.1 - shp@data$B1_mean

writeOGR(shp, dirname(out_shp), layer= sub("^([^.]*).*", "\\1", basename(out_shp)), driver="ESRI Shapefile", overwrite_layer = T)

data = shp@data

p = ggplot(subset(data, crop_type =='Rice' & sowing_doy > 0 & diff > 30 & diff <200 ),aes(x = sowing_doy, y = (B1_mean)))
p = p + geom_point()+theme_bw() + xlim(80, 160)+ylim(80,160)   #+geom_hex(binwidth = c(16,16))
p

it_ras_fields_15 = extract(small_15,it_shape_15,weights=F,normalizeWeights=F, small = F,  na.rm = T,
                           fun = function(x,...)names(which.max(table(x))),df = T)
it_ras_fields_15_count = extract(small_15,it_shape_15, small = F,  na.rm = T, fun =function(x,...)length(x),df = T)
it_ras_fields_15_dev = extract(small_15,it_shape_15, small = F,  na.rm = T, fun =sd,df = T)

save(it_ras_fields_15, file = out_rdata)



it_ras_fields_15$int_id = it_shape_15@data$int_id

it_ras_fields_15_count$int_id = it_shape_15@data$int_id
it_ras_fields_15_dev$int_id = it_shape_15@data$int_id
names(it_ras_fields_15)[2] ='Sow_MOD'
names(it_ras_fields_15_count)[2] = 'count'
names(it_ras_fields_15_dev)[2] =  'dev'
it_ras_fields_15$Sow_MOD = as.numeric(as.character(it_ras_fields_15$Sow_MOD ))

joined_15 = join(it_shape_15@data, it_ras_fields_15, type = 'left')
joined_15 = join(joined_15, it_ras_fields_15_count, type = 'left')
joined_15 = join(joined_15, it_ras_fields_15_dev, type = 'left')
# names(joined_15)[17] = 'Sow_MOD'
joined_15$Sow_MOD[which(is.nan(joined_15$Sow_MOD) != 0)] = NA
joined_15$diff = joined_15$Sow_MOD - joined_15$sowing_doy

sub_15 = droplevels(subset(joined_15, crop_type =='Rice' & is.na(sowing_doy) ==FALSE ) )
levels(sub_15$sowing_met) = c("Dry","Water","Unknown")
sub_15$sowing_met[which(is.na(sub_15$sowing_met))] = 'Unknown'
sub_15 = droplevels(subset(sub_15, sowing_met !='Unknown'))
sub_15$sowing_met[which(sub_15$sowing_met == 'Unknown' & sub$sowing_doy < 120)] = 'Dry'
sub_15$sowing_met[which(sub_15$sowing_met == 'Unknown' & sub$sowing_doy >= 120)] = 'Water'
joinedmelt_15 = melt(sub_15, measure.vars = c("sowing_doy","Sow_MOD","diff"))

p = ggplot(joinedmelt_15, aes(x = sowing_met ,y = value, fill = variable ))
p = p + geom_boxplot()
p

sub_sub = droplevels(subset(sub_15, sowing_met =='Dry' & count >100))

