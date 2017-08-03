
myZonal <- function (x, z, stat,cut, digits = 0, na.rm = TRUE, ...) {
  library(data.table)
  fun <- match.fun(stat)
  vals <- getValues(x)
  if (cut == T) {quant = quantile(vals, c(0.06,0.95), na.rm = T)
  vals[which(vals < quant[1] )] = NA
  vals[which(vals > quant[2])] = NA
  }
  zones <- round(getValues(z), digits = digits)
  rDT <- data.table(vals, z=zones)
  setkey(rDT, z)
  rDT[, lapply(.SD, fun, na.rm = T), by=z]
}
ZonalPipe<- function (path.in.shp, path.in.r, path.out.r, path.out.shp, zone.attribute, stat,cut){

  # 1/ Rasterize using GDAL
  countna = function(x, na.rm) {length(which(is.na(x) ==F))}
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
  command<-paste(command, path.in.shp)
  command<-paste(command, path.out.r)

  system(command)

  # 2/ Zonal Stat using myZonal function
  zone<-raster(path.out.r)

  Zstat<-data.frame(myZonal(r, zone, stat,cut))
  colnames(Zstat)[2:length(Zstat)]<-paste0("B", c(1:(length(Zstat)-1)), "_",stat)

  # 3/ Merge data in the shapefile and write it
  shp<-readOGR(path.in.shp, layer= sub("^([^.]*).*", "\\1", basename(path.in.shp)))

  shp@data <- data.frame(shp@data, Zstat[match(shp@data[,zone.attribute], Zstat[, "z"]),])
  return(shp)
  # writeOGR(shp, path.out.shp, layer= sub("^([^.]*).*", "\\1", basename(path.in.shp)), driver="ESRI Shapefile")
}
