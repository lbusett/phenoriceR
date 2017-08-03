# Used to compute shapefiles, make anomaly maps and make graphs to be  sent to JRC about phenology


# Initialize ----
library(raster)
library(xts)
library(sp)
library(gdalUtils)
library(rgdal)
library(data.table)
library(dplyr)
library(plyr)
library(ggplot2)
library(reshape)
library(grid)
library(gridExtra)
library(hash)
library("scales")
library(tools)
library(stringr)
library(knitr)
library(maptools)
library(ireaRscripts)
library(lubridate)
library(RPostgreSQL)
countries = c('IT','ES','GR')
country = c('IT')

Main_Folder = "/home/lb/projects/ermes/datasets/rs_products/Phenology/%cc%/2016/v1.0/Outputs/ERMES_Grid"
Grid_Folder = "/home/lb/projects/ermes/datasets/ERMES_Folder_Structure/%cc%/Regional/%cc%_Reference_Grid/%cc%_ERMES_Regional_Grid.shp"
admin_shape = "/home/lb/projects/ermes/datasets/rs_products/Phenology/Ancillary_Datasets/World_Provinces/provinces_world_laea_ermes.shp"
# out_folder = file.path(str_replace_all(Main_Folder,"%cc%",country),'Summaries','pdf_and_graphs')


vers = '1.0'
selvar = c(1,1,1,1)

start_year = start_sel_years = 2003
end_year = end_sel_years = 2016
laea_crs = CRS("+init=epsg:3035 +proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0 +units=m +no_defs")
geo_WGS84_crs = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0,0,0,0 ")

ermes_grid = str_replace_all(Grid_Folder,"%cc%",country)
in_RData_file = file.path(str_replace_all(Main_Folder,"%cc%",country),'RData',paste0('ERMES_Aggregates_',start_year,'_',end_year,'.RData'))
data_in = get(load(file = in_RData_file))
selected = as.character(seq(start_sel_years,end_sel_years, 1))
data_in = droplevels(data_in[data_in$year %in% selected,])

# Compute values aggregated on administrative units



# Retieve spatialpointsdataframe of cells for selected country

ERMES_cells_poly = readOGR(dirname(ermes_grid) , basename(file_path_sans_ext(ermes_grid)))
ext_grd = extent(ERMES_cells_poly)

#  Retieve a polygon map of italian regions

mapit = readOGR(dirname(admin_shape),file_path_sans_ext(basename(admin_shape)),	drop_unsupported_fields=T)
mapit_df = fortify(mapit, region = "name")
mapit_data = data.frame(id = unique(mapit_df$id), value = rnorm(174))

# Add spatial info to the data_in fata frame + add some dummy variables
data_in$group = 1
data_in = join(data_in, ERMES_cells_poly@data, by = 'int_id')
data_in$variable = as.factor(data_in$variable)                 #  convert "variable" column to factor
data_in$year = as.factor(data_in$year)                 #  convert "year" column to factor
is.na(data_in) <- do.call(cbind,lapply(data_in, is.nan))
data_in = subset(data_in, !is.na(mean))																	# remove data with "NA" in the mean column
data_in[,"Date":=as.Date(strptime(paste("2008", data_in$mean), format="%Y %j") ),with=FALSE]
data_in$percol <- cut(100*data_in$perc_rice, breaks = c(0,1,10,20,30,40,50,60,70,80,90,100,110))     # catgorize the rice fc - 10 classes
data_in$variable = factor(data_in$variable,levels(data_in$variable)[c(3,4,1,2)])  # reorder the variables
data_in$rice_area = data_in$perc_rice*2000*2000           # compute retrieved area

data_in = subset(data_in, is_rice == 1)  # Consider only cells of "Rice" muncipalities
data_in$variable = factor(data_in$variable, levels = c( 'MinDoys','SoSDoys', 'MaxDoys', 'MaxVis' ))
levels(data_in$variable) = c( 'DOY of Sowing', 'DOY of Emergence', 'DOY of Heading', 'Value of VI at Heading')

mylabels = function(x){

  labels = lb_doytodate(x, 2003)
  labels = format(labels, "%b-%d")
  labels

}

# Build the plot for the rice fraction : Add points to the map, set colors and variables and set limits ----
ncols = 4
data_mindoy = droplevels(subset(data_in, variable == 'DOY of Sowing' & data_in$year %in% selected & perc_rice > 0.01))
# p <- ggplot(data = data_mindoy, aes(x = x_LAEA, y = y_LAEA))
# mapfract <- p + facet_wrap(~year, ncol = ncols)
# mapmin <- p + facet_wrap(~year, ncol = 4 )
# mapmin <- mapmin + geom_tile(aes(fill = mean))
# mapmin <- mapmin + scale_fill_gradientn('Doy of Sowing', limits=c(90, 160),colours = RColorBrewer::brewer.pal(10,"RdYlGn"), labels = mylabels)
# mapmin = mapmin +coord_fixed(xlim = c(ext_grd@xmin,ext_grd@xmax), ylim = c(ext_grd@ymin,ext_grd@ymax))
# mapmin = mapmin + theme_bw() + labs(title = "Doys of Sowing", x = "Longitude", y = "Latitude") +
#   theme(plot.title = element_text(size = 14, vjust = 1)) + labs(x = "Longitude") +
#   theme(axis.text.x  = element_text(size = 8), axis.text.y  = element_text(size = 8))+
#   theme(legend.position="right")+theme(plot.margin = unit(c(0.1,0.1,0.1,0.1), 'cm'))
# mapmin <- mapmin + geom_polygon(data = mapit_df,aes(x = long, y = lat, group = group),  fill = 'transparent', colour = "black")
# levels(data_in$variable)[1] = 'DOY of Sowing'
# print(mapmin)
# # mapfract <- mapfract + geom_tile(aes(fill = percol))
# # mapfract = mapfract +coord_fixed(xlim = c(ext_grd@xmin,ext_grd@xmax), ylim = c(ext_grd@ymin,ext_grd@ymax))
# # mapfract = mapfract + theme_bw() + labs(title = "Rice Cover Fraction", x = "Longitude", y = "Latitude")+ theme(plot.title = element_text(size = 14, vjust = 1))+ labs(x = "Longitude") + theme(axis.text.x  = element_text(size = 8) ,axis.text.y  = element_text(size = 8))+ theme(legend.position="right")+theme(plot.margin = unit(c(0.1,0.1,0.1,0.1), 'cm'))
# # # mapfract = mapfract + theme(plot.margin = unit(c(1,1,1,1), "cm"))
# # mapfract <- mapfract + scale_fill_brewer('Rice cover Fraction', palette="RdYlGn")
# # mapfract <- mapfract + geom_polygon(data = mapit_df,aes(x = long, y = lat, group = group),  fill = 'transparent', colour = "black")
# # print(mapfract)
#
#
# # Build the plot for the Sowing Dates ----
#
# # Build the plot for the mindoy maps : Add points to the map, set colors and variables and set limits
#
#
# # # Build the plot for the SOS Dates ----
#
# # Build the plot for the sos doy maps : Add points to the map, set colors and variables and set limits ----
# data_sos = droplevels(subset(data_in, variable == 'DOY of Emergence' & data_in$year %in% selected))
# p <- ggplot(data = data_sos, aes(x = x_LAEA, y = y_LAEA))
# mapsos <- p + facet_wrap(~year, ncol = ncols)
# mapsos <- mapsos + geom_tile(aes(fill = mean))
# mapsos <- mapsos + scale_fill_gradientn('Doy of Sowing', limits=c(105, 205),colours = RColorBrewer::brewer.pal(10,"RdYlGn"), labels = mylabels)
# mapsos = mapsos +coord_fixed(xlim = c(ext_grd@xmin,ext_grd@xmax), ylim = c(ext_grd@ymin,ext_grd@ymax))
# mapsos = mapsos + theme_bw() + labs(title = "DOY of Emergence", x = "Longitude", y = "Latitude")+
#   theme(plot.title = element_text(size = 14, vjust = 1)) + labs(x = "Longitude") +
#   theme(axis.text.x  = element_text(size = 8), axis.text.y  = element_text(size = 8))+
#   theme(legend.position="right")+theme(plot.margin = unit(c(0.1,0.1,0.1,0.1), 'cm'))
# mapsos <- mapsos + geom_polygon(data = mapit_df,aes(x = long, y = lat, group = group),  fill = 'transparent', colour = "black")
# levels(data_in$variable)[2] = 'DOY of Emergence'
# mapsos

# Build the boxplot of interannual variability of the four variables along years ----

blank_data <- data.frame(variable = rep(c("DOY of Sowing", "DOY of Sowing","DOY of Emergence","DOY of Emergence", "DOY of Heading", "DOY of Heading",
                                          "Value of VI at Heading", "Value of VI at Heading"), 14),  y = rep(as.Date(strptime(paste("2008",c(75,175,105,205,175,275,365,365)), format="%Y %j")),14), x = rep(levels(data_in$year), each = 2))
# blank_data$y = blank_data$y + sort((rep(seq(0,12),8)*365))
data_in$mean[which(data_in$variable == "DOY of Sowing" &  data_in$mean > 175)] = NA
data_in$mean[which(data_in$variable == "DOY of Sowing"&  data_in$mean < 90)] = NA
data_in$mean[which(data_in$variable == "DOY of Emergence"&  data_in$mean > 205)] = NA
data_in$mean[which(data_in$variable == "DOY of Emergence"&  data_in$mean < 100)] = NA
data_in$mean[which(data_in$variable == "DOY of Heading"&  data_in$mean > 275)] = NA
data_in$mean[which(data_in$variable == "DOY of Heading"&  data_in$mean < 175)] = NA

data_in$Date[which(data_in$variable == "DOY of Sowing" &  data_in$mean > 175)] = NA
data_in$Date[which(data_in$variable == "DOY of Sowing"&  data_in$mean < 90)] = NA
data_in$Date[which(data_in$variable == "DOY of Emergence"&  data_in$mean > 205)] = NA
data_in$Date[which(data_in$variable == "DOY of Emergence"&  data_in$mean < 10)] = NA
data_in$Date[which(data_in$variable == "DOY of Heading"&  data_in$mean > 275)] = NA
data_in$Date[which(data_in$variable == "DOY of Heading"&  data_in$mean < 175)] = NA

# Build the boxplot of interannual variability of the pheno variables along years - above 10% of cover ----

boxp2 = ggplot (data = droplevels(subset(data_in, Rice_fc > 0.10 & variable %in% c('DOY of Sowing'))), aes (x = year, y = Date))
boxp2 = boxp2 + geom_boxplot(outlier.colour = 'transparent') + facet_wrap(~variable, drop = T, ncol = 2) + theme_bw()
boxp2 = boxp2 + theme_bw() + labs(title = "Interannual Variability of Retrieved Variables - cells above 10 % cover", x = "Year", y = "Date")+
  theme(plot.title = element_text(size = 14, vjust = 1))+
  theme(axis.text.x  = element_text(size = 8, angle = 45, vjust = 0.5) ,axis.text.y  = element_text(size = 8))+ theme(legend.position="right")
# boxp2 = boxp2 + geom_blank(data = subset(blank_data, variable != 'Value of VI at Heading'), aes(x = x, y = y))
boxp2 = boxp2 + scale_y_date(labels = date_format("%b-%d"), limits = c(as.Date("2008-04-01"), as.Date("2008-06-15")))

#compute stats

stats = ddply(data_in, .(variable),summarize, avg = mean(Date, na.rm = T), sd = sd(Date, na.rm = T))

# Aggregate data on administrative units ----


#define input rasters

in_rast_ricemap_dir_temp = '/home/lb/projects/ermes/datasets/rs_products/Phenology/%cc%/Ancillary/Ricemap/'
in_rast_sow_dir_temp = "/home/lb/projects/ermes/datasets/rs_products/Phenology/%cc%/2016/v1.0/Outputs/ERMES_Grid/TIFFS/%yyyy%/MinDoys"
in_rast_flw_dir_temp = "/home/lb/projects/ermes/datasets/rs_products/Phenology/%cc%/2016/v1.0/Outputs/ERMES_Grid/TIFFS/%yyyy%/MaxDoys"
out_folder_shape = "/home/lb/ermes/data_processing/x_jrc/Shapefiles/fiveyears/%cc%/"
countries = c("ES","GR", "IT")

totdata = list()
counter = 0
nrworking_dir <- '//home/lb/nr_working'
ermes_dir <- '//home/lb/projects/ermes'

for (country in countries) {
  counter = counter + 1
  cc = tolower(country)

  # open the DB
  source(file.path(nrworking_dir,'luigi/code/Ermes/WARM_DB/opencon.R'))

  # Define schema and layer name of administrative areas layer

  schema_name = paste0("reg_",cc)
  table_name = paste0("administrative_areas")

  con <- dbConnect(dbDriver("PostgreSQL"), user=LB_WDB_opts$db_user,password=LB_WDB_opts$db_pwd,
                   dbname=LB_WDB_opts$db_name, host=LB_WDB_opts$db_address, port = LB_WDB_opts$db_port)

  in_rast_ricemap_dir = gsub("%cc%",country, in_rast_ricemap_dir_temp )
  in_rast_sow_dir_cc = gsub("%cc%", country,in_rast_sow_dir_temp)
  in_rast_flw_dir_cc = gsub("%cc%", country,in_rast_flw_dir_temp )
  out_folder_shape_cc = gsub("%cc%", country,out_folder_shape )

  sow_list = NULL
  flw_list = NULL
  in_rast_ricemap = list.files(in_rast_ricemap_dir, "\\.tif$", full.names = T)

  for(yy in seq(along = selected)) {

    # Build the stack of annual files
    in_rast_sow_dir = gsub("%yyyy%", selected[yy], in_rast_sow_dir_cc )
    in_rast_flw_dir = gsub("%yyyy%", selected[yy], in_rast_flw_dir_cc )
    in_rast_sow = list.files(in_rast_sow_dir, "\\.tif$", full.names = T)
    in_rast_flw = list.files(in_rast_flw_dir, "\\.tif$", full.names = T)
    sow_list = c(sow_list, in_rast_sow)
    flw_list = c(flw_list, in_rast_flw)
  }
  sow_stack = stack(sow_list)
  sow_stack = setZ(sow_stack,ymd("2003-01-01") %m+% months(seq(0,13,1)*12))
  flw_stack = stack(flw_list)
  flw_stack = setZ(flw_stack,ymd("2003-01-01") %m+% months(seq(0,13,1)*12))

  out_folder_shp = file.path(out_folder_shape_cc)
  dir.create(out_folder_shp, recursive = TRUE, showWarnings = FALSE)
  out_shapefile = file.path(out_folder_shp,"ERMES_Rice_Statistics.shp")
  aggr_data = lb_compute_pheno_stats_fiveyears(cc = cc, in_rast_ricemap = in_rast_ricemap ,in_rast_sow = sow_stack, in_rast_flw = flw_stack,
                                     out_folder = out_folder_shape_cc, out_shapefile = out_shapefile, sow = TRUE, area = TRUE, con = con)
  aggr_data$country = country
  totdata[[counter]] = aggr_data
  dbDisconnect(con)
}

file_out = "/home/lb/ermes/data_processing/x_jrc/statistics/aggregated_data.RData"
save(totdata, file = file_out)


# Compute average rasters

in_rast = sow_stack
avgsow = mean(in_rast)

# Plot the maps ----





#   dev.off()
#   }    #End Cycle on countries



# # Heading Dates
#
#
# ``` {r mapsmax, echo=FALSE, fig.height = 8, fig.cap = "Map of estimated Heading Dates on 2x2 km ERMES Grid Cells", hide = TRUE, message = FALSE, error = FALSE}
# if (selvar [3] == 1){
# 	# Build the plot for the maxdoy maps : Add points to the map, set colors and variables and set limits
#
# 	data_maxdoy = droplevels(subset(data_in, variable == 'DOY of Heading' & data_in$year %in% selected))
# 	p <- ggplot(data = data_maxdoy, aes(x = x_LAEA, y = y_LAEA))
# 	mapmax <- p + facet_wrap(~year, ncol = ncols)
# 	mapmax <- mapmax + geom_tile(aes(fill = mean))
# 	mapmax <- mapmax + scale_fill_gradientn('Doy of Heading',colours = topo.colors(10), limits=c(185, 285), oob=squish)
# 	mapmax = mapmax +coord_fixed(xlim = c(ext_grd@xmin,ext_grd@xmax), ylim = c(ext_grd@ymin,ext_grd@ymax))
# 	mapmax = mapmax + theme_bw() + labs(title = "Doys of Heading", x = "Longitude", y = "Latitude") +
# 		theme(plot.title = element_text(size = 14, vjust = 1)) + labs(x = "Longitude") +
# 		theme(axis.text.x  = element_text(size = 8), axis.text.y  = element_text(size = 8))+
# 		theme(legend.position="right")+theme(plot.margin = unit(c(0.1,0.1,0.1,0.1), 'cm'))
# 	mapmax <- mapmax + geom_polygon(data = mapit_df,aes(x = long, y = lat, group = group),  fill = 'transparent', colour = "black")
# 	levels(data_in$variable)[3] = 'DOY of Heading'
# 	mapmax
# }
# ```
#
# \newpage
#
#
# # Max VIs
#
#
# ``` {r mapsvi,  echo=FALSE, message = FALSE, error = FALSE, hide = TRUE, results = 'hide', warning = FALSE, fig.height = 8, fig.cap = "Map of Max EVI on 2x2 km ERMES Grid Cells"}
# # Build the plot for the maxvi maps : Add points to the map, set colors and variables and set limits
# if (selvar [4] == 1){
# 	data_maxvi = droplevels(subset(data_in, variable == 'Value of VI at Heading' & data_in$year %in% selected))
# 	p <- ggplot(data = data_maxvi, aes(x = x_LAEA, y = y_LAEA))
# 	mapvimax <- p + facet_wrap(~year, ncol = ncols)
# 	mapvimax <- mapvimax + geom_tile(aes(fill = mean))
# 	mapvimax <- mapvimax + scale_fill_gradientn('Value of VI at Heading',colours = topo.colors(10), limits=c(5000, 8000), oob=squish)
# 	mapvimax = mapvimax +coord_fixed(xlim = c(ext_grd@xmin,ext_grd@xmax), ylim = c(ext_grd@ymin,ext_grd@ymax))
# 	mapvimax = mapvimax + theme_bw() + labs(title = "Values of VI at Heading", x = "Longitude", y = "Latitude")+
# 		theme(plot.title = element_text(size = 14, vjust = 1)) + labs(x = "Longitude") +
# 		theme(axis.text.x  = element_text(size = 8), axis.text.y  = element_text(size = 8))+
# 		theme(legend.position="right")+theme(plot.margin = unit(c(0.1,0.1,0.1,0.1), 'cm'))
# 	mapvimax <- mapvimax + geom_polygon(data = mapit_df,aes(x = long, y = lat, group = group),  fill = 'transparent', colour = "black")
# 	levels(data_in$variable)[4] = 'Value of VI at Heading'
# 	mapvimax
# }
# ```
#
# \newpage
#
#
# # BoxPlots


