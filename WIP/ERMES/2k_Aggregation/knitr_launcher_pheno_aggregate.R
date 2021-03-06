library(raster)
library(sp)
library(gdalUtils)
library(rgdal)
library(data.table)
library(plyr)
library(ggplot2)
library(reshape)
library(grid)
library(gridExtra)
library(hash)
library("scales")
library(tools)
library(stringr)
library(maptools)
memory.limit(8000)
countries = c('IT','ES','GR')
countries = c('ES')

Main_Folder = "/home/lb/projects/ermes/datasets/rs_products/Phenology/%cc%/2016/v1.0/Outputs/ERMES_Grid"
Grid_Folder = "/home/lb/projects/ermes/datasets/ERMES_Folder_Structure/%cc%/Regional/%cc%_Reference_Grid/%cc%_ERMES_Regional_Grid.shp"
admin_shape = "/home/lb/projects/ermes/datasets/rs_products/Phenology/Ancillary_Datasets/World_Provinces/provinces_world_laea_ermes.shp"

a = 10

vers = '1.0'
selvar = c(1,1,1,1)

start_year = 2003
end_year 	 = 2016

start_sel_years = 2003
end_sel_years = 2016

laea_crs = CRS("+init=epsg:3035 +proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0 +units=m +no_defs")
geo_WGS84_crs = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0,0,0,0 ")

# savepdf <- function(file, width=24, height=29.7)
# { fname <- file
# pdf(fname, width=width/2.54, height=height/2.54,
#     pointsize=10)

for (country in countries) {
  out_folder =  file.path(str_replace_all(Main_Folder,"%cc%",country),'pdfs')
  dir.create(out_folder, recursive = T, showWarnings = F)
  dir.create(file.path(out_folder,paste0('Phenorice_',country,'_files'),'figure-latex'), recursive = T, showWarnings = F)
  rmarkdown::render('/home/lb/Source/git/phenorice/source/Postprocessing/R/ERMES/2k_Aggregation/LB_Plot_Aggregate_pheno_and_raster_out.Rmd',
                    output_file = file.path(out_folder, paste0('Phenorice_',country,'.pdf')))
}

