library(sprawl)
library(tibble)
library(raster)
library(phenoriceR)
library(dplyr)
library(sf)
library(dplyr)

# set the main folder ----
main_folder <- "/home/lb/my_data/prasia/Data"
in_rast_folder  <- file.path(main_folder, "orig_mosaic/param_series/")

in_shp <- read_vect(file.path(main_folder,
                              "vector/Ricetlas/riceatlas_asia_reshuffled.shp"))
Regions <- unique(in_shp$Region)

for (reg in Regions[2:3]) {
  message("- ---------------------------------------- - ")
  message("- Working on: ", reg)
  message("- ---------------------------------------- - ")
  in_vect <- dplyr::filter(in_shp, Region == reg)
  # this is needed to remove duplicate polygons and keep only useful columns
  # of the vector
  in_vect <- unique(in_vect[c(1:4, 19)])
  # reproject to the CRS of the rasters
  in_vect <- reproj_vect(in_vect,get_proj4string(
    file.path(main_folder,"orig_mosaic/param_series/decirc/eos_decirc.tif")))
  out_folder      <- file.path(main_folder, "subsets", reg)
  make_folder(out_folder, type = "dirname", verbose = F)
  pr_extract_subarea(in_mosaics_folder = in_rast_folder,
                     in_mask           = in_vect,
                     out_folder        = out_folder)
}


