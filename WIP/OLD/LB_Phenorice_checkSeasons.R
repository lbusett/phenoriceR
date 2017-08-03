# TODO: Add comment
# 
# Author: LB
###############################################################################


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

main_folder = 'D:/Temp/PhenoRice/Processing/PHL/Outputs/2013/' 															# Folder used to store results of " Phenorice_PostProcess_Aggregate
results_folder = file.path(main_folder,'Validate')			# Where to put results

in_modgrid_df = get(load(file.path(results_folder,'out_df_modgrid.RData')))
