library(plyr)
library(ggplot2)
library(reshape2)
library(tools)
library(rgdal)
library(raster)
library(data.table)

it_shape_file_13 = '//10.0.1.252/nr_working/luigi/data/Phenorice/campionamento2013/campitureunite_sos.shp'
it_raster_file_13 = 'Y:/ermes/datasets/rs_products/Phenology/IT/Outputs/v1.0/2013/raster/old_min_identification_Full_Out_2013.dat'
it_2km_tif_13 = 'Y:/ermes/datasets/rs_products/Phenology/IT/Outputs/v4.0/ERMES_Grid/TIFFS/2013/MinDoys/IT_Phenology_MinDoys_2013_004.tif'

it_grid = 'Y:/ermes/datasets/ERMES_Folder_Structure/IT/Regional/IT_Reference_Grid/IT_ERMES_Regional_Grid.shp'

it_shape_13 = readOGR (dirname(it_shape_file_13), file_path_sans_ext(basename(it_shape_file_13)))
it_rast_13 = raster(it_raster_file_13, band = 2)
it_2km_13 = raster(it_2km_tif_13)
it_grid = readOGR(dirname(it_grid), file_path_sans_ext(basename(it_grid)))
NAvalue(it_rast_13) = 0
it_ras_fields_13 = extract(it_rast_13,it_shape_13,weights=TRUE,fun = mean, small = T,  na.rm = T, df = T)
it_ras_fields_13_count = extract(it_rast_13,it_shape_13,weights=F, small = T,  na.rm = T, fun =function(x,...)length(x), df = T)
save(it_ras_fields_13, file = 'd:/temp/phenorice/processing/Validation_Ermes/IT_13_Luigi.RData')

# ras_fields_stdev = extract(in_rast,in_shape,fun = sd, na.rm = T, df = T)
it_ras_fields_13$ID = it_shape_13@data$ID
it_shape_13@data$area = sapply(slot(it_shape_13, "polygons"), slot, "area")/10000
joined_13 = join(it_shape_13@data, it_ras_fields_13, type = 'left')
names(joined_13)[11] = 'Sow_MOD'
joined_13$Sow_MOD[which(joined_13$Sow_MOD == 0)] = NA
joined_13$Unf_est0 = joined_13$Unf_est0 - 10
joined_13$diff = joined_13$Sow_MOD - (joined_13$Unf_est0)

sub_13 = droplevels(subset(joined_13, is.na(Unf_est0) ==FALSE & area >6 & is.na(diff) ==F))
# levels(sub_13$sowing_met) = c("Dry","Water","Unknown")
# sub_13$sowing_met[which(is.na(sub_13$sowing_met))] = 'Unknown'
# sub_13 = droplevels(subset(sub_13, sowing_met !='Unknown'))
# sub_13$sowing_met[which(sub_13$sowing_met == 'Unknown' & sub$sowing_doy < 120)] = 'Dry'
# sub_13$sowing_met[which(sub_13$sowing_met == 'Unknown' & sub$sowing_doy >= 120)] = 'Water'
joinedmelt_13 = melt(sub_13, measure.vars = c("Unf_est0","Sow_MOD","diff"))

p = ggplot(joinedmelt_13, aes(x = variable ,y = value, fill = variable ))
p = p + geom_boxplot()
p+geom_jitter()


sub_13$year = 2013
joinedmelt_13$year = 2013

stats = ddply(joinedmelt_13, .(variable) ,summarize, avg = mean(value, na.rm = T), stdev = sd(value, na.rm = T), mae = mean(abs(value),na.rm = T))

p = ggplot(droplevels(subset(joinedmelt_13, variable !='diff')), aes(x = variable ,y = value, fill = variable ))
p = p + geom_violin()+geom_jitter()+facet_wrap(~year)

p = ggplot(droplevels(subset(joinedmelt_13, variable !='diff')), aes(x = value, color = variable ))
p = p+geom_histogram(binwidth = 8, fill = 'transparent')+theme_bw()+facet_wrap(~variable)
p = p + geom_freqpoly(binwidth = 8)+theme_bw()
#p = p+geom_boxplot(adjust = 1.4)+facet_grid(variable~sowing_met)+theme_bw()+geom_jitter()
p = p+geom_density(alpha = 0.2, adjust = 1.5)+facet_grid(~sowing_met)+theme_bw()
p


