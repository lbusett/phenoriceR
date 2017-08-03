# TODO: Add comment
# 
# Author: LB
###############################################################################


library(raster)
out_folder = 'D:/Temp/PHL_Analysis/Phenorice_Outputs/Output_t12/2013/raster'
in_folder ='D:/Temp/PHL_Analysis/Phenorice_Outputs/Input_subset/2013'

in_VI = brick(list.files(in_folder, glob2rx('EVI_ts*dat'), full.names  = T))
in_smooth_vi = brick(list.files(in_folder, glob2rx('*VI_smooth*dat'), full.names  = T))
in_NDFI = brick(list.files(in_folder, glob2rx('*NDFI*dat'), full.names  = T))
#out_min_arr = list.files(in_folder, '*map_max*')
#out_max_arr = list.files(in_folder, '*map_min*')
out_mindoy = brick(list.files(out_folder, glob2rx('*map_min*dat'), full.names  = T))
out_maxdoy = brick(list.files(out_folder, glob2rx('*map_max*dat'), full.names  = T))


doys_in = c( 4559.000000, 4567.000000, 4575.000000, 4583.000000, 4591.000000, 4599.000000,
		4607.000000, 4615.000000, 4623.000000, 4631.000000, 4639.000000, 4647.000000,
		4655.000000, 4663.000000, 4671.000000, 4679.000000, 4687.000000, 4695.000000,
		4703.000000, 4711.000000, 4719.000000, 4727.000000, 4735.000000, 4743.000000,
		4749.000000, 4757.000000, 4765.000000, 4773.000000, 4781.000000, 4789.000000,
		4797.000000, 4805.000000, 4813.000000, 4821.000000, 4829.000000, 4837.000000,
		4845.000000, 4853.000000, 4861.000000, 4869.000000, 4877.000000, 4885.000000,
		4893.000000, 4901.000000, 4909.000000, 4917.000000, 4925.000000, 4933.000000,
		4941.000000, 4949.000000, 4957.000000, 4965.000000, 4973.000000, 4981.000000,
		4989.000000, 4997.000000, 5005.000000, 5013.000000, 5021.000000, 5029.000000,
		5037.000000, 5045.000000, 5053.000000, 5061.000000, 5069.000000, 5077.000000,
		5085.000000, 5093.000000, 5101.000000, 5109.000000)-365*13+1

b = out_mindoy[[1]]
a = plot(b)
ext = zoom (b)
a = plot(crop(b,ext))
pos = click()
sel_pix = SpatialPoints(coordinates(pos), proj4string = crs("+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs"))
pix_VI  = as.numeric(extract(in_VI,sel_pix)) ; dim(pix_VI) = 70
pix_smooth  = as.numeric(extract(in_smooth_vi,sel_pix)) ; dim(pix_smooth) = 70
pix_mindoy = as.numeric(extract(out_mindoy,sel_pix)) ; dim(pix_mindoy) = 4
pix_maxdoy = as.numeric(extract(out_maxdoy,sel_pix)) ; dim(pix_maxdoy) = 4
 
plot_df = data.frame(doy = doys_in, VI = pix_VI, vi_smooth = pix_smooth,min_q1 = pix_mindoy[1]
,min_q2 = pix_mindoy[2],min_q3 = pix_mindoy[3],min_q4 = pix_mindoy[4]
,max_q2 = pix_maxdoy[2],max_q3 = pix_maxdoy[3],max_q4 = pix_maxdoy[4])


p  = ggplot(plot_df, aes( x = doy))
p = p + geom_point(aes(y = pix_VI)) + geom_line(aes(y = pix_smooth))
p

13583847.2364, 1244014.33462