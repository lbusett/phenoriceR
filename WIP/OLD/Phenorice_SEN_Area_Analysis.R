# TODO: Add comment
#
# Author: LB
###############################################################################



#---- Load libraries and load data from results of LB_Phenorice_plot_dates.R ----

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
		library(scales)
		library(latticeExtra)

		main_folder = 'D:/Temp/PhenoRice/Processing/SEN/Outputs/New_Elab2/RData'															# Folder used to store results of "
		results_folder = file.path(main_folder,'RData')			# Where to put results
		country_code = 'sen'
		sel_quart = c()
		out_RData_file = file.path(main_folder, paste0('phenorice_stats_multyyear2014.RData'))
		load(out_RData_file)


		data = droplevels(subset(results_stats_N, Adm_Name != 'NA' & Adm_Name != 'Dakar'  & Adm_Name != 'Pikine'))
		totdata = ddply(data, .(year), summarise, totarea = sum(n_isrice)*231.656358*231.656358/10000)

		p_totarea = ggplot(totdata, aes(x = as.character(year), y = totarea))
		p_totarea = p_totarea + geom_bar(stat = 'identity') + theme_bw()
		p_totarea

		datamelt = melt (data, measure.vars = c('n_1season','n_2seasons','n_3seasons','n_4seasons'))
		datamelt$value = datamelt$value*231.656358*231.656358/10000
		p_area = ggplot(data = datamelt, aes (x = year, y = value, fill = variable )) + facet_wrap(~Adm_Name)
		p_area = p_area + theme_bw() + ylab('Detected Rice Area [ha]')
		p_area = p_area + geom_bar(stat = 'identity')+ theme(axis.text.x = element_text(angle = 90 , vjust = 0, hjust = 1))
		p_area + scale_x_continuous(breaks = seq(2004,2014,1))

		data_avgs =  droplevels(subset(results_stats, Adm_Name != 'NA' & Adm_Name != 'Dakar'  & Adm_Name != 'Pikine'))


		# Plot area variability separated by quarters
		data = droplevels(subset(results_stats, Adm_Name != 'NA' & Adm_Name != 'Dakar'  & Adm_Name != 'Pikine'))
		totdata = ddply(data, .(year), summarise, totarea = sum(n_pixels)*231.656358*231.656358/10000)
		totdata_seas = ddply(data, .(year,metric ), summarise, totarea = sum(n_pixels)*231.656358*231.656358/10000)
		totdata_seas = droplevels(subset(totdata_seas, metric %in% c('Min_DOY_1st_Quarter','Min_DOY_2nd_Quarter','Min_DOY_3rd_Quarter','Min_DOY_4th_Quarter')))

		p_area_seas = ggplot(totdata_seas, aes(x = year, y = totarea, fill = metric))
		p_area_seas = p_area_seas + geom_bar(stat = 'identity')+theme_bw()+ scale_fill_hue('Seasons (WRT Sowing Dates)', labels = c('Other','Summer','Winter'))
		p_area_seas+ scale_x_continuous(breaks = seq(2004,2014,1))

		pro = melt(data,value,.name = "DOY", measure.vars = c('Min_DOY_1st_Quarter','Min_DOY_2nd_Quarter','Min_DOY_3rd_Quarter','Min_DOY_4th_Quarter'), na.rm = T )
		names(pro)[length(names(pro))-1] = 'Quarter'
		names(pro)[length(names(pro))] = 'DOY'
		pro$Date = as.Date(pro$DOY - 1, origin = "2013-01-01")

		data_area = ddply(pro, .(Quarter, Adm_Name), summarize, area = 231.656358^2*length(DOY)/10000)
		p_area = ggplot(data = data_area, aes (x = Adm_Name, y = area, fill = Quarter ))
		p_area = p_area + theme_bw() + ylab('Detected Rice Area [ha]')
		p_area = p_area + geom_bar(stat = 'identity')+ theme(axis.text.x = element_text(angle = 90 , vjust = 0, hjust = 1))
		p_area = p_area + scale_fill_hue('Seasons', labels = c('Other','Summer','Winter'))




