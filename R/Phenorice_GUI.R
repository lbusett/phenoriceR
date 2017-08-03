#' @title phenorice_GUI
#' @description FUNCTION_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @author Lorenzo Busetto, phD (2017) <lbusett@gmail.com>
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @export 
#' @import gWidgets
#' @import gWidgetsRGtk2
#' @rdname phenorice_GUI

phenorice_GUI <- function (){
  #- ------------------------------------------------------------------------------- -#
#  Start Building the GUI
#- ------------------------------------------------------------------------------- -#
  general_opts = list(in_folder = '', out_folder = '', mask_file = '', tempin_folder = '',
                      seas1_check = 'On', seas1_start_day = 1, seas1_end_day = 31, seas1_start_month = 1 ,  seas1_end_month = 3, seas1_start_doy = 1, seas1_end_doy = 1,
                      seas2_check = 'On', seas2_start_day = 1, seas2_end_day = 30, seas2_start_month = 4 ,  seas2_end_month = 6, seas2_start_doy = 1, seas2_end_doy = 1,
                      seas3_check = 'On', seas3_start_day = 1, seas3_end_day = 30, seas3_start_month = 7 ,  seas3_end_month = 9, seas3_start_doy = 1, seas3_end_doy = 1,
                      seas4_check = 'On', seas4_start_day = 1, seas4_end_day = 31, seas4_start_month = 10 , seas4_end_month = 12, seas4_start_doy = 1, seas4_end_doy = 1,
                      start_year = 2014, end_year = 2014,
                      avg_check = 'On', avg_thresh = 5500,
                      maxth_check = 'On',maxth_thresh = 4000, vi_decr_thresh = 0.55, #vi_decr_width = 8*10, #vi_decr_check = 'On',
                      minth_check = 'On',minth_thresh = 2500,
                      flood_check = 'On', flood_wid = '16',
                      lgt_check = 'On', lgtlow = 40, lgtup = 120,
                      maxeos_check = 'On', maxeoslow = 30, maxeosup = 80,
                      lst_check = 'On' ,lst_thresh = 15,
                      shape_check = 'On' ,shape_meth = 1, shape_thresh = 0.9, 
                      IDL_exe = '"C:/Program Files/Exelis/IDL82/bin/bin.x86_64/idl.exe"',
                      IDL_Dir = '"S:/source_code/IDL/Applications/Phenorice_v2/Phenorice_v22/source/"')
  require(gWidgets)
  options("guiToolkit"="RGtk2")
  args <- commandArgs(TRUE)
  
  RData_file =  file.path(args[1], 'Options_file.RData')
  txt_file =  file.path(args[1], 'Options_file.txt')
  
  
  if (file.exists(RData_file)) {load(RData_file)}
  
  {{main_win <- gbasicdialog(title = "Select PhenoRice Processing Options", parent=NULL, do.buttons=F,
                             visible = F, spacing = 10)
  main_group <- ggroup(container = main_win, horizontal = FALSE, expand = T)
  sel_prod <- general_opts$sel_prod
  }}
  
  #- ------------------------------------------------------------------------------- -#
  # Widgets for input/output folders selection
  #- ------------------------------------------------------------------------------- -#
  {{infold_frame <- gframe(text = '<span foreground="red" size="large">Input Folders and Files</span>', markup = T, container=main_group, expand = T,spacing = 15, horizontal = F)    			# Frame group
  infold_group <- ggroup(horizontal = TRUE, container=infold_frame)  				# Main group
  infold_lab <- glabel(text = 'Folder Containing MODIS Original Time Series: ', justify = "right" , container=infold_group, width = 57)
  infold_wid <- gedit(text = general_opts$in_folder, justify = "right" , container=infold_group, width = 57)			# Selected file
  
  fold_choose <- gbutton("Browse", handler=function(h,...) {
    
    choice = tryCatch(gfile(type="selectdir", text="Select the Output Folder for MODIS data...", abort = function() {}))		# File selection widget
    if(! is.na(choice)){
      svalue(infold_wid)<-choice						## On new selection, set value of the label widget
      general_opts$in_folder = format(choice, justify = "left")	# 	On new selection,  Set value of the selected variable
    }}, container=infold_group)
  }}
  
  
  
  mask_group <- ggroup(horizontal = TRUE, container=infold_frame)  				# Main group
  mask_lab <- glabel(text = 'Land Cover mask file (Optional): ', justify = "right" , container=mask_group, width = 57)
  addSpring(mask_group)
  mask_wid <- gedit(text = format(general_opts$mask_file, justify = "right") , container=mask_group, width = 57)			# Selected file
  fold_choose <- gbutton("Browse", handler=function(h,...) {choice<-gfile(type="open", text="Select the LC MAsk File...")		# File selection widget
  if(! is.na(choice)){svalue(mask_wid)<-choice						## On new selection, set value of the label widget
  general_opts$mask_file = format(choice, justify = "left")	# 	On new selection,  Set value of the selected variable
  }}, container=mask_group)
  
  tempinfold_group <- ggroup(horizontal = TRUE, container=infold_frame)  				# Main group
  tempinfold_lab <- glabel(text = 'Folder For Storage of Yearly Input Files: ', justify = "right" , container=tempinfold_group, width = 57)
  addSpring(tempinfold_group)
  tempinfold_wid <- gedit(text = general_opts$tempin_folder, justify = "right" , container=tempinfold_group, width = 57)			# Selected file
  fold_choose <- gbutton("Browse", handler=function(h,...) {choice<-gfile(type="selectdir", text="Select the Output Folder for MODIS data...")		# File selection widget
  if(! is.na(choice)){svalue(tempinfold_wid)<-choice						## On new selection, set value of the label widget
  general_opts$tempin_folder = format(choice, justify = "left")	# 	On new selection,  Set value of the selected variable
  }}, container=tempinfold_group)
  
  
  
  {{outfold_frame <- gframe(text = '<span foreground="red" size="large">Output File Prefix</span>',  markup = T, container=main_group, expand = T,spacing = 15, horizontal = F)    			# Frame group
  outfold_group <- ggroup(horizontal = TRUE, container=outfold_frame)  				# Main group
  
  outfold_lab <- glabel(text = 'Main Folder For Output Storage', justify = "right" , container=outfold_group, width = 57)
  addSpring(outfold_group)
  outfold_wid <- gedit(text = format(general_opts$out_folder, justify = "right") , container=outfold_group, width = 57)			# Selected file
  fold_choose <- gbutton("Browse", handler=function(h,...) {choice<-gfile(type="selectdir", text="Select the Output Filename.")		# File selection widget
  if(! is.na(choice)){svalue(outfold_wid)<-choice						## On new selection, set value of the label widget
  general_opts$out_folder = format(choice, justify = "left")	# 	On new selection,  Set value of the selected variable
  }}, container=outfold_group)
  }}
  
  
  
  #- ------------------------------------------------------------------------------- -#
  # Widgets for Season selection
  #- ------------------------------------------------------------------------------- -#
  
  {{seas_frame <- gframe(text = '<span foreground="red" size="large">Typical Heading Periods</span>', markup = T, container=main_group, expand = T,spacing = 15, horizontal = F)    			# Frame group
  
  #		check_seas = c('Winter','Spring','Summer','Autumn')
  #		seas_wid <-  gcheckboxgroup(items = check_seas, checked = as.logical(general_opts$sel_seasons), container = seas_group, horizontal = T)
  #
  seas1_group <- ggroup(horizontal = TRUE, container=seas_frame)  				# Main group
  seas1_lab_name <- glabel(text = '<span weight = "bold" >Winter  </span>',markup = T, container = seas1_group)
  seas1_lab <- glabel(text = 'Start Day',markup = T, container = seas1_group)
  
  start_day_wid_1   <- gspinbutton(1,31,  container=seas1_group , value = general_opts$seas1_start_day)
  seas1_lab <- glabel(text = 'Start Month',markup = T, container = seas1_group)
  start_month_wid_1 <- gspinbutton(-3,12,  container=seas1_group , value = general_opts$seas1_start_month)
  addSpace(seas1_group, 25, horizontal=TRUE)
  seas1_lab <- glabel(text = 'End Day',markup = T, container = seas1_group)
  end_day_wid_1   <- gspinbutton(1,31,  container=seas1_group , value = general_opts$seas1_end_day)
  seas1_lab <- glabel(text = 'End Month',markup = T, container = seas1_group)
  end_month_wid_1 <- gspinbutton(-3,12,  container=seas1_group , value = general_opts$seas1_end_month)
  addSpace(seas1_group, 25, horizontal=TRUE)
  addSpring(seas1_group)
  check_seas1 = gradio(items = c('On','Off'), text = 'Select', container=seas1_group, selected = match(general_opts$seas1_check, c('On','Off')), horizontal = T)
  
  seas2_group <- ggroup(horizontal = TRUE, container=seas_frame)  				# Main group
  seas1_lab_name <- glabel(text = '<span weight = "bold" >Spring  </span>',markup = T, container = seas2_group)
  seas2_lab <- glabel(text = 'Start Day',markup = T, container = seas2_group)
  
  start_day_wid_2   <- gspinbutton(1,31,  container=seas2_group , value = general_opts$seas2_start_day)
  seas2_lab <- glabel(text = 'Start Month',markup = T, container = seas2_group)
  start_month_wid_2 <- gspinbutton(1,12,  container=seas2_group , value = general_opts$seas2_start_month)
  addSpace(seas2_group, 25, horizontal=TRUE)
  seas2_lab <- glabel(text = 'End Day',markup = T, container = seas2_group)
  end_day_wid_2   <- gspinbutton(1,31,  container=seas2_group , value = general_opts$seas2_end_day)
  seas2_lab <- glabel(text = 'End Month',markup = T, container = seas2_group)
  end_month_wid_2 <- gspinbutton(1,12,  container=seas2_group , value = general_opts$seas2_end_month)
  addSpace(seas2_group, 25, horizontal=TRUE)
  addSpring(seas2_group)
  check_seas2 = gradio(items = c('On','Off'), text = 'Select', container=seas2_group, selected = match(general_opts$seas2_check, c('On','Off')), horizontal = T)
  
  seas3_group <- ggroup(horizontal = TRUE, container=seas_frame)  				# Main group
  seas1_lab_name <- glabel(text = '<span weight = "bold" >Summer  </span>',markup = T, container = seas3_group)
  seas3_lab <- glabel(text = 'Start Day',markup = T, container = seas3_group)
  
  start_day_wid_3   <- gspinbutton(1,31,  container=seas3_group , value = general_opts$seas3_start_day)
  seas3_lab <- glabel(text = 'Start Month',markup = T, container = seas3_group)
  start_month_wid_3 <- gspinbutton(1,12,  container=seas3_group , value = general_opts$seas3_start_month)
  addSpace(seas3_group, 25, horizontal=TRUE)
  seas3_lab <- glabel(text = 'End Day',markup = T, container = seas3_group)
  end_day_wid_3   <- gspinbutton(1,31,  container=seas3_group , value = general_opts$seas3_end_day)
  seas3_lab <- glabel(text = 'End Month',markup = T, container = seas3_group)
  end_month_wid_3 <- gspinbutton(1,12,  container=seas3_group , value = general_opts$seas3_end_month)
  addSpace(seas3_group, 25, horizontal=TRUE)
  addSpring(seas3_group)
  check_seas3 = gradio(items = c('On','Off'), text = 'Select', container=seas3_group, selected = match(general_opts$seas3_check, c('On','Off')), horizontal = T)
  
  seas4_group <- ggroup(horizontal = TRUE, container=seas_frame)  				# Main group
  seas1_lab_name <- glabel(text = '<span weight = "bold" >Autumn  </span>',markup = T, container = seas4_group)
  seas4_lab <- glabel(text = 'Start Day',markup = T, container = seas4_group)
  
  start_day_wid_4   <- gspinbutton(1,31,  container=seas4_group , value = general_opts$seas4_start_day)
  seas4_lab <- glabel(text = 'Start Month',markup = T, container = seas4_group)
  start_month_wid_4 <- gspinbutton(1,12,  container=seas4_group , value = general_opts$seas4_start_month)
  addSpace(seas4_group, 25, horizontal=TRUE)
  seas4_lab <- glabel(text = 'End Day',markup = T, container = seas4_group)
  end_day_wid_4   <- gspinbutton(1,31,  container=seas4_group , value = general_opts$seas4_end_day)
  seas4_lab <- glabel(text = 'End Month',markup = T, container = seas4_group)
  end_month_wid_4 <- gspinbutton(1,12,  container=seas4_group , value = general_opts$seas4_end_month)
  addSpace(seas4_group, 25, horizontal=TRUE)
  addSpring(seas4_group)
  check_seas4 = gradio(items = c('On','Off'), text = 'Select', container=seas4_group, selected = match(general_opts$seas4_check, c('On','Off')), horizontal = T)
  
  
  }}
  
  
  years_frame = gframe(text = '<span foreground="red" size="large">Select Start and End Years</span>', markup = T, container=main_group, expand = T,spacing = 15)    			# Frame group
  start_date_lab <- glabel(text = '<span weight = "bold" >Starting Year:</span>',markup = T, container = years_frame)  ; size (start_date_lab ) = c(120,20)
  start_year_wid <- gspinbutton(2000 ,2020,  container=years_frame , value = general_opts$start_year, horizontal = T)
  End_date_lab <- glabel(text = '<span weight = "bold" >Ending Year:</span>',markup = T, container = years_frame)  ; size (start_date_lab ) = c(120,20)
  end_year_wid <- gspinbutton(2000 ,2020,  container=years_frame , value = general_opts$end_year, horizontal = T)
  
  {{criteria_frame <- gframe(text = '<span foreground="red" size="large">Select Criteria for Rice Signal Detection</span>',
                             markup = T, container=main_group, expand = T,spacing = 15, horizontal = F)    			# Frame group
  
  criteria_group <- ggroup(horizontal = FALSE, container=criteria_frame)  				# Main group
  avg_group <- ggroup(horizontal = TRUE, container=criteria_group)  				# Main group
  avg_lab <- glabel(text = 'Average VI Value Below Threshold:', justify = "right" , container=avg_group, width = 57)
  avg_wid <- gedit(text = format(general_opts$avg_thresh, justify = "right") , container=avg_group, width = 8)
  addSpring(avg_group)
  avg_check = gradio(items = c('On','Off'), text = 'Select', container=avg_group, selected = match(general_opts$avg_check, c('On','Off')), horizontal = T)
  
  maxth_group <- ggroup(horizontal = TRUE, container=criteria_group, expand = T)  				# Main group
  
  maxth_lab <- glabel(text = 'VI of Max must be above:                ', justify = "right" , container=maxth_group, width =50)
  maxth_wid <- gedit(text = format(general_opts$maxth_thresh, justify = "right") , container=maxth_group, width = 8)
  addSpring(maxth_group)
  maxth_check = gradio(items = c('On','Off'), text = 'Select', container=maxth_group, selected = match(general_opts$maxth_check, c('On','Off')), horizontal = T)
  
  minth_group <- ggroup(horizontal = TRUE, container=criteria_group)  				# Main group
  minth_lab <- glabel(text = 'VI of min must be below:                ', justify = "right" , container=minth_group, width = 8)
  minth_wid <- gedit(text = format(general_opts$minth_thresh, justify = "right") , container=minth_group, width = 8)
  addSpring(minth_group)
  minth_check = gradio(items = c('On','Off'), text = 'Select', container=minth_group, selected = match(general_opts$minth_check, c('On','Off')), horizontal = T)
  
  flood_group <- ggroup(horizontal = TRUE, container=criteria_group)  				# Main group
  flood_lab <- glabel(text = 'Flooding Detected within:               ', justify = "right" , container=flood_group,  width = 8)
  flood_wid <- gedit(text = format(general_opts$flood_wid, justify = "right") , container=flood_group, width = 8)
  flood_lab2 <- glabel(text = ' Days from minimum ', justify = "right" , container=flood_group, width = 8)
  #		vi_decr_wid2 <- gedit(text = format(general_opts$vi_decr_width, justify = "right") , container=vi_decr_group, width = 8)
  #		vi_decr_lab2<- glabel(text = 'Days', container=vi_decr_group)
  addSpring(flood_group)
  flood_check = gradio(items = c('On','Off'), text = 'Select', container=flood_group, selected = match(general_opts$flood_check, c('On','Off')), horizontal = T)
  #
  
  lgt_group <- ggroup(horizontal = TRUE, container=criteria_group)  				# Main group
  lgt_lab <- glabel(text = 'Vegetative Season Length -   Min: ', justify = "right" , container=lgt_group, width = 8)
  lgtlow_wid <- gspinbutton(from = 30, to = 120, by = 8, container=lgt_group,horizontal =F, width = 30, value = general_opts$lgtlow )
  lgt_lab <- glabel(text = ' Max: ', justify = "right" , container=lgt_group, width = 8)
  lgtup_wid <- gspinbutton(from = 30, to = 120, by = 8, container=lgt_group,horizontal =F, width = 30 , value = general_opts$lgtup )
  addSpring(lgt_group)
  lgt_check = gradio(items = c('On','Off'), text = 'Select', container=lgt_group, selected = match(general_opts$lgt_check, c('On','Off')), horizontal = T)
  
  maxeos_group <- ggroup(horizontal = TRUE, container=criteria_group)  				# Main group
  maxeos_lab <- glabel(text = 'Maturity to Harvest Length - Min: ', justify = "right" , container=maxeos_group, width = 8)
  maxeoslow_wid <- gspinbutton(from = 25, to = 120, by = 8, container=maxeos_group,horizontal =F, width = 30, value = general_opts$maxeoslow )
  maxeos_lab <- glabel(text = ' Max: ', justify = "right" , container=maxeos_group, width = 8)
  maxeosup_wid <- gspinbutton(from = 25, to = 120, by = 8, container=maxeos_group,horizontal =F, width = 30 , value = general_opts$maxeosup )
  vi_decr_lab <- glabel(text = '% of decrease to EOS :', justify = "right" , container=maxeos_group, maxeos_group = 8)
  vi_decr_wid <- gedit(text = format(general_opts$vi_decr_thresh, justify = "right") , container=maxeos_group, width = 8)
  addSpring(maxeos_group)
  maxeos_check = gradio(items = c('On','Off'), text = 'Select', container=maxeos_group, selected = match(general_opts$maxeos_check, c('On','Off')), horizontal = T)
  
  lst_group = ggroup(horizontal = TRUE, container=criteria_group)  				# Main group
  lst_lab <- glabel(text = 'LST on Min must be above:         ', justify = "right" , container=lst_group, width = 8)
  lst_wid <- gedit(text = format(general_opts$lst_thresh, justify = "right") , container=lst_group, width = 8)
  lst_lab2 <- glabel(text = ' Degrees C', justify = "right" , container=lst_group, width = 8)
  addSpace(lst_group, 135, horizontal=TRUE)
  addSpring(lst_group)
  lst_check = gradio(items = c('On','Off'), text = 'Select', container=lst_group, selected = match(general_opts$lst_check, c('On','Off')), horizontal = T)
  
  
  shape_group <- ggroup(horizontal = TRUE, container=criteria_group)  				# shape group
  
  shape_lab <- glabel(text = 'Check shape method: ', justify = "right" , container=shape_group, width = 8)
  shape_wid <- gdroplist(c("hypertan","linear"), selected = general_opts$shape_meth, container=shape_group) 
  shape_lab2 <- glabel(text = 'R2 threshold: ', justify = "right" , container=shape_group, width = 8)
  shapethresh_wid <- gspinbutton(from = 0, to = 1, by = 0.05, container=shape_group,horizontal =F, width = 30, value = general_opts$shape_thresh )
  addSpace(shape_group, 50, horizontal=TRUE)
  addSpring(shape_group)
  shape_check = gradio(items = c('On','Off'), text = 'Select', container=shape_group, selected = match(general_opts$shape_check, c('On','Off')), horizontal = T)
  
  
  
  
  }}
  
  {{but_group <- ggroup(container = main_group, horizontal = TRUE)
  #
  {{start_but <- gbutton(text = 'Start', container = but_group, handler = function (h,....) {# If "Start" pressed, retrieve selected values and save in previous file
    
    #								general_opts = list(in_folder = '', out_folder = '', mask_file = '', sel_seasons = c(0,1,1,0), avg_check = 'On', avg_thresh = 5500,
    #										maxth_check = 'On',maxth_thresh = 5000, vi_decr_thresh = 0.66, vi_decr_width = 8*16, vi_decr_check = 'Off',
    #										minth_check = 'On',minth_thresh = 2500, minth_check = 'On',
    #										flood_check = 'On', flood_wid = '16',
    #										minmax_check = 'On', minmaxlow = 8*6, minmaxup = 8*15,
    #										lst_check = 'On' ,lst_thresh = 15)
    
    general_opts$in_folder = svalue(infold_wid)
    general_opts$tempin_folder = svalue(tempinfold_wid)
    general_opts$out_folder = svalue(outfold_wid)
    general_opts$mask_file = svalue(mask_wid)
    
    general_opts$seas1_check = svalue(check_seas1)
    general_opts$seas1_start_day = svalue(start_day_wid_1)
    general_opts$seas1_end_day = svalue(end_day_wid_1)
    general_opts$seas1_start_month = svalue(start_month_wid_1)
    general_opts$seas1_end_month = svalue(end_month_wid_1)
    
    if (svalue(start_month_wid_1) > 0) {
      today = as.Date(paste(svalue(start_day_wid_1),svalue(start_month_wid_1),'2014', sep = '/'), format("%d/%m/%Y"))
      general_opts$seas1_start_doy <- strftime(today, format = "%j")
    } else {
      
      today = as.Date(paste(svalue(start_day_wid_1),svalue(start_month_wid_1)+12,'2014', sep = '/'), format("%d/%m/%Y"))
      general_opts$seas1_start_doy <- as.numeric(strftime(today, format = "%j"))-365
      # browser()
    }
    # today = as.Date(paste(svalue(start_day_wid_1),start_month,'2014', sep = '/'), format("%d/%m/%Y"))
    # general_opts$seas1_start_doy <- strftime(today, format = "%j")
    
    
    if (svalue(end_month_wid_1) > 0) {
      today = as.Date(paste(svalue(end_day_wid_1),svalue(end_month_wid_1),'2014', sep = '/'), format("%d/%m/%Y"))
      general_opts$seas1_end_doy <- strftime(today, format = "%j")
    } else {
      
      today = as.Date(paste(svalue(end_day_wid_1),svalue(end_month_wid_1)+12,'2014', sep = '/'), format("%d/%m/%Y"))
      general_opts$seas1_end_doy <- as.numeric(strftime(today, format = "%j"))-365
      # browser()
    }
    
    
    general_opts$seas2_check = svalue(check_seas2)
    general_opts$seas2_start_day = svalue(start_day_wid_2)
    general_opts$seas2_end_day = svalue(end_day_wid_2)
    general_opts$seas2_start_month = svalue(start_month_wid_2)
    general_opts$seas2_end_month = svalue(end_month_wid_2)
    today = as.Date(paste(svalue(start_day_wid_2),svalue(start_month_wid_2),'2014', sep = '/'), format("%d/%m/%Y"))
    general_opts$seas2_start_doy <- strftime(today, format = "%j")
    today = as.Date(paste(svalue(end_day_wid_2),svalue(end_month_wid_2),'2014', sep = '/'), format("%d/%m/%Y"))
    general_opts$seas2_end_doy <- strftime(today, format = "%j")
    
    general_opts$seas3_check = svalue(check_seas3)
    general_opts$seas3_start_day = svalue(start_day_wid_3)
    general_opts$seas3_end_day = svalue(end_day_wid_3)
    general_opts$seas3_start_month = svalue(start_month_wid_3)
    general_opts$seas3_end_month = svalue(end_month_wid_3)
    today = as.Date(paste(svalue(start_day_wid_3),svalue(start_month_wid_3),'2014', sep = '/'), format("%d/%m/%Y"))
    general_opts$seas3_start_doy <- strftime(today, format = "%j")
    today = as.Date(paste(svalue(end_day_wid_3),svalue(end_month_wid_3),'2014', sep = '/'), format("%d/%m/%Y"))
    general_opts$seas3_end_doy <- strftime(today, format = "%j")
    
    general_opts$seas4_check = svalue(check_seas4)
    general_opts$seas4_start_day = svalue(start_day_wid_4)
    general_opts$seas4_end_day = svalue(end_day_wid_4)
    general_opts$seas4_start_month = svalue(start_month_wid_4)
    general_opts$seas4_end_month = svalue(end_month_wid_4)
    today = as.Date(paste(svalue(start_day_wid_4),svalue(start_month_wid_4),'2014', sep = '/'), format("%d/%m/%Y"))
    general_opts$seas4_start_doy <- strftime(today, format = "%j")
    today = as.Date(paste(svalue(end_day_wid_4),svalue(end_month_wid_4),'2014', sep = '/'), format("%d/%m/%Y"))
    general_opts$seas4_end_doy <- strftime(today, format = "%j")
    
    general_opts$start_year = svalue(start_year_wid)
    general_opts$end_year = svalue(end_year_wid)
    
    #								general_opts$sel_seasons = svalue(seas_wid, index = T)
    general_opts$avg_check = svalue(avg_check)
    general_opts$avg_thresh = svalue(avg_wid)
    
    general_opts$maxth_check = svalue(maxth_check)
    general_opts$maxth_thresh = svalue(maxth_wid)
    
    # general_opts$vi_decr_check = svalue(vi_decr_check)
    general_opts$vi_decr_thresh = svalue(vi_decr_wid)
    # general_opts$vi_decr_width = svalue(vi_decr_wid2)
    
    general_opts$minth_check = svalue(minth_check)
    general_opts$minth_thresh = svalue(minth_wid)
    
    general_opts$flood_check = svalue(flood_check)
    general_opts$flood_wid = svalue(flood_wid)
    
    general_opts$lgt_check = svalue(lgt_check)
    general_opts$lgtlow = svalue(lgtlow_wid)
    general_opts$lgtup = svalue(lgtup_wid)
    
    general_opts$maxeos_check = svalue(maxeos_check)
    general_opts$maxeoslow = svalue(maxeoslow_wid)
    general_opts$maxeosup = svalue(maxeosup_wid)
    
    general_opts$lst_check = svalue(lst_check)
    general_opts$LSsT_thresh = svalue(lst_wid)
    
    general_opts$shape_check = svalue(shape_check)
    general_opts$shape_met = svalue(shape_wid)
    general_opts$shape_thresh = svalue(shapethresh_wid)
    
    save(general_opts, file = RData_file)
    dir.create(general_opts$out_folder, recursive = T)
    #	txt_file = file.path(general_opts$out_folder,'phenorice_options.txt')
    unlink(txt_file)
    lapply(general_opts, write, txt_file, append=TRUE, ncolumns=1000)
    print("Start")
    dispose(main_win)
    
  })
  }}
  
  save_but <- gbutton(text = 'Save Options', container = but_group, handler = function(h,...){
    choice <- gfile(type = "save", text = "Select a File for saving options")		# File selection widget
    if (!is.na(choice)) {
      general_opts$in_folder = svalue(infold_wid)
      general_opts$tempin_folder = svalue(tempinfold_wid)
      general_opts$out_folder = svalue(outfold_wid)
      general_opts$mask_file = svalue(mask_wid)
      
      general_opts$seas1_check = svalue(check_seas1)
      general_opts$seas1_start_day = svalue(start_day_wid_1)
      general_opts$seas1_end_day = svalue(end_day_wid_1)
      general_opts$seas1_start_month = svalue(start_month_wid_1)
      general_opts$seas1_end_month = svalue(end_month_wid_1)
      
      if (svalue(start_month_wid_1) > 0) {
        today = as.Date(paste(svalue(start_day_wid_1),svalue(start_month_wid_1),'2014', sep = '/'), format("%d/%m/%Y"))
        general_opts$seas1_start_doy <- strftime(today, format = "%j")
      } else {
        
        today = as.Date(paste(svalue(start_day_wid_1),svalue(start_month_wid_1)+12,'2014', sep = '/'), format("%d/%m/%Y"))
        general_opts$seas1_start_doy <- as.numeric(strftime(today, format = "%j"))-365
        # browser()
      }
      # today = as.Date(paste(svalue(start_day_wid_1),start_month,'2014', sep = '/'), format("%d/%m/%Y"))
      # general_opts$seas1_start_doy <- strftime(today, format = "%j")
      
      
      if (svalue(end_month_wid_1) > 0) {
        today = as.Date(paste(svalue(end_day_wid_1),svalue(end_month_wid_1),'2014', sep = '/'), format("%d/%m/%Y"))
        general_opts$seas1_end_doy <- strftime(today, format = "%j")
      } else {
        
        today = as.Date(paste(svalue(end_day_wid_1),svalue(end_month_wid_1)+12,'2014', sep = '/'), format("%d/%m/%Y"))
        general_opts$seas1_end_doy <- as.numeric(strftime(today, format = "%j"))-365
        # browser()
      }
      
      
      general_opts$seas2_check = svalue(check_seas2)
      general_opts$seas2_start_day = svalue(start_day_wid_2)
      general_opts$seas2_end_day = svalue(end_day_wid_2)
      general_opts$seas2_start_month = svalue(start_month_wid_2)
      general_opts$seas2_end_month = svalue(end_month_wid_2)
      today = as.Date(paste(svalue(start_day_wid_2),svalue(start_month_wid_2),'2014', sep = '/'), format("%d/%m/%Y"))
      general_opts$seas2_start_doy <- strftime(today, format = "%j")
      today = as.Date(paste(svalue(end_day_wid_2),svalue(end_month_wid_2),'2014', sep = '/'), format("%d/%m/%Y"))
      general_opts$seas2_end_doy <- strftime(today, format = "%j")
      
      general_opts$seas3_check = svalue(check_seas3)
      general_opts$seas3_start_day = svalue(start_day_wid_3)
      general_opts$seas3_end_day = svalue(end_day_wid_3)
      general_opts$seas3_start_month = svalue(start_month_wid_3)
      general_opts$seas3_end_month = svalue(end_month_wid_3)
      today = as.Date(paste(svalue(start_day_wid_3),svalue(start_month_wid_3),'2014', sep = '/'), format("%d/%m/%Y"))
      general_opts$seas3_start_doy <- strftime(today, format = "%j")
      today = as.Date(paste(svalue(end_day_wid_3),svalue(end_month_wid_3),'2014', sep = '/'), format("%d/%m/%Y"))
      general_opts$seas3_end_doy <- strftime(today, format = "%j")
      
      general_opts$seas4_check = svalue(check_seas4)
      general_opts$seas4_start_day = svalue(start_day_wid_4)
      general_opts$seas4_end_day = svalue(end_day_wid_4)
      general_opts$seas4_start_month = svalue(start_month_wid_4)
      general_opts$seas4_end_month = svalue(end_month_wid_4)
      today = as.Date(paste(svalue(start_day_wid_4),svalue(start_month_wid_4),'2014', sep = '/'), format("%d/%m/%Y"))
      general_opts$seas4_start_doy <- strftime(today, format = "%j")
      today = as.Date(paste(svalue(end_day_wid_4),svalue(end_month_wid_4),'2014', sep = '/'), format("%d/%m/%Y"))
      general_opts$seas4_end_doy <- strftime(today, format = "%j")
      
      general_opts$start_year = svalue(start_year_wid)
      general_opts$end_year = svalue(end_year_wid)
      
      #								general_opts$sel_seasons = svalue(seas_wid, index = T)
      general_opts$avg_check = svalue(avg_check)
      general_opts$avg_thresh = svalue(avg_wid)
      
      general_opts$maxth_check = svalue(maxth_check)
      general_opts$maxth_thresh = svalue(maxth_wid)
      
      # general_opts$vi_decr_check = svalue(vi_decr_check)
      general_opts$vi_decr_thresh = svalue(vi_decr_wid)
      # general_opts$vi_decr_width = svalue(vi_decr_wid2)
      
      general_opts$minth_check = svalue(minth_check)
      general_opts$minth_thresh = svalue(minth_wid)
      
      general_opts$flood_check = svalue(flood_check)
      general_opts$flood_wid = svalue(flood_wid)
      
      general_opts$minmax_check = svalue(minmax_check)
      general_opts$minmaxlow = svalue(minmaxlow_wid)
      general_opts$minmaxup = svalue(minmaxup_wid)
      
      general_opts$lst_check = svalue(lst_check)
      general_opts$LSsT_thresh = svalue(lst_wid)
      
      general_opts$shape_check = svalue(shape_check)
      general_opts$shape_met = svalue(shape_wid)
      general_opts$shape_thresh = svalue(shapethresh_wid)
      
      save(general_opts, file = RData_file)
      dir.create(general_opts$out_folder, recursive = T)
      #txt_file = file.path(general_opts$out_folder,'phenorice_options.txt')
      if (file.exists(txt_file)) {unlink(txt_file)}
      lapply(general_opts, write, txt_file, append=TRUE, ncolumns=1000)
      save(general_opts, file = choice)
      
    }
  })
  
  load_but <- gbutton(text = 'Load Options', container = but_group, handler = function(h,...){
    choice <- gfile(type = "open", text = "Select a File for loading options")		# File selection widget
    
    if (!is.na(choice)) {
      load(choice)
      
      svalue(infold_wid) = general_opts$in_folder 
      svalue(tempinfold_wid) = general_opts$tempin_folder 
      svalue(outfold_wid) = general_opts$out_folder 
      svalue(mask_wid) = general_opts$mask_file
      
      svalue(check_seas1) =  general_opts$seas1_check
      svalue(start_day_wid_1) = general_opts$seas1_start_day 
      svalue(end_day_wid_1) = general_opts$seas1_end_day 
      svalue(start_month_wid_1) = general_opts$seas1_start_month
      svalue(end_month_wid_1) = general_opts$seas1_end_month 
      svalue(check_seas2) = general_opts$seas2_check 
      svalue(start_day_wid_2) = general_opts$seas2_start_day 
      svalue(end_day_wid_2) = general_opts$seas2_end_day 
      svalue(start_month_wid_2) = general_opts$seas2_start_month 
      svalue(end_month_wid_2) = general_opts$seas2_end_month 
      
      svalue(check_seas3) = general_opts$seas3_check 
      svalue(start_day_wid_3) = general_opts$seas3_start_day
      svalue(end_day_wid_3) = general_opts$seas3_end_day
      svalue(start_month_wid_3) = general_opts$seas3_start_month
      svalue(end_month_wid_3) = general_opts$seas3_end_month 
      
      svalue(check_seas4) = general_opts$seas4_check
      svalue(start_day_wid_4) = general_opts$seas4_start_day  
      svalue(end_day_wid_4) = general_opts$seas4_end_day
      svalue(start_month_wid_4) = general_opts$seas4_start_month
      svalue(end_month_wid_4) = general_opts$seas4_end_month 
      
      svalue(start_year_wid) = general_opts$start_year
      svalue(end_year_wid) = general_opts$end_year
      
      svalue(avg_check) = general_opts$avg_check
      svalue(avg_wid) = general_opts$avg_thresh  
      
      svalue(maxth_check) = general_opts$maxth_check 
      
      # svalue(vi_decr_check) = general_opts$vi_decr_check
      svalue(vi_decr_wid) = general_opts$vi_decr_thresh 
      # svalue(vi_decr_wid2) = general_opts$vi_decr_width 
      
      svalue(minth_check) = general_opts$minth_check
      svalue(minth_wid) = general_opts$minth_thresh 
      
      svalue(flood_check) = general_opts$flood_check 
      svalue(flood_wid) = general_opts$flood_wid 
      
      svalue(lgt_check) = general_opts$lgt_check 
      svalue(lgtlow_wid) = general_opts$lgtlow 
      svalue(lgtup_wid) = general_opts$lgtup 
      
      svalue(maxeos_check) = general_opts$maxeos_check 
      svalue(maxeoslow_wid) = general_opts$maxeoslow 
      svalue(maxeosup_wid) = general_opts$maxeosup 
      
      svalue(lst_check) = general_opts$lst_check 
      svalue(lst_wid) = general_opts$LSsT_thresh 
      
      svalue(shape_check) = general_opts$shape_check 
      svalue(shape_wid) = general_opts$shape_meth
      svalue(shapethresh_wid)= general_opts$shape_thresh
      
      save(general_opts, file = RData_file)
      
      if (file.exists(txt_file)) {unlink(txt_file)}
      lapply(general_opts, write, txt_file, append=TRUE, ncolumns=1000)
      
    }
  })
  
  quit_but <- gbutton(text = 'Quit', container = but_group, handler = function(h,...){
    assign("Quit", T, envir=globalenv())
    print("Quit")
    dispose(main_win)
  })
  }}
  
  
  visible(main_win, set=TRUE) ## show the selection GUI
  
}
