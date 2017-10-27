Region_name <- "Region_3_-_Central_Luzon"

## "1) Get the polygons of a specific region from the shapefile") ----

in_vect <- dplyr::filter(in_shp, Region == Region_name)
# this is needed to remove duplicate polygons and keep only useful columns
# of the vector
in_vect <- unique(in_vect[c(1:4, 19)])
# reproject to the CRS of the rasters
in_vect <- reproj_vect(in_vect,get_proj4string(
  file.path(main_folder,"orig_mosaic/param_series/decirc/eos_decirc.tif")))

# in_vect contains one polygon for each sub_region of "Region_3_-_Central_Luzon"
plot_vect(in_vect, fill_var = "ID_name")

message("Create cropped rasters and put them in the \"subsets\" subfolder")

in_rast_folder  <- file.path(main_folder, "orig_mosaic/param_series/")
out_folder      <- file.path(main_folder, "subsets", Region_name)
make_folder(out_folder, type = "dirname", verbose = T)
out_folder

#  # you have all you need to crop the mosaics: folder of the mosaic rasters, a # vector where to cut and an output folder. Cropped rasters will be available
#  in out_folder

message("Extracting data on ", Region_name)
pr_extract_subarea(in_mosaics_folder = in_rast_folder,
                   in_mask           = in_vect,
                   out_folder        = out_folder)

## 2. Extract the data from the different provinces of the region" ----

in_files <- list.files(
  file.path(out_folder, "param_series/orig"),
  pattern = "*.RData",
  full.names = TRUE)
in_files

#### test on one file ----
in_rast <- get(load(in_files[5]))
message("Working on : ", in_files[5])
out <- sprawl::extract_rast(in_rast,
                            in_vect,
                            na.value = -32768,
                            join_geom = FALSE,
                            id_field = "ID_name",
                            verbose = FALSE)
out


## 3. Save results as an RData file for future use" ----
make_folder(file.path(out_folder, "RData"))
save(out, file = file.path(out_folder, "RData",
                           paste(Region_name, "sos", "stats.RData", sep = "_")))


out2 <- out$alldata %>%
  dplyr::mutate(price_seas = floor(band_n/14) + 1:4)

summdata <- out2 %>%
  mutate(sosdate = as.Date("2000-01-01") + value)

years <- 2002:2016
season <- cut((summdata$sosdate),
              breaks = as.Date(paste(years,"-01-12",sep="")),
              labels = paste(years[-length(years)],years[-length(years)]+1,sep="/"))

summdata <- out$alldata %>%
  mutate(date_fake = doytodate(value, 2003))


ggplot(summdata) +
  geom_histogram(aes (x = date_fake), binwidth = 8) +
  facet_wrap(~ID_name, scales = "free_y", ncol = 1) + theme_bw()

  # mutate(doy_fake = datetodoy(sosdate)) %>%
  # mutate(season = season) %>%
  # dplyr::filter(!season %in% c("2002/2003", "2001/2002"))

in_ratlas_data <- in_vect %>%
  dplyr::filter(Stage == "Planting")

  ggplot(summdata) +
  geom_histogram(aes(x = date_fake,
                      y= 100 * (..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..]),
binwidth = 8) +
  facet_wrap(~ID_name, ncol = 2) + theme_bw() +
  scale_x_date(date_labels = "%d %b", limits = c(as.Date("2002-11-01"), as.Date("2003-11-01"))) +
  scale_y_continuous("Frequency [%]") +
  geom_point(data = in_ratlas_data, aes(x = doytodate(Peak, 2003), color = as.factor(Seas_rd), y = 15)) +
  geom_errorbarh(data = in_ratlas_data, aes(xmin = doytodate(Start, 2003),
                                            xmax = doytodate(End, 2003),
                                            color = as.factor(Seas_rd), y = 15)) +
  scale_colour_manual("RiceAtlas", values = c("Red" ,"Green"))


  geom_point(data = in_ratlas_data, aes(x = doytodate(Start, 2003), color = Seas_rd), y = 600) +
  geom_point(data = in_ratlas_data, aes(x = doytodate(End, 2003), color = Seas_rd), y = 600)




ggplot(summdata, aes(x = date_fake, y = season, fill = season)) +
  geom_joy(scale = 2.0) +
  scale_x_date(limits = as.Date(c("2002-11-01", "2003-11-01")),
               date_labels = "%b %d") + theme_joy() +
  scale_fill_cyclical(values = c("gray80", "gray50")) +
  ggtitle("Nueva Ecija - Interannual variation of detected EOS")

summdata <- summdata %>%
  group_by(ID_name, date, price_seas) %>%
  summarize(avgsos = mean(date_fake))
