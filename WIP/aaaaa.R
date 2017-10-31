library(sf)
library(dplyr)
library(sprawl)
library(phenorice)
main_folder <- "/home/lb/my_data/prasia/Data"

in_shp <- read_vect(file.path(main_folder,
                              "vector/Ricetlas/riceatlas_asia_reshuffled.shp"))

Region_name <- "Region_3_-_Central_Luzon"

in_vect <- dplyr::filter(in_shp, Region == Region_name)

# Get the polygons of a specific region from the shapefile
in_vect <- in_vect[1:3] %>%
  sf::st_transform(get_proj4string(
    file.path(main_folder,"orig_mosaic/param_series/decirc/eos_decirc.tif"))) %>%
  unique()

plot_vect(in_vect, fill_var = "ID")

mosaic_folder  <- file.path(main_folder, "orig_mosaic/param_series/")
subsets_folder <- file.path(main_folder, "subsets")
make_folder(subsets_folder, type = "dirname", verbose = T)

pr_extract_subarea(mosaic_folder,
                   in_region,
                   subset_name = Region,
                   out_folder  = subsets_folder,
                   what = "decirc")
Region_name <- "Region_3_-_Central_Luzon"
in_files <- list.files(
  file.path(subsets_folder, Region_name, "param_series/orig"),
  pattern= "*.RData",
  full.names = TRUE)

in_rast <- get(load(in_files[5]))

out <- sprawl::extract_rast(in_rast,
                            in_vect,
                            na.value = -32768,
                            join_geom = FALSE,
                            id_field = "ID")

out2 <- out$alldata %>%
  dplyr::filter(!is.na(value))  %>%
  dplyr::mutate(sosdate = as.Date("2000-01-01") + value,
                fc = N/n_pix,
                area = 2146.58*fc) %>%
  dplyr::arrange(ID, band_n, date) %>%
  dplyr::group_by(ID, band_n, date) %>%
  summarize(meanval = mean(value)) %>%
  dplyr::mutate(sosdate = as.Date("2000-01-01") + meanval)


















library(sprawl)
library(tibble)
library(raster)
library(phenoriceR)
library(dplyr)
library(sf)
library(dplyr)
Region_name <- "Region_3_-_Central_Luzon"

# set the main folder ----
main_folder <- "/home/lb/my_data/prasia/Data"

# Load the reshuffled shapefile ----

in_shp <- read_vect(file.path(main_folder,
                              "vector/Ricetlas/riceatlas_asia_reshuffled.shp"))

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
  dplyr::mutate(price_seas = floor(a/14) + 1:4)

summdata <- out2 %>%
  mutate(sosdate = as.Date("2000-01-01") + value)

years <- 2002:2016
season <- cut((summdata$sosdate),
              breaks = as.Date(paste(years,"-01-12",sep="")),
              labels = paste(years[-length(years)],years[-length(years)]+1,sep="/"))

summdata <- summdata %>%
  mutate(date_fake = doytodate(datetodoy(sosdate), 2003)) %>%
  mutate(doy_fake = datetodoy(sosdate)) %>%
  mutate(season = season) %>%
  dplyr::filter(!season %in% c("2002/2003", "2001/2002"))

ggplot(summdata, aes(x = date_fake, y = season, fill = season)) +
  geom_joy(scale = 2.0) +
  scale_x_date(limits = as.Date(c("2002-11-01", "2003-11-01")),
               date_labels = "%b %d") + theme_joy() +
  scale_fill_cyclical(values = c("gray80", "gray50")) +
  ggtitle("Nueva Ecija - Interannual variation of detected EOS")

summdata <- summdata %>%
  group_by(ID_name, date, price_seas) %>%
  summarize(avgsos = mean(date_fake))
