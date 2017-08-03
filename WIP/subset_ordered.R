library(sprawl)
library(sf)
library(raster)

#   ____________________________________________________________________________
#   set input and output folders                                            ####

mosaic_folder <- "/home/lb/my_data/prasia/mosaics/ordered"
out_folder <- "/home/lb/my_data/prasia/mosaics/ordered/subsets/"


#   ____________________________________________________________________________
#   define subsetting area: choose a country and province                   ####

subset_name <- "Nueva_Ecijia"
in_country = "PHL"
bound <- get_boundaries(in_country, level = 1) %>%
  sf::st_as_sf() %>%
  dplyr::filter(NAME_1 == "Nueva Ecija")




#   ____________________________________________________________________________
#   de-circularize dates: convert to dasys elapsed since 01/01/2000         ####

for (in_var in c("sos", "eos", "pos")) {


  in_file <- list.files(file.path(out_folder, subset_name),
                        pattern = paste0(in_var, "*.RData"), full.names = TRUE)
  out_file  <- file.path(out_folder, subset_name, paste0(in_var, "_decirc.tif"))
  out_RData <- file.path(out_folder, subset_name, paste0(in_var, "_decirc.RData"))
  in_rast <- get(load(in_file))
  years <- as.Date(paste(sort(rep(seq(2003, 2016, 1), 4)),"-01-01", sep = ""))
  out_rast <- stack(in_rast)
  for (yy in seq_along(years)) {
    message(in_var, "band - ", yy)
    diffdays       <- as.numeric(years[yy] - as.Date("2000-01-01"))
    out_rast[[yy]] <- getValues(in_rast[[yy]]) + diffdays
  }
  names(out_rast) <- paste0(names(in_rast), "_d")
  writeRaster(out_rast,
              filename = out_file,
              options = "COMPRESS=DEFLATE",
              overwrite = TRUE)
  save(out_rast, file = out_RData)
}


soscirc = "/home/lb/my_data/prasia/mosaics/ordered/subsets/Nueva_Ecijia/sos_decirc.tif"
sos = get(load("/home/lb/my_data/prasia/mosaics/ordered/subsets//Nueva_Ecijia/sos.RData"))

plot_rast(sos[[c(2,6,10,14,18,22)]], in_poly = boundaries)
a + par.settings = list(axis.line = list(col = "transparent"))


soscirc_ext <- extract_rast(stack(soscirc), bound)

test <- test %>%
  mutate(year = years) %>%
  mutate(date = (as.Date("2000-01-01")+avg)) %>%
  mutate(seas = rep(1:4, 14)) %>%
  mutate(doy = datetodoy(date))

ggplot(test, aes(x = year, y = doytodate(doy, 2000))) + geom_point() + facet_wrap(~seas) +
  scale_y_date(date_labels = "%b %d")
