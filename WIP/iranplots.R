library(sprawl)
library(ggspatial)
library(ggplot2)
in_folder  <- "/home/lb/nr_working/shared/PhenoRice/Processing/Iran/Outputs/AOI_3"
out_folder <- "/home/lb/nr_working/shared/PhenoRice/Processing/Iran/Outputs/AOI_3/tiffs"
start_year <- 2010
end_year   <- 2016
reshape_outs(in_folder, out_folder, start_year, end_year)

sowing <- read_rast(list.files(file.path(out_folder, "param_series/"),
                               full.names = T)[7])
NAvalue(sowing) <- -32768
sowing_yearly <- list()
for (yy in seq_along(start_year:end_year)) {
  print(yy)
  sowing_yearly[[yy]] <- sum(sowing[[4*(yy - 1) + 1:4]])
}

sow_s3     <- sowing[[3 + 4*(0:6)]]
band_names <- paste0("Year ",start_year:end_year)
NAvalue(sow_s3) <- -32768
leg_breaks <- c(100,115,130, 145, 160, 175, 190)
leg_labels <- doytodate(leg_breaks, 2010)
leg_labels <- format(leg_labels, "%d/%m")
plot_rast_gg(sow_s3, na.color = "transparent",
             band_names = band_names,
             palette_type = "diverging",
             leg_labels = leg_labels,
             leg_breaks = leg_breaks,
             zlims = c(100, 190),
             outliers_style = "to_minmax",
             zoomin = 0 ,
             basemap = "osmgrayscale",
             na.value = -32768,
             scalebar_txt_dist = 0.05,
             maxpixels = 500000000)

sow_s4     <- sowing[[4 + 4*(0:6)]]
band_names <- paste0("Year ",start_year:end_year)
NAvalue(sow_s4) <- -32768
leg_breaks <- c(100,115,130, 145, 160, 175, 190) + 100
leg_labels <- doytodate(leg_breaks, 2010)
leg_labels <- format(leg_labels, "%d/%m")
plot_rast_gg(sow_s4_cut, na.color = "transparent",
             band_names = band_names,
             palette_type = "diverging",
             palette_name = "RdYlBu",
             # leg_labels = leg_labels,
             # leg_breaks = leg_breaks,
             zlims = c(220, 240),
             # zlims_type = "percs",
             outliers_style = "to_minmax",
             zoomin = 0 ,
             # basemap = "osmgrayscale",
             na.value = -32768,
             scalebar_txt_dist = 0.05,
             maxpixels = 50000000000000)
