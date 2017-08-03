install.packages("gdalUtils")
require(gdalUtils)
require(tools)

in_mainfolder   <- "/home/lb/Source/git/prasia/data-raw/"
out_mainfolder  <- "/home/lb/Source/git/prasia/data-raw/TIFFs"

in_datfiles <- list.files(in_folder, recursive = T, full.names = TRUE, pattern = glob2rx("Phenorice_out*.dat"))

test = TRUE

for (file in seq_along(in_datfiles)) {
  year_folder <- basename(dirname(in_datfiles[file]))
  tile_folder <- basename(dirname(dirname(dirname(in_datfiles[file]))))
  out_folder  <- file.path(out_mainfolder, tile_folder, year_folder)
  dir.create(out_folder, recursive = T, showWarnings = FALSE)
  out_file    <- file.path(out_folder, paste0(basename(file_path_sans_ext(in_datfiles[file])), ".tif"))
  if (test == TRUE) {
    message("Old file: ", in_datfiles[file], " \nNew_file: ", out_file)
    message("---------------------------------------------------------")
  } else {
    message("Compressing file: ", in_datfiles[file], " \nTo: ", out_file)
    gdal_translate(in_datfiles[file], out_file, co = ("COMPRESS=DEFLATE"))
    message("---------------------------------------------------------")
  }
}




