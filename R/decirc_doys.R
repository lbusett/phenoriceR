#' @title convert PhenoRice "dates" to julian date (diff to 2000-01-01)
#' @description convert sos, pos and eos to a numeric julian date expressing number of
#'   days elapsed since 2000-01-01
#' @param in_folder input folder containing the "ordered" multiband tiffs resulting from
#'   `create_ordered_tiffs``
#' @param out_folder where to store the results
#' @return NULL - files "sos_decirc.RData, sos_decirc.tif,
#'   eos_decirc.RData, eos_decirc.tif
#'   pos_decirc.RData, pos_decirc.tif
#'   are created in output folder
#' @details NOTE: Very slow on the full mosaics, but works in the same way on subsets and
#'   is (reasonably) fast.
#' @examples
#' \dontrun{
#'   in_folder  <- "/home/lb/my_data/prasia/mosaics/ordered/subsets/Nueva_Ecijia/"
#'   out_folder <- file.path(in_folder, "decirc")
#'   decirc_phenorice(in_folder, out_folder)
#' }
#' @rdname decirc_phenorice
#' @export
#' @author Lorenzo Busetto, phD (2017) <lbusett@gmail.com>
#' @importFrom gdalUtils gdalbuildvrt gdal_translate
#' @importFrom raster stack setZ calc

decirc_phenorice <- function(in_folder, out_folder){

  dir.create(out_folder, recursive = TRUE, showWarnings = FALSE)
  tmp_folder <- file.path(out_folder, "tmp")
  dir.create(tmp_folder, recursive = TRUE, showWarnings = FALSE)
  #   ____________________________________________________________________________
  #   compute decircularized single bands (using the full files is very slow  ####
  #   on the full mosaics, so create single bands and then aggregate later
  #   in a multiband

  for (in_var in c("sos", "eos", "pos")) {
    in_file <- list.files(in_folder, pattern = paste0(in_var, "_*.tif$"),
                          full.names = TRUE)

    # out_RData <- file.path(out_folder, paste0(in_var, "_decirc.RData"))
    in_rast        <- raster::stack(in_file)
    names(in_rast) <- paste(in_var,
                            paste(sort(rep(seq(2003, 2016, 1), 4)),
                                  c("s1","s2","s3","s4"), sep = "_"),
                            sep = "_")
    years   <- as.Date(paste(sort(rep(seq(2003, 2016, 1), 4)),"-01-01", sep = ""))
    in_rast <- raster::setZ(in_rast, years)
    for (yy in seq_along(years)) {
      message(in_var, "band - ", yy)
      out_file  <- file.path(tmp_folder,
                             paste(in_var, sprintf("%03i", yy), "decirc.tif", sep = "_"))
      diffdays  <- as.numeric(years[yy] - as.Date("2000-01-01"))
      raster::calc(in_rast[[yy]],
                   fun = function(x) {x + diffdays},
                   filename = out_file,
                   ot = "UInt16",
                   options = "COMPRESS=DEFLATE",
                   overwrite = TRUE,
                   NAflag = 32767)
    }


    #   ____________________________________________________________________________
    #   join the bands in multiband and remove single bands                     ####
    in_files <- list.files(tmp_folder, pattern = in_var, full.names = TRUE)
    out_file <- file.path(out_folder, paste0(in_var, "_decirc.tif"))
    temp_vrt <- tempfile(fileext = ".vrt")

    gdalUtils::gdalbuildvrt(in_files,
                            temp_vrt,
                            separate = T, vrtnodata = 32767)

    gdalUtils::gdal_translate(temp_vrt,
                              out_file,
                              ot = "UInt16",
                              options = "COMPRESS=DEFLATE",
                              separate = T,
                              overwrit = TRUE,
                              a_nodata = 32767)

    unlink(in_files)

    #   ____________________________________________________________________________
    #   create also an RData with associated band names and "times"             ####

    out_rast        <- raster::stack(out_file)
    names(out_rast) <- paste(in_var,
                             paste(sort(rep(seq(2003, 2016, 1), 4)),
                                   c("s1_d","s2_d","s3_d","s4_d"), sep = "_"),
                             sep = "_")
    out_rast        <- raster::setZ(out_rast, years)
    save(out_rast, file = paste0(tools::file_path_sans_ext(out_file), ".RData"))
  }
}
