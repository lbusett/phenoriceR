#' @title create_ordered_tiffs
#' @description FUNCTION_DESCRIPTION
#' @param mosaics_folder PARAM_DESCRIPTION
#' @param patterns PARAM_DESCRIPTION
#' @param out_folder PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#'  # # Folder containing the mosaics by year and parameter (e.g., sos_1_2016, sos_2_2016, etc)
#   # Note we are using the "renamed" ones obtained after applying "rename_year_mosaics"
#   mosaics_folder <- "/home/lb/nr_working/shared/PhenoRice/Asia/Data/mosaics/by_year/"
#   outfold <- "home/lb/nr_working/shared/PhenoRice/Asia/Data/mosaics/by_year/ordered"
#   patterns <- c("sos", "eos", "pos", "cumevi", "veglgt", "totlgt", "nseas")
#   create_ordered_tiffs(mosaics_folder,patterns,out_fold)
#' }
#' @rdname create_ordered_tiffs
#' @export
#' @author Lorenzo Busetto, phD (2017) <lbusett@gmail.com>
#' @importFrom doSNOW registerDoSNOW
#' @importFrom gdalUtils gdal_translate
#' @importFrom parallel makeCluster stopCluster
#' @importFrom tools file_path_sans_ext
#' @import foreach
#'
create_ordered_tiffs <- function(mosaics_folder,
                                 patterns,
                                 out_folder) {

  dir.create(out_folder, recursive = TRUE, showWarnings = FALSE)
  for (pattern in patterns) {
    print(pattern)
    create_vrt_mosaics(mosaics_folder, pattern)
  }
  vrts    <- list.files(mosaics_folder, pattern = ".vrt", full.names = T)
  ncores  <- 4
  clust   <- parallel::makeCluster(ncores, outfile = " ")
  doSNOW::registerDoSNOW(clust)

  out <- foreach(
    vrt_n = seq_along(vrts),
    .combine  = "c",
    .packages = c("gdalUtils", "tools"),
    .verbose  = TRUE
  ) %dopar% {

    message("create_ordered_tiffs --> processing: ", basename(vrts[vrt_n]))
    parameter <- basename(tools::file_path_sans_ext(vrts[vrt_n]))
    outfile <- file.path(outfold,
                         paste0(basename(tools::file_path_sans_ext(vrts[vrt_n])), ".tif"))
    if (parameter %in% c("sos_ordered", "eos_ordered", "pos_ordered")) {
      ot <- "Int16"
    }
    if (parameter %in% c("cumevi_ordered")) {
      ot <- "Int32"
    }
    if (parameter %in% c("totlgt_ordered", "veglgt_ordered", "nseas_ordered")) {
      ot <- "Byte"
    }

    gdalUtils::gdal_translate(vrts[vrt_n],
                              outfile,
                              ot        = ot,
                              of        = "GTiff",
                              co        = "COMPRESS=DEFLATE",
                              separate  = TRUE,
                              verbose   = TRUE,
                              overwrite = TRUE)

    return(paste0(outfile, " completed ! "))
  }
  parallel::stopCluster(clust)
}

# require(tidyverse)
# require(gdalUtils)
# require(foreach)
# require(parallel)
# require(doSNOW)
# # Folder containing the mosaics by year and parameter (e.g., sos_1_2016, sos_2_2016, etc)
# # Note we are using the "renamed" ones obtained after applying "rename_year_mosaics"
# mosaics_folder <- "/home/lb/nr_working/shared/PhenoRice/Asia/Data/mosaics/by_year/"
# outfold <- "home/lb/nr_working/shared/PhenoRice/Asia/Data/mosaics/by_year/ordered"
# dir.create(outfold)
# patterns <- c("sos", "eos", "pos", "cumevi", "veglgt", "totlgt", "nseas")
