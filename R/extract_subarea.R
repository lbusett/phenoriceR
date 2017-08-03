#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param in_mosaics_folder PARAM_DESCRIPTION
#' @param in_mask PARAM_DESCRIPTION
#' @param subset_name PARAM_DESCRIPTION
#' @param out_folder PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#'  mosaic_folder <- "/home/lb/my_data/prasia/mosaics/ordered"
#'  out_folder  <- "/home/lb/my_data/prasia/mosaics/ordered/subsets/"
#'  subset_name <- "Nueva_Ecija"
#'  in_country  <- "PHL"
#'  boundmask   <- sprawl::get_boundaries(in_country, level = 1) %>%
#'  sf::st_as_sf() %>%
#'  dplyr::filter(NAME_1 == "Nueva Ecija") %>%
#'  sf::st_combine()
#'  extract_subarea(mosaic_folder,
#'                 boundmask,
#'                 subset_name,
#'                 out_folder)
#'  }
#' @rdname extract_subarea
#' @export
#' @author Lorenzo Busetto, phD (2017) <lbusett@gmail.com>
#' @importFrom raster stack setZ
#' @importFrom sprawl mask_rast
#' @importFrom stringr str_split_fixed
#'
extract_subarea <- function(in_mosaics_folder,
                            in_mask,
                            subset_name,
                            out_folder) {
  #   ____________________________________________________________________________
  #   Mask and save (this will become a function in the future)            ####

  in_tiffs <- list.files(mosaic_folder, pattern = ".tif", full.names = TRUE)
  in_vars <- stringr::str_split_fixed(basename(in_tiffs), "_",2)[,1]

  for (file in seq_along(in_tiffs)) {

    message("extract_subarea --> Extracting: `", in_vars[file], "` data on: `", subset_name,
            "` Please Wait !")
    in_rast        <- raster::stack(in_tiffs[file])
    if (in_vars[file] == "nseas") {
      names(in_rast) <- paste(in_vars[file], seq(2003, 2016, 1), sep = "_")
      in_rast <- raster::setZ(in_rast,
                              as.Date(paste(seq(2003, 2016, 1),"-01-01", sep = "")))
    } else {
      names(in_rast) <- paste(
        in_vars[file],
        paste(sort(rep(seq(2003, 2016, 1), 4)), c("s1","s2","s3","s4"), sep = "_"),
        sep = "_")
      in_rast <- raster::setZ(in_rast,
                              as.Date(paste(sort(rep(seq(2003, 2016, 1), 4)),"-01-01",
                                            sep = "")))
    }

    out_tiff  <- file.path(out_folder, subset_name, paste0(in_vars[file], ".tif"))
    out_RData <- file.path(out_folder, subset_name, paste0(in_vars[file], ".RData"))
    dir.create(dirname(out_tiff), recursive = TRUE, showWarnings = FALSE)
    out_rast  <- sprawl::mask_rast(in_rast,
                                   in_mask,
                                   to_file   = FALSE,
                                   out_rast  = out_tiff,
                                   crop      = TRUE,
                                   overwrite = TRUE)
    out_rast_full <- raster::stack(out_tiff)
    names(out_rast_full) <- names(out_rast)
    out_rast_full <- raster::setZ(out_rast_full, raster::getZ(out_rast))
    save(out_rast, file = out_RData)
  }
}

# #   ____________________________________________________________________________
# #   set input and output folders                                            ####
#
# mosaic_folder <- "/home/lb/my_data/prasia/mosaics/ordered"
# out_folder    <- "/home/lb/my_data/prasia/mosaics/ordered/subsets/"
#
# #   ____________________________________________________________________________
# #   define subsetting area: choose a country                  ####
#
# subset_name <- "PHL"
# in_country  <- "PHL"
# bound <- sprawl::get_boundaries(in_country, level = 1) %>%
#   sf::st_as_sf()
#
# # It's also possible to mask directly on a "province":
#
# subset_name <- "Nueva_Ecija"
# in_country  <- "PHL"
# boundmask   <- sprawl::get_boundaries(in_country, level = 1) %>%
#   sf::st_as_sf() %>%
#   dplyr::filter(NAME_1 == "Nueva Ecija") %>%
#   sf::st_combine()
#
# t1 <- Sys.time()
# extract_subarea(mosaic_folder,
#                 boundmask,
#                 subset_name,
#                 out_folder)
# Sys.time() - t1
