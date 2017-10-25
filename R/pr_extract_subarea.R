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
#'  pr_extract_subarea(mosaic_folder,
#'                 boundmask,
#'                 subset_name,
#'                 out_folder)
#'  }
#' @rdname pr_extract_subarea
#' @export
#' @author Lorenzo Busetto, phD (2017) <lbusett@gmail.com>
#' @importFrom raster stack setZ
#' @importFrom sprawl mask_rast make_folder
#' @importFrom stringr str_split_fixed
#'
pr_extract_subarea <- function(in_mosaics_folder,
                            in_mask,
                            subset_name,
                            out_folder,
                            what = c("decirc", "orig")) {
  #   ____________________________________________________________________________
  #   Mask and save (this will become a function in the future)            ####

  out_folder <- file.path(out_folder, subset_name)

  for (type in what) {

    if (type == "orig") {
      in_RData     <- list.files(file.path(in_mosaics_folder, "orig"), pattern = ".RData",
                                 full.names = TRUE)
      out_folder_2 <- sprawl::make_folder(file.path(out_folder,
                                                    "param_series/orig"))
    } else {
      in_RData   <- list.files(file.path(in_mosaics_folder, "decirc"),
                               pattern = ".RData", full.names = TRUE)
      out_folder_2 <- sprawl::make_folder(file.path(out_folder,
                                                    "param_series/decirc"))
    }
    in_vars      <- stringr::str_split_fixed(basename(in_RData), "_",2)[,1]

    for (file in seq_along(in_RData)) {

      message("extract_subarea --> Extracting: `", in_vars[file], "` data on: `", subset_name,
              "` Please Wait !")
      in_rast   <- get(load(in_RData[file]))
      if (type == "orig") {
      out_tiff  <- file.path(out_folder_2, paste0(in_vars[file], ".tif"))
      out_RData <- file.path(out_folder_2, paste0(in_vars[file], ".RData"))
      } else{
        out_tiff  <- file.path(out_folder_2, paste0(in_vars[file], "_decirc.tif"))
        out_RData <- file.path(out_folder_2, paste0(in_vars[file], "_decirc.RData"))
      }
      if (!file.exists(out_tiff)) {

        out_rast  <- sprawl::crop_rast(in_rast,
                                       in_mask,
                                       mask      = T,
                                       out_type  = "rastobject",
                                       out_file  = out_tiff,
                                       compress  = "DEFLATE")
        out_rast_full        <- sprawl::read_rast(out_tiff)
        names(out_rast_full) <- names(in_rast)
        out_rast_full        <- raster::setZ(out_rast_full, raster::getZ(out_rast))
        save(out_rast_full, file = out_RData)
      }
    }
  }
}

