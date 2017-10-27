#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param in_mosaics_folder folder containing the tiff and RData mosaics.
#'   (e.g., "/home/lb/my_data/prasia/Data/orig_mosaics")
#' @param in_mask polygon on to which data should be extracted
#' @param out_folder folder where results should be stored
#' @return Cropped reasters are saved in the specified output folder.
#' @examples
#' \dontrun{
#'  main_folder <- "/home/lb/my_data/prasia/Data"
#'  mosaic_folder <- "/home/lb/my_data/prasia/Data/orig_mosaics"

#'  in_shp <- read_vect(file.path(main_folder, "vector/Ricetlas/riceatlas_asia_reshuffled.shp"))
#'
#'  # Suppose you want to extract data for Region: "Region_3_-_Central_Luzon" :
#'  # --> extract it from the full shapefile
#'
#'  Region_name <- ""Region_3_-_Central_Luzon""
#'
#'  in_mask <- dplyr::filter(in_shp, Region == Region_name)
#'  in_mask <- unique(in_mask[1:4]) # this is needed to remove duplicate polygons
#'  in_mask
#'  plot_vect(in_mask, fill_var = "ID_name")
#'
#'  # in_mask contains one polygon for each sub_region of "Region_3_-_Central_Luzon"
#'
#'  # Define the output folder
#'  out_folder  <- file.path("/home/lb/my_data/prasia/mosaics/ordered/subsets/",
#'    Region_name)
#'  out_folder
#'
#'  # you have all you need: location of the mosaic rasters, a vector where to
#'  # cut and an output folder
#'
#'  pr_extract_subarea(mosaic_folder,
#'                 in_mask,
#'                 out_folder)
#'
#'  # You will find the cropped rasters in "out_folder"
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
                               out_folder,
                               what = c("decirc", "orig")) {
  #   ____________________________________________________________________________
  #   Mask and save (this will become a function in the future)            ####


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
      in_rast   <- get(load(in_RData[file]))
      if (type == "orig") {
        out_tiff  <- file.path(out_folder_2, paste0(in_vars[file], ".tif"))
        out_RData <- file.path(out_folder_2, paste0(in_vars[file], ".RData"))
        var  <- in_vars[file]
      } else{
        out_tiff  <- file.path(out_folder_2, paste0(in_vars[file], "_decirc.tif"))
        out_RData <- file.path(out_folder_2, paste0(in_vars[file], "_decirc.RData"))
        var  <- paste0(in_vars[file], "_decirc")
      }

      if (!file.exists(out_tiff)) {
        message("extract_subarea --> Extracting: `", var, "` Please Wait !")
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
      } else {
        message("extract_subarea --> ", var, " was already extracted. Skipping!")
      }
    }
  }
}

