#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param in_folder PARAM_DESCRIPTION
#' @param out_folder PARAM_DESCRIPTION
#' @param start_year PARAM_DESCRIPTION
#' @param end_year PARAM_DESCRIPTION
#' @param overwrite PARAM_DESCRIPTION, Default: FALSE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname pr_extract_idl_outs
#' @export
#' @author Lorenzo Busetto, phD (2017) <lbusett@gmail.com>
#' @importFrom raster NAvalue writeRaster
#' @importFrom sprawl read_rast get_rastinfo
#'
pr_extract_idl_outs <- function(in_folder, out_folder, start_year, end_year,
                             overwrite = FALSE) {
  years      <- paste0(seq(start_year,end_year,1))
  in_folders <- file.path(in_folder, years)
  yy = 1
  for (yy in 1:length(years)) {

    print(years[yy])
    in_file <- list.files(in_folders[[yy]], pattern = 'Phenorice_out?',
                          full.names = TRUE)[1]
    inrast  <- sprawl::read_rast(in_file)
    raster::NAvalue(inrast) <- -999
    info    <- sprawl::get_rastinfo(inrast, verbose = FALSE)
    N_seas  <- (info$nbands - 1) / 7
    bandnames <- c("nseas",
                   paste("sos", 1:N_seas, sep = "_"),
                   paste("pos", 1:N_seas, sep = "_"),
                   paste("eos", 1:N_seas, sep = "_"),
                   paste("cumevi", 1:N_seas, sep = "_"),
                   paste("cumevi_vgt", 1:N_seas, sep = "_"),
                   paste("veglgt", 1:N_seas, sep = "_"),
                   paste("totlgt", 1:N_seas, sep = "_"))
    names(inrast) <- bandnames
    for (band in seq_along(bandnames)) {

      in_band      <- inrast[[band]]
      out_filename <- file.path(out_folder,
                                paste0(bandnames[band], "_",years[yy],".tif"))
      make_folder(out_filename, type = "filename")
      if (!file.exists(out_filename) | overwrite) {
        message("Writing ", out_filename)
        raster::writeRaster(in_band, filename = out_filename,
                            options = c("COMPRESS=LZW"),
                            overwrite = overwrite)
      } else {
        message(basename(out_filename) , " alredy exists. Skipping!")
      }
    }
  }
}
