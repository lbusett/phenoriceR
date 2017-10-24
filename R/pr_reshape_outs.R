#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param in_folder PARAM_DESCRIPTION
#' @param out_folder PARAM_DESCRIPTION
#' @param start_year PARAM_DESCRIPTION
#' @param end_year PARAM_DESCRIPTION
#' @param overwrite PARAM_DESCRIPTION, Default: F
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname pr_reshape_outs
#' @export
#' @author Lorenzo Busetto, phD (2017) <lbusett@gmail.com>

pr_reshape_outs <- function(in_folder,
                         out_folder,
                         start_year,
                         end_year,
                         crop_extent = NULL,
                         overwrite = F) {
  pr_extract_idl_outs(in_folder,
                   out_folder = file.path(out_folder,"single_files"),
                   start_year,
                   end_year,
                   overwrite = F)
  patterns <- c("sos", "eos", "pos",
                "cumevi", "cumevi_vgt",
                "veglgt", "totlgt",
                "nseas")
  pr_create_ordered_tiffs(file.path(out_folder,"single_files"),
                       patterns,
                       file.path(out_folder,"param_series"))
  pr_decirc_phenorice(file.path(out_folder,"param_series"),
                   file.path(out_folder,"param_series/decirc"),
                   start_year,
                   end_year)
}
