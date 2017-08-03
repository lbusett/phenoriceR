#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param mosaics_folder PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname rename_year_mosaics
#' @export
#' @author Lorenzo Busetto, phD (2017) <lbusett@gmail.com>

rename_year_mosaics <- function(mosaics_folder, out_folder) {

  dir.create(out_folder, recursive = TRUE, showWarnings = FALSE)
  #   ____________________________________________________________________________
  #   change band names                         ####

  patterns    <- c("n*", "s*", "e*", "p*", "v*", "l*", "c*")
  newpatterns <- c("nseas", "sos_", "eos_", "pos_", "veglgt_", "totlgt_", "cumevi_")

  for (pat in seq_along(patterns)) {
    old_names <- list.files(mosaics_folder, pattern = glob2rx(patterns[pat]), full.names = F)
    new_names <- gsub(strsplit(patterns[pat],"*")[[1]][1], newpatterns[pat], old_names)
    file.rename(file.path(mosaics_folder, old_names), file.path(out_folder, new_names))
  }
}
#
#  mosaics_folder <- "/home/lb/my_data/prasia/mosaics/by_year/2017-07-22/"
#  out_folder <- "/home/lb/my_data/prasia/mosaics/by_year"
#  rename_year_mosaics(mosaics_folder, out_folder)
