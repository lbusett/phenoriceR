#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param in_txt PARAM_DESCRIPTION
#' @param in_riceatlas_shp PARAM_DESCRIPTION
#' @param ISO PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname read_riceatlas
#' @export
#' @author Lorenzo Busetto, phD (2017) <lbusett@gmail.com>
#' @importFrom dplyr filter left_join
#' @importFrom sf st_sf
#' @importFrom sprawl read_vect
read_riceatlas <- function(in_txt,
                           in_riceatlas_shp,
                           ISO){
  require(sf)
  in_data <- read.csv2(in_txt)
  names(in_data)[1] = "OBJECTID"
  in_shape <- sprawl::read_vect(in_riceatlas_shp) %>%
    dplyr::filter(ISO == !!ISO)
  in_shape_geom <- in_shape[1]
  in_shape_long <- in_shape_geom %>%
    dplyr::left_join(in_data) %>%
    sf::st_sf(sf_column_name = "geometry")
  in_shape_long <- subset(in_shape_long, ISO == ISO)
  return(in_shape_long)
}
