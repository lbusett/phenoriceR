#' @title mutate_when
#' @description mutate a column within a dplyr pype only on a subset of rows
#'   specified by a condition
#'   Retrieved from a StackOverflow answer (https://stackoverflow.com/a/34170176/6871135)
#' @param data a `data.frame` (or `tibble` or `data.table`)
#' @param ... expression to be evaluated to filter on rows
#' @return `data frame`
#' @details DETAILS
#' @examples
#' \dontrun{
#'  library(magrittr)
#'  mtcars %>% mutate_when(
#'             mpg > 22,    list(cyl = 100),
#'             disp == 160, list(cyl = 200))
#'
#' }
#' @rdname mutate_when
#' @export
#' @author Kevin Ushey

pr_mutate_when <- function(data, ...) {
  dots <- eval(substitute(alist(...)))
  for (i in seq(1, length(dots), by = 2)) {
    condition <- eval(dots[[i]], envir = data)
    mutations <- eval(dots[[i + 1]], envir = data[condition, , drop = FALSE])
    data[condition, names(mutations)] <- mutations
  }
  data
}
