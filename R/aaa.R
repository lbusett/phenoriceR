mutate_when <- function(data, ...) {
  dots <- eval(substitute(alist(...)))
  for (i in seq(1, length(dots), by = 2)) {
    condition <- eval(dots[[i]], envir = data)
    mutations <- eval(dots[[i + 1]], envir = data[condition, , drop = FALSE])
    data[condition, names(mutations)] <- mutations
  }
  data
}



intest <- riceatlas_phl %>%
  select(Sub_region, N_seasons, Season, Pheno_stage, Start, Peak, End) %>%
  sf::st_set_geometry(NULL) %>%
  # remove NA rows
  dplyr::filter((Start != 0 & Peak != 0))  %>%
  # Compute start vs end and start vs peak differences
  dplyr::mutate(diff_end_start = (End - Start),
                diff_peak_start = (Peak - Start),)

intest2 <- intest %>%
  # split columns
  tidyr::separate(Pheno_stage, c("stage", "season"), "_st") %>%
  # remove NA rows
  dplyr::filter((Start != 0 & Peak != 0))  %>%
  #  compute end - start doy difference and Peak - start doy difference
  dplyr::mutate(diff_end_start = (End - Start),
                diff_peak_start = (Peak - Start),) %>%
  # add 365 where differences are negative
  lbscripts::mutate_when(diff_end_start < 0 , list(End = End + 365)) %>%
  lbscripts::mutate_when(diff_peak_start < 0 , list(Peak = Peak + 365)) %>%
  # re-create the diffs for test purposes
  dplyr::mutate(diff_end_start = (End - Start),
                diff_peak_start = (Peak - Start))

summary(intest2$diff_end_start)
summary(intest2$diff_peak_start)

ggplot(intest2) + geom_boxplot(aes(x = season, y = diff_end_start))
ggplot(intest2) + geom_boxplot(aes(x = season, y = diff_peak_start))

