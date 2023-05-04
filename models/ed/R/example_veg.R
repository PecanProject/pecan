#' Example css, pss, and site objects
#'
#' @export
example_css <- tibble::tribble(
  ~time, ~patch, ~cohort, ~dbh, ~hite, ~pft, ~n, ~bdead, ~balive, ~lai,
  2008,   1,      1,      12.50, 0,     9,   0.001, 0,    0,      0
)

#' @rdname example_css
#' @export
example_pss <- tibble::tribble(
  ~site, ~time, ~patch, ~trk, ~age, ~area, ~water, ~fsc, ~stsc, ~stsl, ~ssc, ~psc, ~msn, ~fsn,
  1,      2008, 1,      1,    70,   1,      0,      1,    5,    5,      0.01, 0,    1,    1
)

#' @rdname example_css
#' @export
example_site <- tibble::tribble(
  ~sitenum, ~area, ~TCI, ~elev, ~slope, ~aspect, ~soil,
  1,        1,    -7,    100,   0,      0,        3
)
attr(example_site, "nsite") <- 1
attr(example_site, "file_format") <- 1
