#' R Interface to LattE and 4ti2
#'
#' Back-end connections to LattE (\url{https://www.math.ucdavis.edu/~latte/})
#' and 4ti2 (\url{http://www.4ti2.de/}) executables and front-end tools
#' facilitating their use in the R ecosystem.
#'
#' @docType package
#' @import mpoly
#' @importFrom ggplot2 ggplot scale_x_continuous scale_y_continuous theme
#'   element_blank theme_bw coord_equal scale_fill_gradient scale_fill_gradient2
#'   aes geom_tile
#' @importFrom magrittr %>%
#' @importFrom stats runif
#' @importFrom utils download.file
#' @importFrom stringr str_detect str_sub str_c str_replace str_replace_all
#'   str_split str_trim str_extract str_extract_all str_which
#' @importFrom dplyr filter mutate arrange
#' @importFrom memoise memoise
#' @importFrom usethis edit_r_environ
#' @importFrom glue glue
#' @name latte
#' @aliases latte package-latte
NULL
