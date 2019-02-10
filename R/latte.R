#' LattE and 4ti2 in R
#'
#' Back-end connections to LattE (\url{https://www.math.ucdavis.edu/~latte/})
#' and 4ti2 (\url{http://www.4ti2.de/}) executables and front-end tools
#' facilitating its use in the R ecosystem.
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
#'   str_split str_trim
#' @importFrom dplyr filter mutate arrange
#' @importFrom memoise memoise
#' @name latte
#' @aliases latte package-latte
NULL
