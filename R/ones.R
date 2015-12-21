#' Ones Vector
#'
#' Make a column vector of ones.
#'
#' @param n how many ones
#' @return a column vector of ones as an integer matrix
#' @export ones
#' @examples
#'
#' ones(5)
#' str(ones(5))
#'
#'
#'
#'
ones <- function(n) matrix(rep(1L, n))
