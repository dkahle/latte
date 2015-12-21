#' Make a vector of ones
#'
#' \code{ones} makes a column vector of ones, \code{ones_r} makes a
#' row vector of one.
#'
#' @param n How many ones
#' @return A Column/row vector of ones as an integer matrix
#' @export ones
#' @name ones
#' @examples
#'
#' ones(5)
#' ones_r(5)
#'
#' str(ones(5))
#'

#' @rdname ones
#' @export
ones <- function(n) matrix(rep(1L, n))


#' @rdname ones
#' @export
ones_r <- function(n) matrix(rep(1L, n), nrow = 1)
