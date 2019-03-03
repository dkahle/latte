#' Ones
#'
#' Make an array of ones
#'
#' @param ... A sequence of dimensions separated by commas
#' @return An integer array of ones
#' @export
#' @examples
#'
#' ones(5)
#' ones(5, 1)
#' ones(1, 5)
#' ones(2, 3)
#' ones(2, 3, 2)
#'
#' str(ones(5))
#'
ones <- function(...){
  dims <- as.integer(as.list(match.call(expand.dots = TRUE))[-1])
  a <- rep(1L, prod(dims))
  dim(a) <- dims
  a
}
