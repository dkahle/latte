#' Iterated Kronecker product
#'
#' Compute the Kronecker product of several matrices.
#'
#' If kronecker is  the function that computes A x B, kprod computes A x B x C and so on; it's a wrapper of Reduce and kronecker.
#'
#' @param ... a listing of matrices
#' @return ... a matrix that is the kronecker product of those matrices (from left to right)
#' @export kprod
#' @examples
#'
#' kprod(diag(2), t(ones(2)))
#' kprod(t(ones(2)), diag(2))
#'
#'
#' kprod(diag(2), t(ones(2)), t(ones(2)))
#' kprod(t(ones(2)), diag(2), t(ones(2)))
#' kprod(t(ones(2)), t(ones(2)), diag(2))
#'
#'
#' rbind(
#'   kprod(diag(2), t(ones(2))),
#'   kprod(t(ones(2)), diag(2))
#' ) # cf. aoki, hara, and takemura p.13
#'
#'
kprod <- function(...) Reduce(kronecker, list(...))
