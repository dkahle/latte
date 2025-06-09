#' Iterated Kronecker product
#'
#' Compute the Kronecker product of several matrices.
#'
#' If kronecker is  the function that computes A x B, kprod computes A x B x C
#' and so on; it's a wrapper of Reduce and kronecker.
#'
#' @param ... A listing of matrices
#' @param FUN A function to pass to [base::kronecker()]
#' @return A matrix that is the kronecker product of the specified matrices
#'   (from left to right).
#' @export
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
#' # cf. aoki, hara, and takemura p.13
#' rbind(
#'   kprod(diag(2), t(ones(2))),
#'   kprod(t(ones(2)), diag(2))
#' ) 
#'
#' 
kprod <- function(..., FUN = `*`) {
  Reduce(
    function(X, Y) kronecker(X, Y, FUN),
    list(...)
  )
}
