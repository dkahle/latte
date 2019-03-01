#' Vector to array conversion
#'
#' Convert a vector into an array given a set of dimensions; it therefore simply
#' wraps [aperm()] and [array()].
#'
#' This function converts an array (or a multi-way contingency table) into a
#' vector, using a consistent ordering of the cells. The ordering of the cells
#' is lexicographical and cannot be specified by the user.
#'
#' @param vec A vector
#' @param dim The desired array dimensions, oftentimes a vector of the number of
#'   levels of each variable in order
#' @return An array
#' @export
#' @seealso [tab2vec()], [aperm()], [array()]
#' @examples
#'
#' data(Titanic)
#' Titanic
#' tab2vec(Titanic)
#' vec2tab(tab2vec(Titanic), dim(Titanic))
#' vec2tab(tab2vec(Titanic), dim(Titanic)) == Titanic
#' all(vec2tab(tab2vec(Titanic), dim(Titanic)) == Titanic)
#'
#' 
vec2tab <- function(vec, dim){
  aperm(
    array(vec, rev(dim)),
    length(dim):1
  )
}
