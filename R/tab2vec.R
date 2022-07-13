#' Array to vector conversion
#'
#' Convert an array into a vector.
#'
#' This function converts an array (or a multi-way contingency table) into a
#' vector, using a consistent ordering of the cells. The ordering of the cells
#' is lexicographical and cannot be specified by the user.
#'
#' @param tab An array of counts
#' @return a Named integer vector.  The names correspond to the cell indices in
#'   the table.
#' @export
#' @seealso [vec2tab()]
#' @examples
#'
#' a <- array(1:6, c(1,2,3))
#' tab2vec(a)
#'
#' data(Titanic)
#' tab2vec(Titanic)
#' Titanic[1,1,1,1]
#' Titanic[1,1,1,2]
#'
#' 
tab2vec <- function(tab){

  # if is a vector, return
  if(is.null(dim(tab))){
    tab <- as.vector(tab)
    names(tab) <- 1:length(tab)
    return(tab)
  }

  # if it's a vector already, short-circuit
  if(length(dim(tab)) == 1){
    tab <- as.vector(tab)
    names(tab) <- 1:length(tab)
    return(tab)
  }

  # otherwise, rotate and class
  u <- aperm(tab, length(dim(tab)):1)
  if(inherits(tab[1], "numeric")) u <- as.vector(u)
  if(inherits(tab[1], "integer")) u <- as.integer(u)

  # create cell indices
  tmpdf <- expand.grid(
    rev.default(lapply(dim(tab), function(x) 1:x))
  )[,length(dim(tab)):1]

  # assign them as names to u
  names(u) <- apply(tmpdf, 1, paste, collapse = ',')

  # return
  u
}
