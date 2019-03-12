#' Plot a matrix
#'
#' plot_matrix is a R variant of Matlab's \code{spy} function.
#'
#' @param A A matrix
#' @return a ggplot object
#' @author David Kahle \email{david@@kahle.io}
#' @name plot-matrix
#' @examples
#'
#' # the no-three-way interaction configuration
#' (A <- kprod(ones(1,3), diag(3), ones(3)))
#' plot_matrix(A)
#' 
#' 
#' if (has_4ti2()) {
#' 
#' plot_matrix(markov(A))
#' 
#' (A <- genmodel(c(2L, 2L), list(1L, 2L)))
#' plot_matrix(A)
#' plot_matrix(markov(A))
#' 
#' (A <- genmodel(c(5L, 5L), list(1L, 2L)))
#' plot_matrix(A)
#' plot_matrix(markov(A))
#' 
#' }
#'






#' @rdname plot-matrix
#' @export
plot_matrix <- function(A){
  x <- NULL; rm(x)
  y <- NULL; rm(y)

  low <- if(any(A < 0)){
    low <- "blue"; high <- "red"
    fillScale <- scale_fill_gradient2(
      low = "blue", mid = "grey80",
      high = "red", midpoint = 0, guide = FALSE, space = "Lab"
    )
  } else {
    fillScale <- scale_fill_gradient(
      low = "white", high = "black", guide = FALSE
    )
  }
  df <- expand.grid(x = 1:ncol(A), y = 1:nrow(A))
  B <- t(A)
  df$A <- as.integer(B[,ncol(B):1])
  ggplot(df, aes(x, y, fill = A)) +
    geom_tile() +
    fillScale +
    theme_bw() + coord_equal() +
    scale_x_continuous(expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0)) +
    theme(
      line = element_blank(), text = element_blank(),
      plot.margin = grid::unit(c(0, 0, 0, 0), "lines")
    )
}
