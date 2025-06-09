#' Solve an integer progam with LattE
#'
#' [latte_max()] and [latte_min()] use LattE's `latte-maximize` and
#' `latte-minimize` functions to find the maximum or minimum of a linear
#' objective function over the integers points in a polytope (i.e. satisfying
#' linearity constraints). This makes use of the digging algorithm; see the
#' LattE manual at \url{https://www.math.ucdavis.edu/~latte/} for details.
#'
#' @param objective A linear polynomial to pass to [mpoly::mp()], see examples
#' @param constraints A collection of linear polynomial (in)equalities that
#'   define the feasibility region, the integers in the polytope
#' @param method Method `"LP"` or `"cones"`
#' @param dir Directory to place the files in, without an ending `/`
#' @param opts Options; see the LattE manual at
#'   \url{https://www.math.ucdavis.edu/~latte/}
#' @param quiet Show latte output
#' @param shell Messages the shell code used to do the computation
#' @param type `"max"` or `"min"`
#' @return A named list with components `par`, a named-vector of optimizing
#'   arguments, and `value`, the value of the objective function at the optimial
#'   point.
#' @name latte-optim
#' @examples
#'
#'
#' if (has_latte()) {
#'
#' latte_max(
#'   "-2 x + 3 y", 
#'   c("x + y <= 10", "x >= 0", "y >= 0")
#' )
#' 
#' latte_max(
#'   "-2 x + 3 y", 
#'   c("x + y <= 10", "x >= 0", "y >= 0"),
#'   quiet = FALSE
#' )
#'
#'
#' df <- expand.grid("x" = 0:10, "y" = 0:10)
#' df <- subset(df, x + y <= 10L)
#' df$objective <- with(df, -2*x + 3*y)
#' library("ggplot2")
#' ggplot(df, aes(x, y, size = objective)) +
#'   geom_point()
#'
#' latte_min(
#'   "-2 x + 3 y",
#'   c("x + y <= 10", "x >= 0", "y >= 0"),
#'   method = "cones"
#' )
#'
#'
#'
#' latte_min("-2 x - 3 y - 4 z", c(
#'   "3 x + 2 y + z <= 10",
#'   "2 x + 5 y + 3 z <= 15",
#'   "x >= 0", "y >= 0", "z >= 0"
#' ), "cones", quiet = FALSE)
#'
#'
#'
#'
#'
#' }
#' 
latte_optim <- function(
  objective, 
  constraints, 
  type = c("max", "min"),
  method = c("lp","cones"), 
  dir = tempdir(),
  opts = "",
  quiet = TRUE,
  shell = FALSE
){


  if (!has_latte()) missing_latte_stop()

  type   <- match.arg(type)
  method <- match.arg(method)


  ## set executable to use
  exec <- if(type == "max") "latte-maximize" else "latte-minimize"


  ## parse objective
  if(is.character(objective)) objective <- mp(objective)
  stopifnot(is.linear(objective))


  ## parse constraints into the poly <= 0 format
  nConstraints <- length(constraints)

  if(is.character(constraints) && nConstraints > 1){

  	parsedCons <- as.list(rep(NA, nConstraints))

  	geqNdcs <- which(str_detect(constraints, " >= "))
  	leqNdcs <- which(str_detect(constraints, " <= "))
  	eeqNdcs <- which(str_detect(constraints, " == "))
  	eqNdcs  <- which(str_detect(constraints, " = "))

    if(length(geqNdcs) > 0){
      tmp <- strsplit(constraints[geqNdcs], " >= ")
      parsedCons[geqNdcs] <- lapply(tmp, function(v) mp(v[2]) - mp(v[1]))
    }

    if(length(leqNdcs) > 0){
      tmp <- strsplit(constraints[leqNdcs], " <= ")
      parsedCons[leqNdcs] <- lapply(tmp, function(v) mp(v[1]) - mp(v[2]))
    }

    if(length(eeqNdcs) > 0){
      tmp <- strsplit(constraints[eeqNdcs], " == ")
      parsedCons[eeqNdcs] <- lapply(tmp, function(v) mp(v[1]) - mp(v[2]))
    }

    if(length(eqNdcs) > 0){
      tmp <- strsplit(constraints[eqNdcs], " = ")
      parsedCons[eqNdcs] <- lapply(tmp, function(v) mp(v[1]) - mp(v[2]))
    }

    linearityNdcs <- sort(c(eeqNdcs, eqNdcs))

    constraints <- parsedCons
    class(constraints) <- "mpolyList"

    if (!all(is.linear(constraints))) stop("all polynomials must be linear.", call. = FALSE)
    

  }


  ## mpoly_list_to_mat is in file count.r
  matFull <- mpoly_list_to_mat(
  	structure(
  		c(list(objective), constraints),
  		class = "mpolyList"
  	)
  )


  ## make dir to put latte files in (within the tempdir) timestamped
  dir.create(scratch_dir <- file.path(dir, time_stamp()))


  ## switch to temporary directory
  user_working_directory <- getwd()
  setwd(scratch_dir); on.exit(setwd(user_working_directory), add = TRUE)


  ## convert constraints to latte hrep code and write file
  mat <- cbind(
    -matFull[-1,"coef",drop=FALSE],
    -matFull[-1,-ncol(matFull)]
  )

  if(length(linearityNdcs) > 0) attr(mat, "linearity")   <- linearityNdcs
  
  
  ## note: the nonnegative stuff is built into this
  write.latte(mat, "optim_code")


  ## convert objective to latte hrep code and write file
  mat <- cbind(
    matFull[1,"coef",drop=FALSE],
    matFull[1,-ncol(matFull),drop=FALSE]
  )[,-1, drop = FALSE]
  write.latte(mat, "optim_code.cost")



  ## run latte function
  if (is_mac() || is_unix()) { 
    
    system2(
      file.path(get_latte_path(), exec),
      paste(opts, file.path(scratch_dir, "optim_code")),
      stdout = glue("optim_out"), 
      stderr = glue("optim_err")
    )
    
    # generate shell code
    shell_code <- glue(
      "{file.path(get_latte_path(), exec)} {paste(opts, file.path(scratch_dir, 'optim_code'))} > {exec}_out 2> {exec}_err"
    )
    if(shell) message(shell_code)
    
  } else if(is_win()){ # windows
    
    matFile <- file.path(scratch_dir, "optim_code 2> out.txt")
    matFile <- chartr("\\", "/", matFile)
    matFile <- str_c("/cygdrive/c", str_sub(matFile, 3))
    system(
      paste(
        paste0("cmd.exe /c env.exe"),
        file.path(get_latte_path(), exec),
        opts,
        matFile
      ),
      intern = FALSE, ignore.stderr = FALSE
    )
     
  }
  

  
  ## print count output when quiet = FALSE
  std_out <- readLines("optim_out")
  if(!quiet) cat(std_out, sep = "\n")
  # note: strangely, latte posts non-error info to stderr
  std_err <- readLines("optim_err")
  if(!quiet && any(std_err != "")) message(str_c(std_err, collapse = "\n"))
  
  ## parse output
  if (method == "cones") {
    optimal_solution_string <- std_err[str_which(std_err, "An optimal solution")]
  } else {
    optimal_solution_string <- std_err[str_which(std_err, "A vertex which we found")]
  }

  optimal_solution_string <- str_extract(optimal_solution_string, "\\[-?\\d+( -?\\d+)+\\].?$")
  par <- as.integer(str_extract_all(optimal_solution_string, "-?\\d+")[[1]])
  names(par) <- colnames(matFull)[1:(ncol(matFull)-1)]
  
  val <- str_extract(std_err, "(?<=The optimal value is: )-?\\d+")
  val <- as.integer(val[!is.na(val)])

  ## out
  list(par = par, value = val)
}












#' @rdname latte-optim
#' @export
latte_max <- function(objective, constraints, method = c("lp","cones"),
  dir = tempdir(), opts = "", quiet = TRUE
){

  latte_optim(objective, constraints, "max", method, dir, opts, quiet)

}




#' @rdname latte-optim
#' @export
latte_min <- function(objective, constraints, method = c("lp","cones"),
  dir = tempdir(), opts = "", quiet = TRUE
){

  latte_optim(objective, constraints, "min", method, dir, opts, quiet)

}




