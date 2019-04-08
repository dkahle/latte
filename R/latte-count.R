#' Count integer points in a polytope
#'
#' \code{latte_count} uses LattE's count function to count the (integer) lattice
#' points in a polytope and compute Ehrhart polynomials.
#'
#' The specification should be one of the following: (1) a character string or
#' strings containing an inequality in the mpoly expression format (see
#' examples), (2) a list of vertices, (3) a list of A and b for the equation Ax
#' <= b (see examples), or (4) raw code for LattE's count program. If a
#' character vector is supplied, (1) and (4) are distinguished by the number of
#' strings.
#'
#' Behind the scenes, count works by writing a latte file and running count on
#' it.  If a specification other than a length one character is given to it
#' (which is considered to be the code), count attempts to convert it into LattE
#' code and then run count on it.
#'
#' @param spec Specification, see details and examples
#' @param dir Directory to place the files in, without an ending /
#' @param quiet Show latte output?
#' @param mpoly When opts = "--ehrhart-polynomial", return the mpoly version of
#'   it
#' @param ... Additional arguments to pass to the function, see count --help at
#'   the command line to see examples.  Note that dashes - should be specified
#'   with underscores _
#' @return The count.  If the count is a number has less than 10 digits, an
#'   integer is returned.  If the number has 10 or more digits, an integer in a
#'   character string is returned. You may want to use the gmp package's as.bigz
#'   to parse it.
#' @name latte-count
#' @examples
#' 
#' if (has_latte()) {
#'
#' spec <- c("x + y <= 10", "x >= 1", "y >= 1")
#' latte_count(spec) # 45
#' latte_count(spec, quiet = FALSE) # 45
#' latte_count(spec, dilation = 10) # 3321
#' latte_count(spec, homog = TRUE) # 45
#'
#' # by default, the output from LattE is in
#' list.files(tempdir())
#' list.files(tempdir(), recursive = TRUE)
#'
#' # ehrhart polynomials
#' latte_count(spec, ehrhart_polynomial = TRUE)
#' latte_count(spec, ehrhart_polynomial = TRUE, mpoly = FALSE)
#'
#' # ehrhart series (raw since mpoly can't handle rational functions)
#' latte_count(spec, ehrhart_series = TRUE)
#'
#' # simplified ehrhart series - not yet implemented
#' #latte_count(spec, simplified_ehrhart_polynomial = TRUE)
#'
#' # first terms of the ehrhart series
#' latte_count(spec, ehrhart_taylor = 1)
#' # latte_count(spec, ehrhart_taylor = 2)
#' # latte_count(spec, ehrhart_taylor = 3)
#' # latte_count(spec, ehrhart_taylor = 4)
#'
#' # multivariate generating function
#' latte_count(spec, multivariate_generating_function = TRUE)
#'
#'
#' # by vertices
#' spec <- list(c(1,1), c(10,1), c(1,10), c(10,10))
#' latte_count(spec)
#' latte_count(spec, vrep = TRUE)
#'
#' code <- "
#' 5 3
#' 1 -1  0
#' 1  0 -1
#' 1 -1 -1
#' 0  1  0
#' 0  0  1
#' "
#' latte_count(code)
#'
#'
#' # for Ax <= b, see this example from the latte manual p.10
#' A <- matrix(c(
#'    1,  0,
#'    0,  1,
#'    1,  1,
#'   -1,  0,
#'    0, -1
#' ), nrow = 5, byrow = TRUE)
#' b <- c(1, 1, 1, 0, 0)
#' latte_count(list(A = A, b = b))
#'
#'
#'
#'
#'
#'
#'
#'
#'
#' }
#' 
#' 






count_core <- function(spec, dir = tempdir(), quiet = TRUE, mpoly = TRUE, ...){

  if (!has_latte()) missing_latte_stop()


  ## initialize specification
  specification <- "unknown"


  ## compute other args
  opts <- as.list(match.call(expand.dots = FALSE))[["..."]]
  if (is.null(opts)) opts <- list()
  

  ## look at opts
  if (("erhart_series" %in% names(opts)) && mpoly) {
    message("mpoly can't handle rational functions; reverting to raw output.")
    mpoly <- FALSE
  }

	if ("simplified_ehrhart_polynomial" %in% names(opts)) {
    stop("This option is not yet supported by latte.", call. = FALSE)
  }



  ## if the specification is pure code
  if(is.character(spec) && length(spec) == 1L){
    specification <- "code"
    code <- spec
  }




  ## if the specification is A and b
  if(is.list(spec) && length(spec) == 2L){

    bNegA <- unname(cbind(spec$b, -spec$A))
    spec  <- paste(dim(bNegA), collapse = " ")
    bNegA <- paste(apply(bNegA, 1, paste, collapse = " "), collapse = "\n")
    spec  <- paste(spec, bNegA, sep = "\n")
    spec  <- str_c("\n", spec)

    specification <- "code"
    code <- spec
  }



  ## check for vertex specification
  if(("vrep" %in% names(opts)) && isTRUE(opts[["vrep"]])) specification <- "vertex"

  if(
  	is.list(spec) && !is.mpolyList(spec) && 
  	(("vrep" %notin% names(opts)) || !isTRUE(opts[["vrep"]]))
  ){
    specification <- "vertex"
    message("Undeclared vertex specification, setting vrep = TRUE.")
    opts <- c(opts, "vrep" = TRUE)
  }




  ## if giving a character string of equations, parse
  ## each to the poly <= 0 format
  # this should transition to eq_to_mp
  if (is.character(spec) && length(spec) > 1) {

  	parsedSpec <- as.list(rep(NA, length(spec)))

  	geqNdcs <- which(str_detect(spec, " >= "))
  	leqNdcs <- which(str_detect(spec, " <= "))
  	eeqNdcs <- which(str_detect(spec, " == "))
  	eqNdcs  <- which(str_detect(spec, " = "))

    if (length(geqNdcs) > 0) {
      tmp <- strsplit(spec[geqNdcs], " >= ")
      parsedSpec[geqNdcs] <- lapply(tmp, function(v) mp(v[2]) - mp(v[1]))
    }

    if (length(leqNdcs) > 0) {
      tmp <- strsplit(spec[leqNdcs], " <= ")
      parsedSpec[leqNdcs] <- lapply(tmp, function(v) mp(v[1]) - mp(v[2]))
    }

    if (length(eeqNdcs) > 0) {
      tmp <- strsplit(spec[eeqNdcs], " == ")
      parsedSpec[eeqNdcs] <- lapply(tmp, function(v) mp(v[1]) - mp(v[2]))
    }

    if (length(eqNdcs) > 0) {
      tmp <- strsplit(spec[eqNdcs], " = ")
      parsedSpec[eqNdcs] <- lapply(tmp, function(v) mp(v[1]) - mp(v[2]))
    }

    linearityNdcs <- sort(c(eeqNdcs, eqNdcs))

    spec <- parsedSpec
    class(spec) <- "mpolyList"
  }




  ## convert the mpoly specification into a matrix, see latte manual, p. 8
  if (is.mpolyList(spec)) {
  	specification <- "hyperplane"
    if (!all(is.linear(spec))) stop("All polynomials must be linear.", call. = FALSE)
    mat <- mpoly_list_to_mat(spec)
    mat <- cbind(-mat[,"coef",drop=FALSE], -mat[,-ncol(mat)])

    # convert to code
    code <- paste(nrow(mat), ncol(mat))
    code <- str_c(code, "\n")
    code <- str_c(code,
      paste(apply(unname(mat), 1, paste, collapse = " "),
      collapse = "\n")
    )

    if(length(linearityNdcs) > 0){
      code <- str_c(code, "\n")
      code <- str_c(code,
        str_c("linearity ", length(linearityNdcs), " ",
          paste(linearityNdcs, collapse = " "))
      )
    }
  }




  ## convert vertex specification into a matrix
  if(specification == "vertex"){

  	if(any(!sapply(spec, function(v) length(v) != 1))){
  	  stop("Unequal number of coordinates in vertex specification.", call. = FALSE)
  	}

    mat <- matrix(unlist(spec), ncol = 2, byrow = TRUE)

    # convert to code
    mat <- cbind(1, mat)
    code <- paste(nrow(mat), ncol(mat))
    code <- str_c(code, "\n")
    code <- str_c(code,
      paste(apply(unname(mat), 1, paste, collapse = " "),
      collapse = "\n")
    )

  }




  ## make dir to put latte files in (within the tempdir) timestamped
  dir.create(project_dir <- file.path(dir, time_stamp()))




  ## write code file
  writeLines(code, con = file.path(project_dir, "count_code.latte"))




  ## switch to temporary directory
  user_working_directory <- getwd()
  setwd(project_dir); on.exit(setwd(user_working_directory), add = TRUE)




  ## run count
  call_latte("count", "count_code.latte", project_dir, opts)




  ## print count output when quiet = FALSE
  if(!quiet) cat(readLines("stdout"), sep = "\n")
  # note: strangely, latte posts non-error info to stderr
  if(!quiet) std_err <- readLines("stderr")
  if(!quiet && any(std_err != "")) message(str_c(std_err, collapse = "\n"))




  ## parse ehrhart polynomial
  if (
  	("ehrhart_polynomial" %in% names(opts)) && 
  	isTRUE(opts[["ehrhart_polynomial"]])
  ) {
    outPrint <- readLines("stdout")
    rawPoly <- rev(outPrint)[2]
    if(!mpoly) return(str_trim(rawPoly))
    rawPoly <- str_replace_all(rawPoly, " \\* ", " ")
    if(str_sub(rawPoly, 1, 5) == " + 1 ") rawPoly <- str_sub(rawPoly, 6)
    return(mp(str_trim(rawPoly)))
  }




  ## parse ehrhart series
  if (
  	("ehrhart_series" %in% names(opts)) && 
  	isTRUE(opts[["ehrhart_series"]])
  ) {
    outPrint <- readLines("count_code.latte.rat")

    # take off initial "x := " and terminating ":"
    outPrint <- str_sub(outPrint, start = 6, end = nchar(outPrint)-1)

    # return
    return(outPrint)
  }




  ## parse multivariate generating function
  if (
  	("multivariate_generating_function" %in% names(opts)) && 
  	isTRUE(opts[["multivariate_generating_function"]])
  ) {
    outPrint <- readLines("count_code.latte.rat")

    # collapse
    outPrint <- paste(outPrint, collapse = "")

    # return
    if(!mpoly) return(outPrint)

    # change x[0] to vars[1], and so on
    indets <- vars(spec)
    for(k in 1:length(indets)){
      outPrint <- str_replace_all(outPrint, str_c("x\\[",k-1,"\\]"), indets[k])
    }
    return(outPrint)
  }




  ## parse truncated taylor series
  if (
  	("ehrhart_taylor" %in% names(opts)) && 
  	isTRUE(opts[["ehrhart_taylor"]])
  ) {
    outPrint <- readLines("stdout")

    # collapse
    outPrint <- paste(outPrint, collapse = " + ")
    outPrint <- str_replace_all(outPrint, "t", " t")

    # return
    if(!mpoly) return(outPrint)
    return(mp(outPrint))
  }



  ## read in integer and parse if small enough
  out <- readLines("numOfLatticePoints")
  if(nchar(out) < 10) out <- as.integer(out)




  ## print out stats
  if(!quiet){
    cat(readLines("latte_stats"), sep = "\n")
    cat("\n")
  }




  ## out
  out
}

















#' @export
#' @rdname latte-count
latte_count <- memoise::memoise(count_core)





#' @export
#' @rdname latte-count
latte_fcount <- count_core
















mpoly_list_to_mat <- function(mpolyList){
  # this only works for linear mpolyList objects
  vars <- vars(mpolyList)
  varsC <- c(vars, "coef")
  vecMpolyList <- unclass(mpolyList)
  vecMpolyList <- lapply(vecMpolyList, unclass)
  vecMpolyList <- lapply(vecMpolyList, lapply, function(v){
    if(names(v)[1] == "coef") return(v)
    o <- v["coef"]
    names(o) <- names(v)[1]
    o
  })
  vecMpolyList <- lapply(vecMpolyList, unlist)
  vecMpolyList <- lapply(vecMpolyList, function(x){
    varsNeeded <- setdiff(varsC, names(x))
    if(length(varsNeeded) > 0){
      tmp <- rep(0, length(varsNeeded))
      names(tmp) <- varsNeeded
      x <- c(x, tmp)
      x <- x[varsC]
    }
    x
  })
  vecMpolyList <- lapply(vecMpolyList, function(x){
    df <- as.data.frame(t(x))
    row.names(df) <- runif(1) # another way?
    df
  })
  vecMpolyList  <- unsplit(vecMpolyList,
  1:length(vecMpolyList), drop = TRUE)
  row.names(vecMpolyList) <- 1:nrow(vecMpolyList)

  as.matrix(vecMpolyList)
}






