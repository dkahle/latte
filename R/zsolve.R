#' Solve a linear system over the integers
#'
#' zsolve runs 4ti2's zsolve program to compute the configuration matrix A
#' corresponding to graphical statistical models given by a simplicial complex
#' and levels on the nodes.
#'
#' @param mat The A matrix (see the 4ti2 documentation or examples)
#' @param rel A vector of "<" or ">" relations
#' @param rhs The right hand side b
#' @param sign The signs of the individual
#' @param lat A lattice basis (instead of a matrix)
#' @param lb Lower bounds on columns
#' @param ub Upper bounds on columns
#' @param dir Directory to place the files in, without an ending /
#' @param quiet If FALSE, messages the 4ti2 output
#' @param shell Messages the shell code used to do the computation
#' @param ... Additional arguments to pass to the function
#' @return The configuration matrix of the model provided
#' @export
#' @examples
#'
#' if (has_4ti2()) {
#'
#' mat <- rbind(
#'   c( 1, -1),
#'   c(-3,  1),
#'   c( 1,  1)
#' )
#' rel <- c("<", "<", ">")
#' rhs <- c(2, 1, 1)
#' sign <- c(0, 1)
#'
#' zsolve(mat, rel, rhs, sign)
#' zsolve(mat, rel, rhs, sign, quiet = FALSE)
#' zsolve(mat, rel, rhs, sign, shell = TRUE)
#'
#' zsolve(mat, rel, rhs, sign, p = "gmp", quiet = FALSE)
#'
#' }
#'
#' 
zsolve <- function(mat, rel, rhs, sign, lat, lb, ub,
  dir = tempdir(), quiet = TRUE, shell = FALSE, ...
){
  
  if (!has_4ti2()) missing_4ti2_stop()

  ## compute other args
  opts <- as.list(match.call(expand.dots = FALSE))[["..."]]
  if(is.null(opts)){
    opts <- ""
  } else {
    opts <- paste0("-", names(opts), "", unlist(opts))
    opts <- paste(opts, collapse = " ")
  }


  ## create and move to dir
  ####################################

  ## make dir to put 4ti2 files in (within the tempdir) timestamped
  dir.create(dir2 <- file.path(dir, timeStamp()))


  ## switch to temporary directory
  oldWd <- getwd()
  setwd(dir2); on.exit(setwd(oldWd), add = TRUE)


  ## arg check
  ####################################
  if(missing(mat) && missing(lat))
    stop("Either mat or lat must be specified.", call. = FALSE)

  if(!missing(mat) && !all(is.wholenumber(mat)))
    stop("The entries of mat must all be integers.")

  if(!missing(lat) && !all(is.wholenumber(lat)))
    stop("The entries of lat must all be integers.")

  if(!missing(rhs) && !all(is.wholenumber(rhs)))
    stop("The entries of rhs must all be integers.")

  if(!all(rel %in% c("<", ">")))
    stop("rel must be a vector of \"<\"'s or \">\"'s.")


  ## write files
  ####################################

  if(!missing(mat)) write.latte(mat, "system.mat")
  write.latte(t(rel), "system.rel")
  write.latte(t(rhs), "system.rhs")
  if(!missing(sign)) write.latte(t(sign), "system.sign")
  if(missing(mat) && !missing(lat)) write.latte(mat, "system.lat")
  if(!missing(lb)) write.latte(mat, "system.lb")
  if(!missing(ub)) write.latte(mat, "system.ub")


  ## move to dir and run 4it2 zsolve
  ####################################

  ## run 4ti2
  if (is_mac() || is_unix()) {
    
    system2(
      file.path(get_4ti2_path(), "zsolve"),
      paste(opts, file.path(dir2, "system")),
      stdout = "zsolve_out", 
      stderr = "zsolve_err"
    )
    
    # generate shell code
    shell_code <- glue(
      "{file.path(get_4ti2_path(), 'zsolve')} {paste(opts, file.path(dir2, 'system'))} > zsolve_out 2> zsolve_err"
    )
    if(shell) message(shell_code)

  } else if (is_win()) {

    matFile <- file.path(dir2, "system")
    matFile <- chartr("\\", "/", matFile)
    matFile <- str_c("/cygdrive/c", str_sub(matFile, 3))

    system2(
      "cmd.exe",
      glue("/c env.exe {file.path(get_4ti2_path(), 'zsolve')} {opts} {matFile}"),
      stdout = "zsolve_out", 
      stderr = "zsolve_err"
    )
    
    # generate shell code
    shell_code <- glue(
      "cmd.exe /c env.exe {file.path(get_4ti2_path(), 'zsolve')} {opts} {matFile} > zsolve_out 2> zsolve_err"
    )
    if(shell) message(shell_code)

  }


  ## print output, if desired
  if(!quiet) message(paste(readLines("zsolve_out"), "\n"))
  std_err <- readLines("zsolve_err")
  if(any(std_err != "")) warning(str_c(std_err, collapse = "\n"), call. = FALSE)


  ## read and return
  list(
    zinhom = read.latte("system.zinhom"),
    zhom = read.latte("system.zhom")
  )

}
