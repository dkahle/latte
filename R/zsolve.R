#' Generate a configuration matrix
#'
#' zsolve runs 4ti2's zsolve program to compute the
#' configuration matrix A corresponding to graphical statistical
#' models given by a simplicial complex and levels on the nodes.
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
#'
zsolve <- function(mat, rel, rhs, sign, lat, lb, ub,
  dir = tempdir(), quiet = TRUE, shell = FALSE, ...
){

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
  dir2 <- file.path2(dir, timeStamp())
  suppressWarnings(dir.create(dir2))


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
  if(is.mac() || is.unix()){

    system2(
      file.path2(getOption("4ti2_path"), "zsolve"),
      paste(opts, file.path2(dir2, "system")),
      stdout = paste0("zsolve", "Out"), stderr = FALSE
    )

    # generate shell code
    shell_code <- paste(
      file.path2(getOption("4ti2_path"), "zsolve"),
      paste(opts, file.path2(dir2, "system")),
      ">", paste0("zsolve", "Out")
    )
    if(shell) message(shell_code)

  } else if(is.win()){

    matFile <- file.path2(dir2, "system")
    matFile <- chartr("\\", "/", matFile)
    matFile <- str_c("/cygdrive/c", str_sub(matFile, 3))

    system2(
      "cmd.exe",
      paste(
        "/c env.exe",
        file.path(getOption("4ti2_path"), "zsolve"),
        opts, matFile
      ), stdout = paste0("zsolve", "Out"), stderr = FALSE
    )

    # generate shell code
    shell_code <- paste("cmd.exe",
      "/c env.exe", file.path(getOption("4ti2_path"), "zsolve"),
      opts, matFile, ">", paste0("zsolve", "Out")
    )
    if(shell) message(shell_code)

  }


  ## print output, if desired
  if(!quiet) cat(readLines(paste0("zsolve", "Out")), sep = "\n")


  ## read and return
  list(
    zinhom = read.latte("system.zinhom"),
    zhom = read.latte("system.zhom")
  )

}
