#' Solve a linear system over the rationals
#'
#' qsolve runs 4ti2's qsolve program to compute the
#' configuration matrix A corresponding to graphical statistical
#' models given by a simplicial complex and levels on the nodes.
#'
#' @param mat The A matrix (see the 4ti2 documentation or examples)
#' @param rel A vector of "<" or ">" relations
#' @param sign The signs of the individual
#' @param dir Directory to place the files in, without an ending /
#' @param quiet If FALSE, messages the 4ti2 output
#' @param shell Messages the shell code used to do the computation
#' @param ... Additional arguments to pass to the function
#' @return The configuration matrix of the model provided
#' @export
#' @examples
#'
#' \dontrun{ requires 4ti2
#'
#' # x + y > 0
#' # x + y < 0
#'
#' mat <- rbind(
#'   c( 1,  1),
#'   c( 1,  1)
#' )
#' rel <- c(">", "<")
#' sign <- c(0, 0)
#'
#' qsolve(mat, rel, sign)
#' qsolve(mat, rel, sign, quiet = FALSE)
#' qsolve(mat, rel, sign, shell = TRUE)
#'
#' qsolve(mat, rel, sign, p = "arb", quiet = FALSE)
#' 
#' }
#'
#'
qsolve <- function(mat, rel, sign,
  dir = tempdir(), quiet = TRUE, shell = FALSE, ...
){

  ## compute other args
  opts <- as.list(match.call(expand.dots = FALSE))[["..."]]
  if("rhs" %in% names(opts)) stop("qsolve only solve homogeneous systems (b = 0).")
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

  if(!missing(mat) && !all(is.wholenumber(mat)))
    stop("The entries of mat must all be integers.")

  if(!missing(sign) && !all(is.wholenumber(sign)))
    stop("The entries of sign must all be integers.")

  if(!all(rel %in% c("<", ">")))
    stop("rel must be a vector of \"<\"'s or \">\"'s.")


  ## write files
  ####################################

  if(!missing(mat)) write.latte(mat, "PROJECT.mat")
  write.latte(t(rel), "PROJECT.rel")
  if(!missing(sign)) write.latte(t(sign), "PROJECT.sign")


  ## move to dir and run 4it2 qsolve
  ####################################

  ## run 4ti2
  if(is.mac() || is.unix()){

    system2(
      file.path2(getOption("4ti2_path"), "qsolve"),
      paste(opts, file.path2(dir2, "PROJECT")),
      stdout = paste0("qsolve", "Out"), stderr = FALSE
    )

    # generate shell code
    shell_code <- paste(
      file.path2(getOption("4ti2_path"), "qsolve"),
      paste(opts, file.path2(dir2, "PROJECT")),
      ">", paste0("qsolve", "Out")
    )
    if(shell) message(shell_code)

  } else if(is.win()){

    matFile <- file.path2(dir2, "PROJECT")
    matFile <- chartr("\\", "/", matFile)
    matFile <- str_c("/cygdrive/c", str_sub(matFile, 3))

    system2(
      "cmd.exe",
      paste(
        "/c env.exe",
        file.path(getOption("4ti2_path"), "qsolve"),
        opts, matFile
      ), stdout = paste0("qsolve", "Out"), stderr = FALSE
    )

    # generate shell code
    shell_code <- paste("cmd.exe",
      "/c env.exe", file.path(getOption("4ti2_path"), "qsolve"),
      opts, matFile, ">", paste0("qsolve", "Out")
    )
    if(shell) message(shell_code)

  }


  ## print output, if desired
  if(!quiet) cat(readLines(paste0("qsolve", "Out")), sep = "\n")


  ## read and return
  list(
    qhom = read.latte("PROJECT.qhom"),
    qfree = read.latte("PROJECT.qfree")
  )

}
