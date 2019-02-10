#' Generate a configuration matrix
#'
#' genmodel runs 4ti2's genmodel program to compute the
#' configuration matrix A corresponding to graphical statistical
#' models given by a simplicial complex and levels on the nodes.
#'
#' @param varlvls a vector containing the number of levels of each
#'   variable
#' @param facets the facets generating the hierarchical model, a
#'   list of vectors of variable indices
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
#' varlvls <- rep(2, 2)
#' facets <- list(1, 2)
#' genmodel(varlvls, facets)
#'
#' varlvls <- rep(3, 3)
#' facets <- list(1:2, 2:3, c(3,1))
#' genmodel(varlvls, facets)
#'
#' # compare this to algstat's hmat function
#'
#' }
#'
genmodel <- function(varlvls, facets, dir = tempdir(), quiet = TRUE,
    shell = FALSE, ...
){

  ## compute other args
  opts <- as.list(match.call(expand.dots = FALSE))[["..."]]
  if(is.null(opts)){
    opts <- ""
  } else {
    opts <- paste0("-", names(opts), "", unlist(opts))
    opts <- paste(opts, collapse = " ")
  }


  ## compute/write 4ti2 code file
  ####################################

  ## format varlvls
  formatted_varlvls <- str_sub(format_latte(t(varlvls)), 3)

  ## format simplices
  formatted_facets <- vapply(
    facets,
    function(v) paste(length(v), paste(v, collapse = " ")),
    character(1)
  )
  formatted_facets <- paste(
    length(facets),
    paste(formatted_facets, collapse = "\n"),
    sep = "\n"
  )

  ## bring the two together
  code <- paste(formatted_varlvls, formatted_facets, sep = "\n")


  ## make dir to put 4ti2 files in (within the tempdir) timestamped
  dir2 <- file.path2(dir, timeStamp())
  suppressWarnings(dir.create(dir2))


  ## make 4ti2 file
  writeLines(code, con = file.path2(dir2, "PROJECT.mod"))


  ## move to dir and run 4it2 genmodel
  ####################################


  ## switch to temporary directory
  oldWd <- getwd()
  setwd(dir2); on.exit(setwd(oldWd), add = TRUE)


  ## run 4ti2
  if(is.mac() || is.unix()){

    system2(
      file.path2(getOption("4ti2_path"), "genmodel"),
      paste(opts, file.path2(dir2, "PROJECT")),
      stdout = paste0("genmodel", "Out"), stderr = FALSE
    )

    # generate shell code
    shell_code <- paste(
      file.path2(getOption("4ti2_path"), "genmodel"),
      paste(opts, file.path2(dir2, "PROJECT")),
      ">", paste0("genmodel", "Out")
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
        file.path(getOption("4ti2_path"), "genmodel"),
        opts, matFile
      ), stdout = paste0("genmodel", "Out"), stderr = FALSE
    )

    # generate shell code
    shell_code <- paste("cmd.exe",
      "/c env.exe", file.path(getOption("4ti2_path"), "genmodel"),
      opts, matFile, ">", paste0("genmodel", "Out")
    )
    if(shell) message(shell_code)

  }


  ## print output, if desired
  if(!quiet) cat(readLines(paste0("genmodel", "Out")), sep = "\n")


  ## read and return
  read.latte(paste0("PROJECT", ".mat"))

}
