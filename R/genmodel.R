#' Generate a configuration matrix
#'
#' genmodel runs 4ti2's genmodel program to compute the configuration matrix A
#' corresponding to graphical statistical models given by a simplicial complex
#' and levels on the nodes.
#'
#' @param varlvls a vector containing the number of levels of each variable
#' @param facets the facets generating the hierarchical model, a list of vectors
#'   of variable indices
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
#' varlvls <- rep(2, 2)
#' facets <- list(1, 2)
#' genmodel(varlvls, facets)
#' genmodel(varlvls, facets, quiet = FALSE)
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
  
  if (!has_4ti2()) missing_4ti2_stop()
  

  ## compute other args
  opts <- as.list(match.call(expand.dots = FALSE))[["..."]]
  if (is.null(opts)) {
    opts <- ""
  } else {
    opts <- str_c("-", names(opts), "", unlist(opts))
    opts <- str_c(opts, collapse = " ")
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
  dir.create(scratch_dir <- file.path(dir, time_stamp()))


  ## make 4ti2 file
  writeLines(code, con = file.path(scratch_dir, "PROJECT.mod"))


  ## move to dir and run 4it2 genmodel
  ####################################


  ## switch to temporary directory
  user_working_directory <- getwd()
  setwd(scratch_dir); on.exit(setwd(user_working_directory), add = TRUE)


  ## run 4ti2
  if (is_mac() || is_unix()) {

    system2(
      file.path(get_4ti2_path(), "genmodel"),
      paste(opts, file.path(scratch_dir, "PROJECT")),
      stdout = "genmodel_out", 
      stderr = "genmodel_err"
    )

    # generate shell code
    shell_code <- glue(
      "{file.path(get_4ti2_path(), 'genmodel')} {paste(opts, file.path(scratch_dir, 'PROJECT'))} > genmodel_out 2> genmodel_err"
    )
    if(shell) message(shell_code)

  } else if (is_win()) {

    matFile <- file.path(scratch_dir, "PROJECT")
    matFile <- chartr("\\", "/", matFile)
    matFile <- str_c("/cygdrive/c", str_sub(matFile, 3))

    system2(
      "cmd.exe",
      glue("/c env.exe {file.path(get_4ti2_path(), 'genmodel')} {opts} {matFile}"),
      stdout = "genmodel_out", 
      stderr = "genmodel_err"
    )

    # generate shell code
    shell_code <- glue(
      "cmd.exe /c env.exe {file.path(get_4ti2_path(), 'genmodel')} {opts} {matFile} > genmodel_out 2> genmodel_err"
    )
    if(shell) message(shell_code)

  }


  ## print output, if desired
  if(!quiet) message(paste0(readLines("genmodel_out"), "\n"))
  std_err <- readLines("genmodel_err")
  if(any(std_err != "")) warning(str_c(std_err, collapse = "\n"), call. = FALSE)

  ## read and return
  read.latte(paste0("PROJECT", ".mat"))

}
