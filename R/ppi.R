#' Compute the primitive partition identities
#'
#' ppi runs 4ti2's ppi program to compute the primitive partition identities,
#' that is, the Graver basis of 1:N.
#'
#' @param N A postive integer > 2
#' @param dir Directory to place the files in, without an ending /
#' @param quiet If FALSE, messages the 4ti2 output
#' @param shell Messages the shell code used to do the computation
#' @param ... Additional arguments to pass to the function
#' @return A matrix containing the basis as its columns (for easy addition to
#'   tables)
#' @seealso [graver()]
#' @export
#' @examples
#'
#' if (has_4ti2()) {
#'
#' ppi(3)
#' t(ppi(3)) %*% 1:3
#' plot_matrix(ppi(3))
#'
#' graver(t(1:3))
#' plot_matrix(graver(t(1:3)))
#'
#' ppi(5, quiet = FALSE, shell = TRUE)
#'
#' }
#'
#' 
ppi <- function(N, dir = tempdir(), quiet = TRUE, shell = FALSE, ...){

  
  if (!has_4ti2()) missing_4ti2_stop()
  
  ## arg checking
  stopifnot(is.wholenumber(N) && N > 2)

  ## compute other args
  opts <- as.list(match.call(expand.dots = FALSE))[["..."]]
  if(is.null(opts)){
    opts <- ""
  } else {
    opts <- str_c("-", names(opts), "", unlist(opts))
    opts <- str_c(opts, collapse = " ")
  }

  ## move to dir and run 4it2 ppi
  ####################################

  ## make dir to put 4ti2 files in (within the tempdir) timestamped
  dir.create(scratch_dir <- file.path(dir, time_stamp()))


  ## switch to temporary directory
  user_working_directory <- getwd()
  setwd(scratch_dir); on.exit(setwd(user_working_directory), add = TRUE)


  ## run 4ti2
  if (is_mac() || is_unix()) {
  
    system2(
      file.path(get_4ti2_path(), "ppi"),
      paste(opts, N),
      stdout = "ppi_out", 
      stderr = "ppi_err"
    )
    
    # generate shell code
    shell_code <- glue(
      "{file.path(get_4ti2_path(), 'ppi')} {paste(opts, file.path(scratch_dir, 'system'))} > ppi_out 2> ppi_err"
    )
    if(shell) message(shell_code)

  } else if (is_win()) {
    
    system2(
      "cmd.exe",
      glue("/c env.exe {file.path(get_4ti2_path(), 'ppi')} {opts} {N}"),
      stdout = "ppi_out", 
      stderr = "ppi_err"
    )
    
    # generate shell code
    shell_code <- glue(
      "cmd.exe /c env.exe {file.path(get_4ti2_path(), 'ppi')} {opts} {N} > ppi_out 2> ppi_err"
    )
    if(shell) message(shell_code)

  }


  ## print output, if desired
  if(!quiet) cat(readLines("ppi_out"), sep = "\n")


  ## read and return
  t(read.latte(str_c("ppi", N, ".gra")))

}
