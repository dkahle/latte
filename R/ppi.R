#' Compute the primitive partition identities
#'
#' ppi runs 4ti2's ppi program to compute the primitive partition
#' identities, that is, the Graver basis of 1:N.
#'
#' @param N A postive integer > 2
#' @param dir Directory to place the files in, without an ending /
#' @param quiet If FALSE, messages the 4ti2 output
#' @param shell Messages the shell code used to do the computation
#' @param ... Additional arguments to pass to the function
#' @return A matrix containing the basis as its columns (for easy
#'   addition to tables)
#' @seealso [graver()]
#' @export
#' @examples
#'
#' if (has_4ti2()) {
#'
#' ppi(3)
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
ppi <- function(N, dir = tempdir(), quiet = TRUE,
    shell = FALSE, ...
){

  
  if (!has_4ti2()) missing_4ti2_stop()
  
  ## arg checking
  stopifnot(is.wholenumber(N) && N > 2)

  ## compute other args
  opts <- as.list(match.call(expand.dots = FALSE))[["..."]]
  if(is.null(opts)){
    opts <- ""
  } else {
    opts <- paste0("-", names(opts), "", unlist(opts))
    opts <- paste(opts, collapse = " ")
  }

  ## move to dir and run 4it2 ppi
  ####################################

  ## make dir to put 4ti2 files in (within the tempdir) timestamped
  dir2 <- file.path2(dir, timeStamp())
  suppressWarnings(dir.create(dir2))


  ## switch to temporary directory
  oldWd <- getwd()
  setwd(dir2); on.exit(setwd(oldWd), add = TRUE)


  ## run 4ti2
  if (is_mac() || is_unix()) {
  
    system2(
      file.path2(get_4ti2_path(), "ppi"),
      paste(opts, N),
      stdout = paste0("ppi", "Out"), stderr = FALSE
    )

    # generate shell code
    shell_code <- paste(
      file.path2(get_4ti2_path(), "ppi"),
      paste(opts, N),
      ">", paste0("ppi", "Out"), paste0("\n(in ", dir2, ")")
    )
    if(shell) message(shell_code)

  } else if (is_win()) {

    system2(
      "cmd.exe",
      paste(
        "/c env.exe",
        file.path(get_4ti2_path(), "ppi"),
        opts, N
      ), stdout = paste0("ppi", "Out"), stderr = FALSE
    )

    # generate shell code
    shell_code <- paste("cmd.exe",
      "/c env.exe", file.path(get_4ti2_path(), "ppi"),
      opts, N, ">", paste0("ppi", "Out"), paste0("\n(in ", dir2, ")")
    )
    if(shell) message(shell_code)

  }


  ## print output, if desired
  if(!quiet) cat(readLines(paste0("ppi", "Out")), sep = "\n")


  ## read and return
  t(read.latte(paste0("ppi", N, ".gra")))

}
