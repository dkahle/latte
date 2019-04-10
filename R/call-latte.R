#' Unified interface to calling LattE
#'
#' Unified interface to calling LattE
#'
#' @param which_exe The name of LattE executable, e.g. \code{"count"}
#' @param input_file The filename of file to be given to the executable, without
#'   a path
#' @param project_dir The path associated with \code{input_file}.
#' @param opts A named list of optional command line arguments to the
#'   executable.
#' @param stdout,stderr Filenames to redirect standard output and standard error
#'   to.
#' @param shell \code{logical(1)}; should the shell command be messaged to the
#'   user?
#' @return \code{invisible()}
call_latte <- function(
	which_exe,
	input_file,
	project_dir,
	opts = list(),
	stdout = "stdout",
	stderr = "stderr",
	shell = FALSE
) {
	
	
	# translate args to string
	if (length(opts) == 0L) {
		opts <- ""
	} else {
		opts_names <- names(opts)
		opts_names <- str_replace_all(opts_names, "_", "-")
		opts <- str_c("--", opts_names, "=", unlist(opts))
		opts <- str_replace_all(opts, "=TRUE", "")
		opts <- str_c(opts, collapse = " ")
	}
	
	
	#  make call
	if (is_mac() || is_unix()) {
		
		system2(
			file.path(get_latte_path(), which_exe),
			paste(opts, file.path(project_dir, input_file)),
			stdout = stdout, 
			stderr = stderr
		)
		
	} else if (is_win()) {
		
		matFile <- file.path(project_dir, input_file)
		matFile <- chartr("\\", "/", matFile)
		matFile <- str_c("/cygdrive/c", str_sub(matFile, 3))
		
		system2(
			"cmd.exe",
			paste("/c env.exe", file.path(get_latte_path(), which_exe), opts, matFile), 
			stdout = stdout, 
			stderr = stderr
		)
		
	}
	
	
	invisible()
	
}