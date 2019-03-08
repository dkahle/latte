.onAttach <- function(...) {

  packageStartupMessage('  Please cite latte! See citation("latte") for details.')
  
  if (!has_4ti2()) {
    packageStartupMessage("  - 4ti2 was not set in .Renviron. Use set_4ti2_path() to set it.")
  }
  
  if (!has_latte()) {
    packageStartupMessage("  - LattE was not in .Renviron. Use set_latte_path() to set it.")
  }
  
  invisible(TRUE)
  
}




