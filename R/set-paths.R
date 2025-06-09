#' Set paths to LattE and 4ti2 executables
#'
#' These are helper functions that deal with pathing to external programs and
#' asking if they are present. When latte is loaded it attempts to find LattE
#' and 4ti2 executables by looking for environment variables specifying them,
#' i.e. their paths as specified in your `.Renviron` file.
#'
#' For easiest use, you'll want to specify the paths of LattE and 4ti2
#' executables in your `~/.Renviron` file. They should look something like
#'
#' `LATTE=/Applications/latte/bin`
#'
#' `FOURTITWO=/Applications/latte/bin`
#'
#' You can set these permanently with [usethis::edit_r_environ()].
#'
#' You can change these for the current session using, e.g., [set_latte_path()],
#' which accepts a character string or, if missing, uses [file.choose()] to let
#' you interactively; you just select an arbitrary executable.
#'
#' @param path A character string, the path to a 4ti2 function (e.g. `markov`)
#'   for setting 4ti2's path or a LattE function (e.g. `count`) for LattE's path
#' @return A logical(1) or character(1) containing the path.
#' @name pathing
#' @author David Kahle \email{david@@kahle.io}
#' @examples
#'
#'
#' has_4ti2()
#' if (has_4ti2()) get_4ti2_path()
#'
#' has_latte()
#' if (has_4ti2()) get_latte_path()
#'
#'
#' # you can set these paths permanently with the following. note that you'll 
#' # need to re-start the R session afterwards or simply pass the path into, 
#' # e.g., set_latte_path(). see below for more details on that.
#' if (interactive()) edit_r_environ()
#'
#'
#' # you can change these in your current session with set_latte_path() and
#' if (had_latte <- has_latte()) old_latte_path <- get_latte_path()
#' set_latte_path("/path/to/latte")
#' get_latte_path()
#'
#' if (had_latte) set_latte_path(old_latte_path)
#' get_latte_path()
#' 
NULL












#' @rdname pathing
#' @export
set_latte_path <- function(path){

  if(missing(path) && interactive()){

    latte_path <- dirname(file.choose())
    if(is_win() && str_detect(latte_path,"C:/")){
      latte_path <- str_replace(dirname(latte_path), "C:/", "/cygdrive/c/")
    }
    Sys.setenv("LATTE" = latte_path)
    return(invisible(latte_path))

  } else if(!missing(path)){

    Sys.setenv("LATTE" = path)
    return(invisible(path))

  } else {
    
    stop(
      "If the session is not interactive, a path must be specified.",
      call. = FALSE
    )
    
  }
}













#' @rdname pathing
#' @export
set_4ti2_path <- function(path){

  if(missing(path) && interactive()){

    `4ti2_path` <- dirname(file.choose())
    if(is_win() && str_detect(`4ti2_path`,"C:/")){
      `4ti2_path` <- str_replace(`4ti2_path`, "C:/", "/cygdrive/c/")
    }
    Sys.setenv("FOURTITWO" = `4ti2_path`)
    return(invisible(`4ti2_path`))

  } else if(!missing(path)){

    Sys.setenv("FOURTITWO" = path)
    return(invisible(path))

  } else {
    
    stop(
      "If the session is not interactive, a path must be specified.",
      call. = FALSE
    )
    
  }
}









#' @rdname pathing
#' @export
get_4ti2_path <- function() {
	if (Sys.getenv("FOURTITWO") != "") {
		Sys.getenv("FOURTITWO")
	} else {
		Sys.getenv("4TI2")
	}
}



#' @rdname pathing
#' @export
get_latte_path <- function() Sys.getenv("LATTE")



#' @rdname pathing
#' @export
has_4ti2 <- function() get_4ti2_path() != ""



#' @rdname pathing
#' @export
has_latte <- function() get_latte_path() != ""



#' @rdname pathing
#' @export
missing_4ti2_stop <- function() {
  stop(
    "latte doesn't know where 4ti2 is.\n", 
    "See ?set_4ti2_path to learn how to set it.", 
    call. = FALSE
  )
}



#' @rdname pathing
#' @export
missing_latte_stop <- function() {
  stop(
    "latte doesn't know where LattE is.\n", 
    "See ?set_latte_path to learn how to set it.", 
    call. = FALSE
  )
}



#' @importFrom usethis edit_r_environ
#' @export
usethis::edit_r_environ



