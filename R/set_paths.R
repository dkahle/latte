#' Set paths to LattE and 4ti2 executables
#'
#' These functions set the path to external programs either by (1) passing them
#' a character string or (2) using [file.choose()].
#'
#' When latte is loaded it attempts to find LattE and 4ti2 executables
#' (represented by count and markov, respectively).  How it looks depends on
#' your operating system.
#'
#' If you're using a Mac or Linux machine, it looks based on your system's path.
#' Unfortunately, R changes the system path in such a way that the path that R
#' sees is not the same as the path that you'd see if you were working in the
#' terminal. (You can open the Terminal app on a Mac by going to
#' /Applications/Utilities/Terminal.)  Consequently, latte tries to guess the
#' file in which your path is set.  To do so, it first checks if your home
#' directory (type echo ~/ in the terminal to figure out which directory this is
#' if you don't know) for the file named .bash_profile.  If this file is
#' present, it runs it and then checks your system's path variable (echo $PATH).
#' If it's not present, it does the same for .bashrc and then .profile. In any
#' case, once it has its best guess at your path, it looks for "latte".
#'
#' On Windows, latte just uses [Sys.which()] on "whereis" to On Windows, latte
#' just uses [Sys.which()] on "whereis" to determine where the executables count
#' and markov are (for LattE and 4ti2, respectively).
#'
#' @param path A character string, the path to a 4ti2 function (e.g. markov) for
#'   setting 4ti2's path or a LattE function (e.g. count) for LattE's path
#' @return An invisible character string, the path found.  More importantly, the
#'   function has the side effect of setting the option "latte_path" or
#'   "4ti2_path"
#' @name set_paths
#' @author David Kahle \email{david@@kahle.io}
#' @examples
#'
#' \dontrun{ requires LattE and 4ti2
#'
#' ## for LattE
#' getOption("latte_path")
#' set_latte_path()
#'
#' ## for 4ti2 (typically the same as LattE)
#' getOption("4ti2_path")
#' set_4ti2_path()
#'
#'
#'
#' ## each of these functions can be used statically as well
#' (`4ti2_path` <- getOption("4ti2_path"))
#' set_4ti2_path("/path/to/4ti2/directory")
#' getOption("4ti2_path")
#' set_4ti2_path(4ti2_path) # undoes example
#'
#'
#'
#' }
#' 
NULL












#' @rdname set_paths
#' @export
set_latte_path <- function(path){

  if(missing(path) && interactive()){

    latte_path <- dirname(file.choose())
    if(is_win() && str_detect(latte_path,"C:/")){
      latte_path <- str_replace(dirname(latte_path), "C:/", "/cygdrive/c/")
    }
    options(latte_path = latte_path)
    return(invisible(latte_path))

  } else if(!missing(path)){

    options(latte_path = path)
    return(invisible(path))

  } else {
    stop(
      "If the session is not interactive, a path must be specified.",
      call. = FALSE
    )
  }
}













#' @rdname set_paths
#' @export
set_4ti2_path <- function(path){

  if(missing(path) && interactive()){

    `4ti2_path` <- dirname(file.choose())
    if(is_win() && str_detect(`4ti2_path`,"C:/")){
      `4ti2_path` <- str_replace(`4ti2_path`, "C:/", "/cygdrive/c/")
    }
    options(`4ti2_path` = `4ti2_path`)
    return(invisible(`4ti2_path`))

  } else if(!missing(path)){

    options(`4ti2_path` = path)
    return(invisible(path))

  } else {
    stop(
      "If the session is not interactive, a path must be specified.",
      call. = FALSE
    )
  }
}
