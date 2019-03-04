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
#' # these are stored in your .Renviron file; that's where you should put the 
#' # path to LattE and 4ti2 executables. for example, you should have a lines 
#' # that look like
#' # LATTE=/Applications/latte/bin
#' # 4TI2=/Applications/latte/bin
#' # you can set these with usethis::edit_r_environ() 
#'
#' # you can change these in your current session with set_latte_path() and 
#' # set_4ti2_path(), for example set_4ti2_path("/path/to/4ti2")
#' 
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
    Sys.setenv("4TI2" = `4ti2_path`)
    return(invisible(`4ti2_path`))

  } else if(!missing(path)){

    Sys.setenv("4TI2" = path)
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
get_4ti2_path <- function() Sys.getenv("4TI2")



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



