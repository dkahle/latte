#' Set paths to external functions
#'
#' These functions set the path to external programs either by (1)
#' passing them a character string or (2) using
#' \code{\link{file.choose}}.
#'
#' @param path A character string, the path to a 4ti2 function (e.g.
#'   markov) for setting 4ti2's path or a LattE function (e.g.
#'   count) for LattE's path
#' @return An invisible character string, the path found.  More
#'   importantly, the function has the side effect of setting the
#'   option "latte_path" or "4ti2_path"
#' @name setPaths
#' @author David Kahle \email{david.kahle@@gmail.com}
#' @examples
#'
#' \dontrun{ # the below code requires suggested external software
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












#' @rdname setPaths
#' @aliases set_latte_path
#' @export
set_latte_path <- function(path){

  if(missing(path) && interactive()){

    latte_path <- dirname(file.choose())
    if(is.win() && str_detect(latte_path,"C:/")){
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


#' @export
set_latte_path <- set_latte_path











#' @rdname setPaths
#' @export
set_4ti2_path <- function(path){

  if(missing(path) && interactive()){

    `4ti2_path` <- dirname(file.choose())
    if(is.win() && str_detect(`4ti2_path`,"C:/")){
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
