#' Compute a basis with 4ti2
#'
#' 4ti2 provides several executables that can be used to generate
#' bases for a configuration matrix A.  See the references for
#' details.
#'
#' @param A The configuration matrix
#' @param format How the basis (moves) should be returned.  if
#'   "mat", the moves are returned as the columns of a matrix.
#' @param dim The dimension to be passed to \code{\link{vec2tab}} if
#'   format = "tab" is used; a vector of the number of levels of
#'   each variable in order
#' @param all If TRUE, all moves (+ and -) are given.  if FALSE,
#'   only the + moves are given as returned by the executable.
#' @param dir Directory to place the files in, without an ending /
#' @param quiet If FALSE, messages the 4ti2 output
#' @param shell Messages the shell code used to do the computation
#' @param dbName The name of the model in the markov bases database,
#'   http://markov-bases.de, see examples
#' @param ... Additional arguments to pass to the function
#' @return a matrix containing the Markov basis as its columns (for
#'   easy addition to tables)
#' @name fourTiTwo
#' @references Drton, M., B. Sturmfels, and S. Sullivant (2009).
#'   \emph{Lectures on Algebraic Statistics}, Basel: Birkhauser
#'   Verlag AG.
#' @examples
#'
#' \dontrun{ these examples require having 4ti2 installed
#'
#'
#'
#'
#' # basic input and output for the 2x2 independence example
#' (A <- rbind(
#'   kprod(diag(3), ones(1,3)),
#'   kprod(ones(1,3), diag(3))
#' ))
#' markov(A)
#'
#'
#'
#' # you can get the output formatted in different ways:
#' markov(A, all = TRUE)
#' markov(A, "vec")
#' markov(A, "tab", c(3, 3))
#' tableau(markov(A), dim = c(3, 3)) # tableau notation
#'
#'
#'
#' # you can add options by listing them off
#' # to see the options available to you by function,
#' # go to http://www.4ti2.de
#' markov(A, p = "arb")
#'
#'
#'
#' # the basis functions are automatically cached for future use.
#' # (note that it doesn't persist across sessions.)
#' A <- rbind(
#'   kprod(  diag(4), ones(1,4), ones(1,4)),
#'   kprod(ones(1,4),   diag(4), ones(1,4)),
#'   kprod(ones(1,4), ones(1,4),   diag(4))
#' )
#' system.time(markov(A))
#' system.time(markov(A))
#'
#' # the un-cashed versions begin with an "f"
#' # (think: "forgetful" markov)
#' system.time(fmarkov(A))
#' system.time(fmarkov(A))
#'
#'
#'
#' # you can see the code used by typing shell = TRUE
#' # but since the function is cached, it is only shown
#' # the first time the function is run
#' (A <- rbind(
#'   kprod(diag(2), ones(1,4)),
#'   kprod(ones(1,4), diag(2))
#' ))
#' markov(A, shell = TRUE)
#' markov(A, shell = TRUE) # no message
#' fmarkov(A, shell = TRUE)
#'
#'
#'
#' # compare the bases for the 3x3x3 no-three-way interaction model
#' A <- rbind(
#'   kprod(  diag(3),   diag(3), ones(1,3)),
#'   kprod(  diag(3), ones(1,3),   diag(3)),
#'   kprod(ones(1,3),   diag(3),   diag(3))
#' )
#' str(zbasis(A))   #    8 elements = ncol(A) - qr(A)$rank
#' str(markov(A))   #   81 elements
#' str(groebner(A)) #  110 elements
#' str(graver(A))   #  795 elements
#'
#'
#'
#' # the other bases are also cached
#' A <- rbind(
#'   kprod(  diag(3), ones(1,3), ones(1,2)),
#'   kprod(ones(1,3),   diag(3), ones(1,2)),
#'   kprod(ones(1,3), ones(1,3),   diag(2))
#' )
#' system.time(graver(A))
#' system.time(graver(A))
#' system.time(fgraver(A))
#' system.time(fgraver(A))
#'
#'
#'
#' # LAS ex 1.2.1, p.12 : 2x3 independence
#' (A <- rbind(
#'   kprod(diag(2), ones(1,3)),
#'   kprod(ones(1,2), diag(3))
#' ))
#'
#' markov(A, "tab", c(3, 3))
#' # Prop 1.2.2 says that there should be
#' 2*choose(2, 2)*choose(3,2) # = 6
#' # moves (up to +-1)
#' markov(A, "tab", c(3, 3), TRUE)
#'
#'
#'
#' # LAS example 1.2.12, p.17  (no 3-way interaction)
#' (A <- rbind(
#'   kprod(  diag(2),   diag(2), ones(1,2)),
#'   kprod(  diag(2), ones(1,2),   diag(2)),
#'   kprod(ones(1,2),   diag(2),   diag(2))
#' ))
#' plot_matrix(A)
#' markov(A)
#' groebner(A)
#' graver(A)
#' tableau(markov(A), dim = c(2,2,2))
#'
#'
#'
#'
#'
#' # using the markov bases database, must be connected to internet
#' # A <- markov(dbName = "ind3-3")
#' B <- markov(rbind(
#'   kprod(diag(3), ones(1,3)),
#'   kprod(ones(1,3), diag(3))
#' ))
#' # all(A == B)
#'
#'
#'
#'
#'
#' # possible issues
#' markov(diag(1, 10))
#' zbasis(diag(1, 10), "vec")
#' groebner(diag(1, 10), "vec", all = TRUE)
#' graver(diag(1, 10), "vec", all = TRUE)
#' graver(diag(1, 4), "tab", all = TRUE, dim = c(2,2))
#'
#' }
#'
#'







#' @param exec don't use this parameter
#' @param memoise don't use this parameter
basis <- function(exec, memoise = TRUE){

  ## stuff in basis
  extension <- switch(exec,
    markov = ".mar",
    groebner = ".gro",
    hilbert = ".hil",
    graver = ".gra",
    zbasis = ".lat",
    zsolve = ".zfree"
  )

  commonName = switch(exec,
    markov = "markov",
    groebner = "grobner",
    hilbert = "hilbert",
    graver = "graver",
    zbasis = "lattice",
    zsolve = "solve"
  )


  ## memoise or not
  mem_or_not <- if(memoise) memoise::memoise else function(x) x


  ## create the function to return
  mem_or_not(function(A, format = c("mat", "vec", "tab"), dim = NULL,
    all = FALSE, dir = tempdir(), quiet = TRUE, shell = FALSE, dbName = NULL,
    ...
  ){

    ## check for 4ti2
    program_not_found_stop("4ti2_path")


    ## check args
    format <- match.arg(format)
    if(format == "tab" && missing(dim)){
      stop('if format = "tab" is specified, dim must be also.', call. = FALSE)
    }


    ## compute other args
    opts <- as.list(match.call(expand.dots = FALSE))[["..."]]
    if(is.null(opts)){
      opts <- ""
    } else {
      opts <- paste0("-", names(opts), "", unlist(opts))
      opts <- paste(opts, collapse = " ")
    }



    ## make dir to put 4ti2 files in (within the tempdir) timestamped
    dir2 <- file.path2(dir, timeStamp())
    suppressWarnings(dir.create(dir2))


    ## make 4ti2 file
    if(!missing(A)) write.latte(A, file.path2(dir2, "PROJECT.mat"))


    ## switch to temporary directory
    oldWd <- getwd()
    setwd(dir2); on.exit(setwd(oldWd), add = TRUE)


    ## create/retrieve markov basis
    if(is.null(dbName)){

      ## run 4ti2 if needed
      if(is.mac() || is.unix()){

        system2(
          file.path2(getOption("4ti2_path"), exec),
          paste(opts, file.path2(dir2, "PROJECT")),
          stdout = paste0(exec, "Out"), stderr = FALSE
        )

        # generate shell code
        shell_code <- paste(
          file.path2(getOption("4ti2_path"), exec),
          paste(opts, file.path2(dir2, "PROJECT")),
          ">", paste0(exec, "Out")
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
            file.path(getOption("4ti2_path"), exec),
            opts, matFile
          ), stdout = paste0(exec, "Out"), stderr = FALSE
        )

        # generate shell code
        shell_code <- paste("cmd.exe",
          "/c env.exe", file.path(getOption("4ti2_path"), exec),
          opts, matFile, ">", paste0(exec, "Out")
        )
        if(shell) message(shell_code)

      }


      if(!quiet) cat(readLines(paste0(exec, "Out")), sep = "\n")

    } else { # if the model name is specified

      download.file(
        paste0("http://markov-bases.de/data/", dbName, "/", dbName, extension),
        destfile = "PROJECT.mar" # already in tempdir
      )

    }

    ## fix case of no graver basis
    if(exec == "graver"){
      if(paste0("PROJECT", extension) %notin% list.files(dir2)){
        warning(sprintf("%s basis empty, returning 0's.", capitalize(commonName)), call. = FALSE)
        return(fix_graver(A, format, dim))
      }
    }


    ## figure out what files to keep them, and make 4ti2 object
    basis <- t(read.latte(paste0("PROJECT", extension)))


    ## fix case of no basis
    basisDim <- dim(basis)
    noBasisFlag <- FALSE
    if(any(basisDim == 0)){
      noBasisFlag <- TRUE
      warning(sprintf("%s basis empty, returning 0's.", capitalize(commonName)), call. = FALSE)
      basisDim[basisDim == 0] <- 1L
      basis <- rep(0L, prod(basisDim))
      dim(basis) <- basisDim
    }


    ## format
    if(all && !noBasisFlag) basis <- cbind(basis, -basis)


    # out
    if(format == "mat"){
      return(basis)
    } else {
      lbasis <- as.list(rep(NA, ncol(basis)))
      for(k in 1:ncol(basis)) lbasis[[k]] <- basis[,k]
      if(format == "vec") return(lbasis)
      if(format == "tab") return(lapply(lbasis, vec2tab, dim = dim))
    }
  })
}





# #' @export
# #' @rdname fourTiTwo
# zsolve <- basis("zsolve", memoise = TRUE)

#' @export
#' @rdname fourTiTwo
zbasis <- basis("zbasis", memoise = TRUE)

#' @export
#' @rdname fourTiTwo
markov <- basis("markov", memoise = TRUE)

#' @export
#' @rdname fourTiTwo
groebner <- basis("groebner", memoise = TRUE)

#' @export
#' @rdname fourTiTwo
hilbert <- basis("hilbert", memoise = TRUE)

#' @export
#' @rdname fourTiTwo
graver <- basis("graver", memoise = TRUE)






# #' @export
# #' @rdname fourTiTwo
# fzsolve <- basis("zsolve", memoise = FALSE)

#' @export
#' @rdname fourTiTwo
fzbasis <- basis("zbasis", memoise = FALSE)

#' @export
#' @rdname fourTiTwo
fmarkov <- basis("markov", memoise = FALSE)

#' @export
#' @rdname fourTiTwo
fgroebner <- basis("groebner", memoise = FALSE)

#' @export
#' @rdname fourTiTwo
fhilbert <- basis("hilbert", memoise = FALSE)

#' @export
#' @rdname fourTiTwo
fgraver <- basis("graver", memoise = FALSE)




fix_graver <- function(A, format, dim){
  if(format == "mat"){
    return(matrix(0L, nrow = ncol(A)))
  } else {
    lbasis <- list(rep(0L, ncol(A)))
    if(format == "vec") return(lbasis)
    if(format == "tab") return(lapply(lbasis, vec2tab, dim = dim))
  }
}
