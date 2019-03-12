call_4ti2 <- function() {
  
  # make dir to put 4ti2 files in (within the tempdir) timestamped
  dir.create(dir2 <- file.path(dir, timeStamp()))
  
  
  # make 4ti2 file
  if(!missing(A)) write.latte(A, file.path(dir2, "PROJECT.mat"))
  
  
  # switch to temporary directory
  oldWd <- getwd()
  setwd(dir2); on.exit(setwd(oldWd), add = TRUE)
  
  
  # switch to temporary directory
  if (is_mac() || is_unix()) {
    
    system2(
      file.path(get_4ti2_path(), exec),
      paste(opts, file.path(dir2, "PROJECT")),
      stdout = glue("{exec}_out"), 
      stderr = glue("{exec}_err")
    )
    
    # generate shell code
    shell_code <- glue(
      "{file.path(get_4ti2_path(), exec)} {paste(opts, file.path(dir2, 'PROJECT'))} > {exec}_out 2> {exec}_err"
    )
    if(shell) message(shell_code)
    
  } else if (is_win()) {
    
    matFile <- file.path(dir2, "PROJECT")
    matFile <- chartr("\\", "/", matFile)
    matFile <- str_c("/cygdrive/c", str_sub(matFile, 3))
    
    system2(
      "cmd.exe",
      glue("/c env.exe {file.path(get_4ti2_path(), exec)} {opts} {matFile}"),
      stdout = glue("{exec}_out"), 
      stderr = glue("{exec}_err")
    )
    
    # generate shell code
    shell_code <- glue(
      "cmd.exe /c env.exe {file.path(get_4ti2_path(), exec)} {opts} {matFile} > {exec}_out 2> {exec}_err"
    )
    if(shell) message(shell_code)
    
  }
  
  
  if(!quiet) cat(readLines(glue("{exec}_out")), sep = "\n")
  std_err <- readLines(glue("{exec}_err"))
  if(any(std_err != "")) warning(str_c(std_err, collapse = "\n"), call. = FALSE)
  
  
}