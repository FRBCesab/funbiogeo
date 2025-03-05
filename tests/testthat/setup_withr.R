#' Setup Tests Infrastructure
#' 

## Temporary Directory ----

create_tempdir <- function(path = file.path(tempdir(), "fb")) {
  
  old_wd <- getwd()
  
  withr::defer(fs::dir_delete(path), envir = parent.frame())
  
  dir.create(path)
  
  setwd(path)
  
  withr::defer(setwd(old_wd), envir = parent.frame())
  
  invisible(path)
}
