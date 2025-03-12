#' Setup Tests Infrastructure
#' 

## Temporary Directory ----

create_tempdir <- function(path = file.path(tempdir(), "fb")) {
  
  withr::defer(fs::dir_delete(path), envir = parent.frame())
  
  dir.create(path)
  
  invisible(path)
}
