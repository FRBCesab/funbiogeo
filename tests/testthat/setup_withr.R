#' Setup Tests Infrastructure
#'

## Temporary Directory ----

create_tempdir <- function(path = file.path(tempdir(), "fb")) {
  withr::defer(fs::dir_delete(path), envir = parent.frame())

  dir.create(path)

  invisible(path)
}


## Detect Fedora OS (used to skip some spatial tests) ----

is_fedora <- function() {
  grepl("fedora", sessionInfo()$running, ignore.case = TRUE)
}
