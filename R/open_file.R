#' Open a file in editor
#' 
#' Heavily inspired by [usethis::edit_file()].
#' 
#' @param path a `character` of length 1. The path to file to open
#' 
#' @return Target path invisibly
#' 
#' @noRd

open_file <- function(path) {
  
  if (!requireNamespace("rstudioapi")) {
    
    utils::file.edit(path)
    
  } else if (
    rstudioapi::isAvailable() && rstudioapi::hasFun("navigateToFile")
  ) {
    
    rstudioapi::navigateToFile(path)
    
  } else {
    
    utils::file.edit(path)
    
  }
  
  invisible(path)
}
