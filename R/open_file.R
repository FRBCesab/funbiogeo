#' Open a file in editor
#' 
#' @param path a `character` of length 1. The path to file to open.
#' 
#' @return No return values.
#' 
#' @noRd

open_file <- function(path) {
  
  if (rstudioapi::isAvailable() && rstudioapi::hasFun("navigateToFile")) {
    
    rstudioapi::navigateToFile(path)
    
  } else {
    
    utils::file.edit(path)  
  }
  
  invisible(NULL)
}
