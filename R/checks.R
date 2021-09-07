#' Check site-species object format
#'
#' The object should be either a `data.frame` or a `matrix` with only positive
#' real numbers or `NAs` (of the good type!).
#' 
#' @param site_sp Potential site-species matrix
#'
#' @return nothing but errors if input data is not consistent.
check_site_sp = function(site_sp) {
  if (!is.data.frame(site_sp) & !is.matrix(site_sp)) {
    stop("Provided site-species object is not a numeric matrix or a data.frame",
         call. = FALSE)
  }
  
  if (0 %in% dim(site_sp)) {
    stop("Provided site-species object should have at least one row and ",
         "one column", call. = FALSE)
  }
  
  if (!is.numeric(as.matrix(site_sp))) {
    stop("Provided site-species object is not a numeric matrix or a data.frame",
         call. = FALSE)
  }
  
  if (any(site_sp < 0, na.rm = TRUE)) {
    stop("Provided site-species matrix contains negative values", call. = FALSE)
  }
} 

#' Check trait matrix format
#' 
#' The object should be either a `data.frame` or a `matrix`
#' 
#' @param traits 
#'
#' @return
#'
#' @examples
check_traits = function(traits) {
  if (!is.data.frame(traits) & !is.matrix(traits)) {
    stop("Provided species-traits object is not a numeric matrix or ",
         "a data.frame", call. = FALSE)
  }
  
  if (0 %in% dim(traits)) {
    stop("Provided species-traits object should have at least one row and ",
         "one column", call. = FALSE)
  }
}