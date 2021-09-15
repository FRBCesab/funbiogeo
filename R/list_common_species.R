#' Get the intersection of two vectors of species names
#' 
#' @param first_list a vector of species names
#' @param second_list a vector of species names
#' 
#' @return A vector (common species names).
#' 
#' @noRd

list_common_species <- function(first_list, second_list) {
  
  if (any(is.na(first_list)) | any(is.na(second_list))) {
    stop("Species names cannot contain NA", call. = FALSE)
  }
  
  common_species <- intersect(first_list, second_list)
  
  if (length(common_species) == 0) {
    stop("No species found in common between inputs", call. = FALSE)
  }
  
  common_species
}
