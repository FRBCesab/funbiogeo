is_ggridges_installed <- function() {
  isTRUE(requireNamespace("ggridges"))
}

#' @importFrom stats weighted.mean
#' @noRd
weighted_mean <- function(x, w, ..., na.rm = FALSE){
  if(na.rm){
    keep <- !is.na(x) & !is.na(w)
    w <- w[keep]
    x <- x[keep]
  }
  weighted.mean(x, w, ..., na.rm = FALSE)
}

#' Function to split species traits data.frame into a list based on provided
#' species categories
#' 
#' @noRd
split_species_categories <- function(
    species_traits, species_categories = NULL
  ) {
  
  species_traits_categories <- list(species_traits)
  
  if (!is.null(species_categories)) {
    
    species_traits_categories <- merge(
      species_traits, species_categories, by = colnames(species_categories)[1]
    )
    
    species_traits_categories <- split(
      species_traits_categories[
        , -ncol(species_traits_categories), drop = FALSE
      ],
      species_traits_categories[, ncol(species_traits_categories)]
    )
    
  }
  
  return(species_traits_categories)
  
}