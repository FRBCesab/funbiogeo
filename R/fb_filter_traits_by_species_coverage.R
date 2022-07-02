#' Filter traits with a given species coverage threshold
#' 
#' @description
#' Selects traits (columns) for which the percentage of species without 
#' `NA` (missing trait values) is higher than a threshold.
#' 
#' @param threshold_species_proportion `numeric(1)` \[default = `NULL`\]\cr{}
#'   between 0 and 1. The percentage of species coverage threshold.
#' 
#' @inheritParams fb_get_trait_coverage_by_site
#'
#' @return A subset of `species_traits` with traits for the specified
#'   proportion of species.
#' 
#' @export
#'
#' @examples
#' library("funbiogeo")
#' 
#' data("species_traits")
#' 
#' # Filter traits that have at least 60% non-missing values
#' new_species_traits <- fb_filter_traits_by_species_coverage(
#'   species_traits,
#'   threshold_species_proportion = 0.6
#' )
#' 
#' # There are now only 2 traits
#' ncol(new_species_traits)

fb_filter_traits_by_species_coverage <- function(
    species_traits, 
    threshold_species_proportion = 0
) {
  
  # Check inputs
  check_species_traits(species_traits)
  check_threshold_proportion(threshold_species_proportion, "species")
  
  
  # Get species coverage for each trait ----
  
  species_coverage <- fb_count_species_by_trait(species_traits)
  
  
  # Filter traits by species coverage ----
  
  selected_traits <- species_coverage[
    which(species_coverage[["coverage"]] >= threshold_species_proportion), 
    "trait"]
  
  returned_traits <- species_traits[
    , c(colnames(species_traits)[1], selected_traits), drop = FALSE
  ]
  
  if (identical(selected_traits, character(0))) {
    message("No trait has the specified species coverage threshold")
    
    # Return empty data.frame when no species are retained
    returned_traits <- species_traits[NULL, ]
  }
  
  return(returned_traits)
}
