#' Filter sites with a given trait coverage threshold
#' 
#' @description
#' ...
#' 
#' @param threshold_traits_proportion a numeric of length 1 between 0 and 1.
#'   The percentage trait coverage threshold
#' 
#' @inheritParams fb_get_trait_coverage_by_site
#'
#' @return A subset of `site_species` with sites covered by X% of 
#' abundance/coverage considering all provided traits.
#' 
#' @export
#'
#' @examples
#' library("funbiogeo")
#' 
#' data("site_species")
#' data("species_traits")
#' 
#' # Filter all the sites where all species have known traits
#' new_site_species <- fb_filter_sites_by_trait_coverage(
#'   site_species, species_traits
#' )
#' 
#' # There is only one such site
#' nrow(new_site_species)
#' 
#' # Filter sites where at least 80% of species have known traits
#' new_site_species_2 <- fb_filter_sites_by_trait_coverage(
#'   site_species, species_traits, threshold_traits_proportion = 0.8
#' )
#' 
#' # There are now four sites
#' nrow(new_site_species_2)

fb_filter_sites_by_trait_coverage <- function(
    site_species, species_traits, threshold_traits_proportion = 1
) {
  
  
  # Check inputs
  check_site_species(site_species)
  check_species_traits(species_traits)
  check_threshold_proportion(threshold_traits_proportion, "trait")
  
  
  # Get trait coverage for site and traits ----

  trait_coverage <- fb_get_trait_coverage_by_site(site_species, species_traits)
  
  
  # Filter sites by coverage ----
  
  selected_sites <- trait_coverage[
    which(trait_coverage[["trait_coverage"]] >= threshold_traits_proportion),
    "site"]
  
  if (length(selected_sites) == 0) {
    
    message("No sites has the specified trait coverage threshold")
    
    returned_sites <- site_species[NULL,]
  
  } else {
    
    returned_sites <- site_species[
      site_species[["site"]] %in% selected_sites, , drop = FALSE
    ]
    
  }
  
  return(returned_sites)
}
