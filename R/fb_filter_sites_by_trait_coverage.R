#' Filter sites with a given trait coverage threshold
#' 
#' @description
#' ...
#' 
#' @param threshold_traits_proportion a numeric of length 1 between 0 and 1. The 
#' percentage trait coverage threshold
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
#' cover <- fb_filter_sites_by_trait_coverage(site_species, species_traits)

fb_filter_sites_by_trait_coverage <- function(
    site_species, species_traits, threshold_traits_proportion = 1
) {
  
  
  ## Check inputs ----
  
  if (missing(site_species)) {
    stop("Argument 'site_species' (site x species matrix) is required")
  }
  
  if (missing(species_traits)) {
    stop("Argument 'species_traits' (species x traits matrix) is required")
  }
  
  check_site_species(site_species)
  
  check_species_traits(species_traits)
  
  if (!is.numeric(threshold_traits_proportion) |
      threshold_traits_proportion > 1 |
      threshold_traits_proportion < 0) {
    stop("Coverage threshold should be a numeric value >= 0 and <= 1",
         call. = FALSE)
  }
  
  
  # Get trait coverage for site and traits ----

  trait_coverage <- fb_get_trait_coverage_by_site(site_species, species_traits)
  
  
  # Filter sites by coverage ----
  
  selected_sites <- trait_coverage[
    which(trait_coverage[["trait_coverage"]] >= threshold_traits_proportion),
    "site"]
  
  
  if (identical(selected_sites, character(0))) {
    message("No sites has the specified trait coverage threshold")
  }
    
  site_species[site_species[["site"]] %in% selected_sites, , drop = FALSE]
}