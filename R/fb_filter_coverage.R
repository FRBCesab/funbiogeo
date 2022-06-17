#' Filter sites with a given trait coverage threshold
#' 
#' @description
#' ...
#' 
#' @param coverage_threshold a numeric of length 1 between 0 and 1. The 
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
#' cover <- fb_filter_coverage(site_species, species_traits)

fb_filter_coverage <- function(site_species, species_traits, 
                               coverage_threshold = 1) {
  
  
  ## Check inputs ----
  
  if (missing(site_species)) {
    stop("Argument 'site_species' (site x species matrix) is required")
  }
  
  if (missing(species_traits)) {
    stop("Argument 'species_traits' (species x traits matrix) is required")
  }
  
  check_site_species(site_species)
  
  check_species_traits(species_traits)
  
  if (!is.numeric(coverage_threshold) | coverage_threshold > 1 |
      coverage_threshold < 0) {
    stop("Coverage threshold should be a numeric value >= 0 and <= 1",
         call. = FALSE)
  }
  
  
  # Get trait coverage for site and traits ----

  trait_coverage <- fb_get_trait_coverage_by_site(site_species, species_traits)
  
  
  # Filter sites by coverage ----
  
  selected_sites <- trait_coverage[
    which(trait_coverage[["trait_coverage"]] >= coverage_threshold),
    "site"]
  
  
  if (identical(selected_sites, character(0))) {
    message("No sites has the specified trait coverage threshold")
  }
    
  site_species[site_species[["site"]] %in% selected_sites, , drop = FALSE]
}
