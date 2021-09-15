#' Filter sites with a given trait coverage threshold
#' 
#' @description
#' ...
#' 
#' @param coverage_threshold a numeric of length 1 between 0 and 1. The 
#' percentage trait coverage threshold
#' 
#' @inheritParams fb_get_coverage
#'
#' @return A subset of `sites_species` with sites covered by X% of 
#' abundance/coverage considering all provided traits.
#' 
#' @export
#'
#' @examples
#' ## Add an example ----

fb_filter_coverage <- function(sites_species, species_traits, 
                               coverage_threshold = 1) {
  
  
  ## Check inputs ----
  
  if (missing(sites_species)) {
    stop("Argument 'sites_species' (sites x species matrix) is required")
  }
  
  if (missing(species_traits)) {
    stop("Argument 'species_traits' (species x traits matrix) is required")
  }
  
  check_sites_species(sites_species)
  
  check_species_traits(species_traits)
  
  if (!is.numeric(coverage_threshold) | coverage_threshold > 1 |
      coverage_threshold < 0) {
    stop("Coverage threshold should be a numeric value >= 0 and <= 1",
         call. = FALSE)
  }
  
  
  # Get trait coverage for site and traits ----

  trait_coverage <- fb_get_coverage(sites_species, species_traits)
  
  
  # Filter sites by coverage ----
  
  selected_sites <- trait_coverage[
    which(trait_coverage$"trait_coverage" >= coverage_threshold), "site"]
  
  
  if (identical(selected_sites, character(0))) {
    message("No sites has the specified trait coverage threshold")
  }
    
  sites_species[selected_sites, , drop = FALSE]
}
