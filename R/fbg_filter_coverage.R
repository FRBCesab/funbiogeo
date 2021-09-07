#' Filter sites with a given trait threshold
#'
#' @param site_sp 
#' @param traits 
#' @param trait_threshold 
#'
#' @return sites covered by X% of abundance/coverage consider all provided traits
#' @export
#'
#' @examples
fbg_filter_coverage = function(site_sp, traits, trait_threshold = 1) {
  # Check input format of site-species matrix
  check_site_sp(site_sp)
  
  # Check input format of species-traits matrix
  check_traits(traits)
  
  # Get species in common between both matrices
  species = species_in_common(site_sp, traits)
  
  # Get trait coverage for site and traits
  trait_coverage = fbg_get_coverage()
  
  # Filter site by coverage
  selected_sites = site_sp[0 <= trait_threshold]
  
  return(selected_sites)
}