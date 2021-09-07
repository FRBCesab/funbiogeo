#' Compute trait coverage
#'
#' @param site_sp 
#' @param traits 
#'
#' @return Return for each site the traits coverage for all provided species and
#' traits
#' @export
#'
#' @examples
fbg_get_coverage = function(site_sp, traits) {
  # Check input format of site-species matrix
  check_site_sp(site_sp)
  
  # Check input format of species-traits matrix
  check_traits(traits)
  
  # Get species in common between both matrices
  species = list_common_species(site_sp, traits)
  
  return(site_sp)
}