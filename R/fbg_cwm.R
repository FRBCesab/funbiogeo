#' Compute CWM
#'
#' @param site_sp 
#' @param traits 
#'
#' @return
#' @export
#'
#' @examples
fbg_cwm = function(site_sp, traits) {
  # Check input format of site-species matrix
  check_site_sp(site_sp)
  
  # Check input format of species-traits matrix
  check_traits(traits)
  
  # Get species in common between both matrices
  species = species_in_common(site_sp, traits)
  
  # Select quantitative traits for CWM
  quanti_traits = traits
  
  # Compute CWM
  cwm = site_sp[, species] %*% quanti_traits[species,]
  
  # Reformat CWM in good way (tidy format)
}