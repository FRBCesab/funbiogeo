#' Compute trait coverage
#' 
#' Compute trait coverage for all sites, i.e., percent of total
#' abundance/presence of species that have trait data compared to total species.
#' This function assumes consider that all species provided in the trait dataset
#' have all their traits specified.
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
  species = list_common_species(colnames(site_sp), rownames(traits))
  
  # Subset data with common species
  traits  = traits[species,, drop = FALSE]
  
  # Count all species per site
  site_total_abundance = rowSums(site_sp, na.rm = TRUE)
  
  # Count species with traits per site
  site_cover_abundance = rowSums(site_sp[, species, drop = FALSE], na.rm = TRUE)
  
  # Compute trait coverage
  trait_coverage = site_cover_abundance/site_total_abundance
  
  # Transforming into tidy format
  trait_coverage = data.frame(
    site = names(trait_coverage),
    trait_coverage = trait_coverage,
  stringsAsFactors = FALSE
  )
  
  return(trait_coverage)
}