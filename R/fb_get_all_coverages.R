#' Compute Trait Coverage per Site for All Traits
#'
#' @inheritParams fb_get_trait_coverage_by_site
#'
#' @return a data.frame with a column with sites and one column per provided
#'   trait giving its coverage (percent species per site, weighted by abundance
#'   that have trait data)
#' @export
#'
#' @examples
#' fb_get_all_coverages(site_species, species_traits)
fb_get_all_coverages <- function(site_species, species_traits) {
  
  # Checks
  check_site_species(site_species)
  check_species_traits(species_traits)
  
  # Computing Trait Coverage per Site for all Traits
  full_coverage <- fb_get_trait_coverage_by_site(site_species, species_traits)
  colnames(full_coverage)[2] <- "all_traits"
  
  # Trait by Trait
  trait_coverage <- lapply(colnames(species_traits)[-1], function(x) {
    
    single_trait_coverage <- fb_get_trait_coverage_by_site(
      site_species, species_traits[, c("species", x)]
    )
    
    colnames(single_trait_coverage)[2] <- x
    
    return(single_trait_coverage)
  })
  
  # Combine Trait by Trait Coverages
  trait_coverage <- Reduce(
    function(...) merge(..., by = "site", all.x = TRUE), trait_coverage
  )
  
  # Merge All Coverages (all traits and trait by trait)
  merge(full_coverage, trait_coverage, by = "site")
}