#' Compute Trait Coverage per Site for Each Trait
#' 
#' Compute trait coverage for all sites, i.e., the percentage of total 
#' abundance/presence of species that have traits data compared to total
#' species. This function assumes that all species provided in the traits dataset have
#' all their traits specified (meaning that all species have either known or
#' `NA` values reported as their traits).
#' The coverage of each trait separately is returned as well as all traits taken
#' together if wanted.
#' 
#' @inheritParams fb_get_trait_coverage_by_site
#' 
#' @param all_traits a logical (default = `TRUE`) which tell if the coverage
#' considering all provided traits should be provided in an additional column
#' `all_traits`
#'
#' @return a data.frame with a column with sites and one column per provided
#'   trait giving its coverage (percent species per site, weighted by abundance
#'   that have trait data), and, when argument `all_traits = TRUE`,
#'   an additional column named `all_traits` considering the coverage of all
#'   traits taken together.
#'   
#' @export
#'
#' @examples
#' site_trait_cov <- fb_get_all_trait_coverages_by_site(
#'   woodiv_site_species, woodiv_traits
#' )
#' 
#' head(site_trait_cov)
fb_get_all_trait_coverages_by_site <- function(
    site_species, species_traits, all_traits = TRUE
) {
  
  # Checks
  check_site_species(site_species)
  check_species_traits(species_traits)
  
  if (!is.logical(all_traits)) {
    stop("Argument 'all_traits' should be TRUE or FALSE")
  }
  
  full_coverage <- data.frame(site = rownames(site_species))
  
  # Computing Trait Coverage per Site for all Traits
  if (all_traits) {
    full_coverage <- fb_get_trait_coverage_by_site(site_species, species_traits)
    colnames(full_coverage)[2] <- "all_traits"
  }
  
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