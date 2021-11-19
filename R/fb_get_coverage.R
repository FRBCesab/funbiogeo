#' Compute traits coverage
#' 
#' @description
#' Compute traits coverage for all sites, i.e., the percentage of total 
#' abundance/presence of species that have traits data compared to total
#' species.
#' This function assumes that all species provided in the traits dataset have
#' all their traits specified.
#' 
#' @param site_species a `matrix` or `data.frame` with sites in rows and 
#' species in columns. Rows names (sites) and columns names (species) must be 
#' defined.
#' 
#' @param species_traits a `matrix` or `data.frame` with species in rows and 
#' traits in columns. Rows names (species) and columns names (traits) must be 
#' defined.
#'
#' @return A `data.frame` with `n` rows (where `n` is the number of sites) and 
#' two columns: `site`, the site label, and `trait_coverage`, the percent of
#' total abundance/presence of species that have traits data.
#' 
#' @export
#'
#' @examples
#' library("funbiogeo")
#' 
#' data("site_species")
#' data("species_traits")
#' 
#' cover <- fb_get_coverage(site_species, species_traits)

fb_get_coverage <- function(site_species, species_traits) {
  
  
  ## Check inputs ----
  
  if (missing(site_species)) {
    stop("Argument 'site_species' (site x species matrix) is required")
  }
  
  if (missing(species_traits)) {
    stop("Argument 'species_traits' (species x traits matrix) is required")
  }
  
  check_site_species(site_species)
  
  check_species_traits(species_traits)
  
  
  # Get species in common between both matrices ----
  
  species <- list_common_species(colnames(site_species), 
                                 rownames(species_traits))
  
  
  # Subset data with common species ----
  
  species_traits <- species_traits[species, , drop = FALSE]
  
  
  # Count all species (presence/abundance) per site ----
  
  site_total_abundance <- rowSums(site_species, na.rm = TRUE)
  
  
  # Count species with traits per site -----
  
  site_cover_abundance <- rowSums(site_species[ , species, drop = FALSE], 
                                  na.rm = TRUE)
  
  # Compute trait coverage ----
  
  trait_coverage <- site_cover_abundance / site_total_abundance
  
  
  # Transforming into tidy format ----
  
  data.frame("site"           = names(trait_coverage), 
             "trait_coverage" = trait_coverage,
             stringsAsFactors = FALSE,
             row.names = NULL)
}
