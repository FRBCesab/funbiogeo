#' Compute Trait Coverage For Each Site Weighted by Abundance
#' 
#' Compute trait coverage for all sites, i.e., the percentage of total 
#' abundance/presence of species that have traits data compared to total
#' species.
#' This function assumes that all species provided in the traits dataset have
#' all their traits specified (meaning that all species have either known or
#' `NA` values reported as their traits).
#'  **NB**: this function returns trait coverage using all traits
#' provided in the input `species_traits` data.frame.
#' 
#' @param site_species a `data.frame` with sites in rows and species in columns.
#' **NOTE**: the first column should be named **`"site"`** and indicate site
#' names. The other columns should be named according to species names.
#' 
#' @param species_traits a `data.frame` with species in rows and
#' traits as columns. **NOTE**: The first column should be named **`"species"`**
#' and contain species names. The other columns should be named according
#' to trait names.
#'
#' @return A `data.frame` with `n` rows (where `n` is the number of sites) and 
#' two columns: `site`, the site label, and `trait_coverage`, the percent of
#' total abundance/presence of species that have traits data.
#' 
#' @export
#'
#' @examples
#' site_trait_cov <- fb_get_trait_coverage_by_site(
#'     woodiv_site_species, woodiv_traits
#' )
#' 
#' head(site_trait_cov)
fb_get_trait_coverage_by_site <- function(site_species, species_traits) {
  
  
  # Check inputs
  check_site_species(site_species)
  check_species_traits(species_traits)
  
  # Get species in common between both matrices
  species <- list_common_species(
    colnames(site_species), species_traits[["species"]]
  )
  
  # Take species with NA into account
  species_with_na <- species_traits[["species"]][
    !stats::complete.cases(species_traits)
  ]
  species_with_na <- intersect(species_with_na, species)
  
  # Subset data with common species
  species_traits <- species_traits[
    species_traits[["species"]] %in% species,, drop = FALSE
  ]
  
  
  # Count all species (presence/abundance) per site
  site_total_abundance <- rowSums(
    site_species[ , -1, drop = FALSE], na.rm = TRUE
  )
  
  
  # Count species with traits per site
  if (length(species_with_na) != 0) site_species[, species_with_na] <- 0
  site_cover_abundance <- rowSums(
    site_species[, species, drop = FALSE], na.rm = TRUE
  )
  
  
  # Compute trait coverage
  trait_coverage <- site_cover_abundance / site_total_abundance
  
  
  # Transforming into tidy format
  data.frame(site             = site_species[["site"]], 
             trait_coverage   = trait_coverage,
             stringsAsFactors = FALSE)
}
