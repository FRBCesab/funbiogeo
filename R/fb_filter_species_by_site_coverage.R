#' Filter species with a given sites coverage threshold
#' 
#' @description
#' Selects species (columns) for which the percentage of sites where the 
#' species is present (distribution value higher than 0 and non-NA) is higher 
#' than a threshold.
#' 
#' @param threshold_sites_proportion a numeric of length 1 between 0 and 1.
#'   The percentage of sites coverage threshold.
#' 
#' @inheritParams fb_get_trait_coverage_by_site
#'
#' @return A subset of `site_species` with species with a prevalence higher 
#' than `threshold_sites_proportion`.
#' 
#' @export
#'
#' @examples
#' library("funbiogeo")
#' 
#' # Filter species present in at least 10% of the sites
#' new_site_species <- fb_filter_species_by_site_coverage(
#'   woodiv_site_species,
#'   threshold_sites_proportion = 0.1
#' )
#' 
#' new_site_species[1:3, 1:4]
#' 
#' # There are now only 84 species (instead of 149)
#' ncol(new_site_species)

fb_filter_species_by_site_coverage <- function(
    site_species, threshold_sites_proportion = 0
) { 
  
  ## Check inputs ----
  
  check_site_species(site_species)
  check_threshold_proportion(threshold_sites_proportion, "site")

  
  # Get sites coverage for each species ----
  
  sites_coverage <- fb_count_sites_by_species(site_species)
  
  
  # Filter species by site coverage ----
  
  selected_species <- sites_coverage[
    which(sites_coverage[["coverage"]] >= threshold_sites_proportion), "species"
  ]
  
  if (length(selected_species) == 0) {
  
    message("All species are absent from the study area")
    
    returned_sites <- site_species[NULL, ]
  
  } else {
    
    returned_sites <- site_species[ , c("site", selected_species), drop = FALSE]
    
  }
  
  return(returned_sites)
}
