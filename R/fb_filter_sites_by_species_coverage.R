#' Filter sites with a given species coverage threshold
#' 
#' @description
#' Selects sites (rows) for which the percentage of present species 
#' (distribution value higher than 0 and non-NA) is higher than a threshold.
#' 
#' @param threshold_species_proportion a numeric of length 1 between 0 and 1.
#'   The percentage of species coverage threshold.
#' 
#' @inheritParams fb_get_trait_coverage_by_site
#'
#' @return A subset of `site_species` with sites covered by X% of species.
#' 
#' @export
#'
#' @examples
#' library("funbiogeo")
#' 
#' data("site_species")
#' 
#' # Get sites with more than 40% of the species
#' new_site_species <- fb_filter_sites_by_species_coverage(
#'   site_species,
#'   threshold_species_proportion = 0.4
#' )
#' # There are now only 148 sites
#' dim(new_site_species)
#' new_site_species[1:3, 1:4]

fb_filter_sites_by_species_coverage <- function(
    site_species, threshold_species_proportion = 0
) { 
  
  ## Check inputs ----
  
  check_site_species(site_species)
  check_threshold_proportion(threshold_species_proportion, "species")
  
  
  # Get species coverage for each site ----
  
  sites_coverage <- fb_count_species_by_site(site_species)
  
  
  # Filter species by site coverage ----
  
  selected_sites <- sites_coverage[
    which(sites_coverage[["coverage"]] >= threshold_species_proportion), 
    "site"]
  
  if (length(selected_sites) == 0) {
    
    message("All sites are empty (no species)")
    returned_sites <- site_species[NULL, ]
    
  } else {
    
    returned_sites <- site_species[
      site_species[ , 1] %in% selected_sites, , drop = FALSE
    ]
  }
  
  return(returned_sites)
}
